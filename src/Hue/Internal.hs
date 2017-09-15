{-# LANGUAGE ConstraintKinds, TypeFamilies, GADTs, UndecidableInstances, GeneralizedNewtypeDeriving #-}
module Hue.Internal where

import GHC.TypeLits as TypeLits

import Network.HTTP.Simple hiding (JSONParseException, Proxy)
import Network.HTTP.Types.Method
import Data.Aeson
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS

import Data.Text (Text, append)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)

import Data.Kind
import Data.Proxy
import Data.Singletons
import Data.Singletons.Prelude.Bool

import Control.Applicative
import Control.Exception hiding (TypeError)
import Control.Monad.Except
import Control.Monad.Reader

import Hue.Internal.Endpoint

newtype Hue a = Hue {
  unHue :: ReaderT HueConfig (ExceptT HueException IO) a
  } deriving (
    Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadReader HueConfig
  , MonadError HueException)

-- | Evaluate a hue action with a specific config
evalHue :: MonadIO m => HueConfig -> Hue a -> m a
evalHue config h = do
  e <- unwrapHue config h
  case e of 
    Left (JSONParseException err) -> error $ Text.unpack err 
    Right (Left err) -> error $ show err
    Right (Right a) -> pure a

  where
    unwrapHue c = liftIO . try . runExceptT . flip runReaderT c . unHue

-- | Send a request to the bridge on the given Endpoint.
-- 
-- If the Endpoint requires a non-unit body,
-- then you can pass the body as second argument to this request function.
request :: forall (method :: StdMethod) body resp. 
        ( ToJSON body
        , FromJSON resp
        , KnownMethodType method
        , SingI (IsUnit body)
        , SingI (IsUnit resp)
        , MethodAllowsBody method body
        )
        => Endpoint method body resp  -- ^ The API endpoint to send the request to
        -> HueFn body resp            -- ^ A function if body is not @()@
request r = case (sing :: Sing (IsUnit body), sing :: Sing (IsUnit resp)) of
    -- Both the body and the result are (), so parse the response as Unit and discard it
    (STrue, STrue) -> void $ mkRequest >>= (performRequest :: Request -> Hue Unit)
    -- Only the body is (), so parse the response and return it 
    (STrue, SFalse) -> mkRequest >>= performRequest
    -- Only the response type is (), create a function that asks for the body and discard the result
    (SFalse, STrue) -> \b -> void $ setRequestBodyJSON b <$> mkRequest >>= (performRequest :: Request -> Hue Unit)
    -- Both are not (), create a function that asks for the body and return the result directly
    (SFalse, SFalse) -> \b -> setRequestBodyJSON b <$> mkRequest >>= performRequest
  where 
    mkRequest :: Hue Request
    mkRequest = do
      path <- endpointPath r . unCredentials . configCredentials <$> ask
      ip <- ipAddress . configIP <$> ask
      pure 
        $ setRequestPath path
        $ setRequestMethod (getMethodType (Proxy :: Proxy method))
        $ setRequestHost ip
        $ defaultRequest

    performRequest :: FromJSON a
                   => Request
                   -> Hue a
    performRequest req = do
      response <- liftIO $ httpLBS req
      let responseText = getResponseBody response
      liftIO $ print responseText
      case eitherDecode responseText of 
        Left err -> throwParseError err (LBS.toStrict responseText)
        Right (HueErrorResponse err) -> throwError $ HueApiException err
        Right (HueResponse a) -> pure a

    throwParseError :: String 
                    -> ByteString 
                    -> Hue a
    throwParseError err responseText = 
      liftIO $ throwIO $ Hue.Internal.JSONParseException $
          "Error parsing bridge response: \n\t\
          \" `append` Text.pack err `append` "\n\
          \Attempted to parse response:\n\t"
          `append` decodeUtf8 responseText

-- | Type level function to determine whether a HTTP method allows a body.
type family MethodAllowsBody (method :: StdMethod) body :: Constraint where
  MethodAllowsBody 'GET () = ()
  MethodAllowsBody 'GET b = TypeError ('Text "HTTP method GET does not allow attaching a request body.")
  MethodAllowsBody 'DELETE () = ()
  MethodAllowsBody 'DELETE b = TypeError ('Text "HTTP method DELETE does not allow attaching a request body.")
  MethodAllowsBody m b = ()

type HueFn b r = HueRequestType (IsUnit b) b (HueResponseType (IsUnit r) r)

type family HueResponseType isRespUnit r where
  HueResponseType 'True r = Hue ()
  HueResponseType 'False r = Hue r

type family HueRequestType isBodyUnit body r where
  HueRequestType 'True b r = r
  HueRequestType 'False b r = b -> r

type family IsUnit a where
  IsUnit () = 'True
  IsUnit a = 'False      

-- | Class to retrieve the request method as a runtime ByteString
-- from the method type
class KnownMethodType (a :: StdMethod) where
  getMethodType :: Proxy a -> ByteString

instance KnownMethodType 'GET where
  getMethodType _ = "GET"

instance KnownMethodType 'PUT where
  getMethodType _ = "PUT"

instance KnownMethodType 'POST where
  getMethodType _ = "POST"

instance KnownMethodType 'DELETE where
  getMethodType _ = "DELETE"

-- ----------------------------------------------------------------
-- BASE TYPES
--   Authentication
-- ----------------------------------------------------------------

-- | A sample config with no credentials and the ip set to @192.168.1.100@
defaultConfig :: HueConfig
defaultConfig = HueConfig 
  (BridgeIP "192.168.1.100")
  (HueCredentials "-")

data HueConfig = HueConfig {
  configIP :: BridgeIP
, configCredentials :: HueCredentials
} deriving (Show)

-- The IP address of the bridge to send requests to.
newtype BridgeIP = BridgeIP {
  ipAddress :: ByteString
} deriving (Show)

-- An authentication token needed for most endpoints.
newtype HueCredentials = HueCredentials {
  unCredentials :: Text
} deriving (Show)

instance FromJSON HueCredentials where
  parseJSON v = do
    [HueSuccess c] <- parseJSON v
    withObject "HueCredentials object" (\o -> HueCredentials <$> o .: "username") c

instance ToEndpointSegment HueCredentials where
  toSegment (HueCredentials creds) = TextSegment creds

-- | Device type used when registering a new application with the bridge.
newtype HueDeviceType = HueDeviceType Text deriving (Show)

instance ToJSON HueDeviceType where
  toJSON (HueDeviceType devType) = toJSON $ Map.fromList [("devicetype" :: Text, devType)]  


-- ----------------------------------------------------------------
-- BASE TYPES
--   Hue API response representation
-- ----------------------------------------------------------------

-- | Specific unit type to make Aeson parse () as an object.
data Unit = Unit deriving (Show)

instance FromJSON Unit where 
  parseJSON v = do
    _ :: [HueSuccess Value] <- parseJSON v
    pure Unit

-- Representation for all responses from the bridge
data HueResponse a = 
    HueErrorResponse [HueError]
  | HueResponse a 
  deriving (Show)

instance (FromJSON a) => FromJSON (HueResponse a) where
  parseJSON v = (HueErrorResponse <$> parseHueErrors v) <|> (HueResponse <$> parseJSON v)
    where
      parseHueErrors = withArray "array" $ \arr -> do
        errors <- catMaybes <$> traverse (optional . parseJSON) (V.toList arr)
        case length errors of
          0 -> mzero
          _ -> pure errors

-- | Some bridge endpoints respond with a list of HueSuccess objects.
-- E.g. most endpoints that set/update some data.
data HueSuccess a = HueSuccess a deriving (Show)

instance (FromJSON a) => FromJSON (HueSuccess a) where
  parseJSON = withObject "HueSuccess object" $ \o -> do
    s <- o .: "success"
    HueSuccess <$> parseJSON s

-- | Errors returned by the bridge.
data HueError = HueError Int Text deriving (Show)

instance FromJSON HueError where 
  parseJSON = withObject "HueError object" $ \o -> do
    e <- o .: "error"
    HueError <$> e .: "type" <*> e .: "description"

data HueException = HueApiException [HueError] deriving (Show)

instance Exception HueException

newtype JSONParseException = JSONParseException Text deriving (Show)

instance Exception JSONParseException