{-# LANGUAGE ConstraintKinds, TypeFamilies, GADTs, UndecidableInstances, GeneralizedNewtypeDeriving #-}
-- |
-- Module: Hue.Internal 
-- Copyright: (c) 2017 Thomas Smith
-- License: BSD3
-- Maintainer: Thomas Smith <tnsmith@live.nl>
-- Stability: experimental
--
-- Base functions and types to perform requests against the Hue bridge.
module Hue.Internal where

import Network.HTTP.Simple hiding (JSONParseException, Proxy, Request)
import qualified Network.HTTP.Simple as HTTP
import Data.Aeson
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

import Data.String
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS

import Data.Text (Text, append)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)

import Data.Singletons
import Data.Singletons.Prelude.Bool

import Control.Applicative
import Control.Exception hiding (TypeError)
import Control.Monad.Except
import Control.Monad.Reader

import Hue.Internal.Request

-- | The Hue Monad.
newtype Hue a = Hue {
  unHue :: ReaderT HueConfig (ExceptT HueApiException IO) a
  } deriving (
    Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadReader HueConfig
  , MonadError HueApiException)

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

-- | Send a request to the bridge.
-- 
-- This function gives you the opportunity to talk directly to any bridge endpoint,
-- and provide or get data in your own types.
-- In some cases however, calling the API directly can be cumbersome or too low level
-- and it might be easier to use one of the pre-defined 'Hue' actions.
-- 
-- The result of calling this function depends on the type  of the supplied 'Request'. 
-- There are two possible return types:
-- 
--  * If the body type is @()@, request immediately results in a Hue action:
-- 
--    >>> let someRequest = ... :: Request () SomeResult
--    >>> :t request someRequest
--    request someRequest :: Hue SomeResult
-- 
--  * Otherwise, 'request' will instead return a function asking for the body:
-- 
--    >>> let someRequest = ... :: Request Int SomeResult
--    >>> :t request someRequest
--    request someRequest :: Int -> Hue SomeResult
-- 
-- This makes 'request' type safe with respect to the request body and result.
-- See "Hue.Request" for more details.
request :: forall body resp. 
        ( ToJSON body
        , FromJSON resp
        , SingI (IsUnit body)
        , SingI (IsUnit resp)
        )
        => Request body resp -- ^ The API request to send the request to
        -> HueFn body resp
request r = case (sing :: Sing (IsUnit body), sing :: Sing (IsUnit resp)) of
    -- Both the body and the result are (), so parse the response as Unit and discard it
    (STrue, STrue) -> void $ mkRequest >>= (performRequest :: HTTP.Request -> Hue Unit)
    -- Only the body is (), so parse the response and return it 
    (STrue, SFalse) -> mkRequest >>= performRequest
    -- Only the response type is (), create a function that asks for the body and discard the result
    (SFalse, STrue) -> \b -> void $ setRequestBodyJSON b <$> mkRequest >>= (performRequest :: HTTP.Request -> Hue Unit)
    -- Both are not (), create a function that asks for the body and return the result directly
    (SFalse, SFalse) -> \b -> setRequestBodyJSON b <$> mkRequest >>= performRequest
  where 
    mkRequest :: Hue HTTP.Request
    mkRequest = do
      path <- requestPath r . unCredentials . configCredentials <$> ask
      ip <- ipAddress . configIP <$> ask
      pure 
        $ setRequestPath path
        $ setRequestMethod (requestMethod r)
        $ setRequestHost ip
        $ defaultRequest

    performRequest :: FromJSON a
                   => HTTP.Request
                   -> Hue a
    performRequest req = do
      response <- liftIO $ httpLBS req
      let responseText = getResponseBody response
      case eitherDecode responseText of 
        Left err -> throwParseError err (LBS.toStrict responseText)
        Right (HueErrorResponse err) -> throwError $ HueApiException err
        Right (HueResponse a) -> pure a

    throwParseError :: String 
                    -> ByteString 
                    -> Hue a
    throwParseError err responseText = 
      liftIO $ throwIO $ Hue.Internal.JSONParseException $
          "Error parsing bridge response: \n\t"
          `append` Text.pack err
          `append` "\nAttempted to parse response:\n\t"
          `append` decodeUtf8 responseText

-- | Determine the return type of the 'request' function.
type HueFn b r = HueRequestType (IsUnit b) b (HueResponseType (IsUnit r) r)

-- | If 'request' is called with @()@ as response, then the result is @Hue ()@
type family HueResponseType isRespUnit r where
  HueResponseType 'True r = Hue ()
  HueResponseType 'False r = Hue r

-- | If 'request' is called with @()@ as body, then the result is @body -> Hue a@
type family HueRequestType isBodyUnit body r where
  HueRequestType 'True b r = r
  HueRequestType 'False b r = b -> r

-- | Determine at the type level whether a type is @()@ or not.
type family IsUnit a where
  IsUnit () = 'True
  IsUnit a = 'False

-- ----------------------------------------------------------------
-- BASE TYPES
--   Authentication
-- ----------------------------------------------------------------

-- | Create a HueConfig with an explicit bridge IP address. 
-- 
-- Credentials will be set to a default that can only be used against unauthenticated resources.
-- 
-- To get a HueConfig with credentials, can use the functions from "Hue.Auth".
configWithIP :: BridgeIP -> HueConfig
configWithIP ip = HueConfig ip (HueCredentials "-")

-- | Configuration necessary to query the bridge.
-- 
-- Contains the bridge IP address and (optionally) credentials needed to turn lights on and off.
data HueConfig = HueConfig {
  configIP :: BridgeIP
, configCredentials :: HueCredentials
} deriving (Show)

-- | The IP address of the bridge to send requests to.
newtype BridgeIP = BridgeIP {
  ipAddress :: ByteString
} deriving (Show)

-- | BridgeIP can be created from String literals/
instance IsString BridgeIP where
  fromString = BridgeIP . fromString


-- | An authentication token needed for most endpoints.
newtype HueCredentials = HueCredentials {
  unCredentials :: Text
} deriving (Show)

instance FromJSON HueCredentials where
  parseJSON v = do
    [HueSuccess c] <- parseJSON v
    withObject "HueCredentials object" (\o -> HueCredentials <$> o .: "username") c

-- | Credentials can be used as part of an API path.
instance ToPathSegment HueCredentials where
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

-- | Representation for all responses from the bridge.
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

-- | Wrapper for errors returned by the bridge.
data HueApiException = HueApiException [HueError] deriving (Show)

instance Exception HueApiException

-- | When the bridge response cannot be parsed...
newtype JSONParseException = JSONParseException Text deriving (Show)

instance Exception JSONParseException