{-# LANGUAGE ConstraintKinds, TypeFamilies, GADTs, UndecidableInstances, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
-- |
-- Module: Hue.Internal 
-- Copyright: (c) 2017 Thomas Smith
-- License: BSD3
-- Maintainer: Thomas Smith <tnsmith@live.nl>
-- Stability: experimental
--
-- Base functions and types to perform requests against the Hue bridge.
-- 
-- This is an internal module.
-- 
-- Please use "Hue" instead. 
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

-- | Evaluate a hue action with a specific config.
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
-- This function gives you the opportunity to talk directly to any bridge endpoint
-- and provide or get data in your own types.
-- In some cases however, calling the API directly can be cumbersome or too low level
-- and it might be easier to use one of the pre-defined 'Hue' actions.
-- 
-- Pre-defined requests can be found in the module that they functionally belong to, for 
-- example see 'lights' in "Hue.Light".
request :: Request a -> Hue a
request r = case requestResult r of
  IgnoreResult -> do
    _ :: [HueSuccess Value] <- mkRequest >>= performRequest
    pure ()
  ParseResult -> mkRequest >>= performRequest

  where

    mkRequest :: Hue HTTP.Request
    mkRequest = do
      path <- requestPath r . unCredentials . configCredentials <$> ask
      ip <- ipAddress . configIP <$> ask
      pure 
        $ setRequestPath path
        $ setRequestMethod (requestMethod r)
        $ setRequestHost ip
        $ withRequestBody r $ \case
          (Body b) -> setRequestBodyJSON b defaultRequest
          _        -> defaultRequest

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
} deriving (Show, Eq, Ord)

-- | Identifies a bridge uniquely.
data Bridge = Bridge {
    bridgeIP :: BridgeIP
  , bridgeSerial :: Text
  , bridgeIconURL :: Text
} deriving (Show)

instance Eq Bridge where
  b1 == b2 = bridgeSerial b1 == bridgeSerial b2

deriving instance Ord Bridge

-- | BridgeIP can be created from String literals/
instance IsString BridgeIP where
  fromString = BridgeIP . fromString


-- | An authentication token needed for most endpoints.
newtype HueCredentials = HueCredentials {
  unCredentials :: Text
} deriving (Show)

-- | We're able to receive credentials from the bridge.
instance FromJSON HueCredentials where
  parseJSON v = do
    [HueSuccess c] <- parseJSON v
    withObject "HueCredentials object" (\o -> HueCredentials <$> o .: "username") c

-- | Credentials can be used as part of an API path.
instance ToPathSegment HueCredentials where
  toSegment (HueCredentials creds) = TextSegment creds

-- | Device type used when registering a new application with the bridge.
newtype HueDeviceType = HueDeviceType Text deriving (Show)

-- | We're able to send a device type to the brige for registration.
instance ToJSON HueDeviceType where
  toJSON (HueDeviceType devType) = toJSON $ Map.fromList [("devicetype" :: Text, devType)]  


-- ----------------------------------------------------------------
-- BASE TYPES
--   Hue API response representation
-- ----------------------------------------------------------------

-- | Representation for all responses from the bridge.
data HueResponse a = 
    HueErrorResponse [HueError]
  | HueResponse a 
  deriving (Show)

-- | Any response from the bridge can be decoded.
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

-- | Success messages can be decoded.
instance (FromJSON a) => FromJSON (HueSuccess a) where
  parseJSON = withObject "HueSuccess object" $ \o -> do
    s <- o .: "success"
    HueSuccess <$> parseJSON s

-- | Errors returned by the bridge.
data HueError = HueError Int Text deriving (Show)

-- | Error responses can be decoded.
instance FromJSON HueError where 
  parseJSON = withObject "HueError object" $ \o -> do
    e <- o .: "error"
    HueError <$> e .: "type" <*> e .: "description"

-- | Wrapper for errors returned by the bridge.
data HueApiException = HueApiException [HueError] deriving (Show)

-- | We might throw HueApiException.
instance Exception HueApiException

-- | When the bridge response cannot be parsed...
newtype JSONParseException = JSONParseException Text deriving (Show)

-- | We might throw JSONParseException.
instance Exception JSONParseException