-- |
-- Module: Hue.Internal
-- Copyright: (c) 2018 Thomas Smith
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

import Prelude hiding (putStrLn)


import Data.Aeson hiding (Result)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

import Data.String
import Data.ByteString (ByteString)

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Control.Applicative
import Control.Exception.Safe
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Fail

-- | The Hue Monad.
newtype Hue a = Hue {
  unHue :: ReaderT HueConfig (ExceptT HueApiException IO) a
  } deriving (
    Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadReader HueConfig
  , MonadError HueApiException
  , MonadFail)

evalHue :: MonadIO m => HueConfig -> Hue a -> m (Either HueApiException a)
evalHue config h = do
  e <- unwrapHue config h
  case e of
    Left (JSONParseException err) -> error $ Text.unpack err
    Right x -> pure x
  where
    unwrapHue c = liftIO . try . runExceptT . flip runReaderT c . unHue

-- | Evaluate a hue action with a specific config.
unsafeEvalHue :: MonadIO m => HueConfig -> Hue a -> m a
unsafeEvalHue config h = do
  e <- unwrapHue config h
  case e of
    Left (JSONParseException err) -> error $ Text.unpack err
    Right (Left err) -> error $ show err
    Right (Right a) -> pure a
  where
    unwrapHue c = liftIO . try . runExceptT . flip runReaderT c . unHue



-- ----------------------------------------------------------------
-- BASE TYPES
--   Authentication
-- ----------------------------------------------------------------

-- | Configuration necessary to query the bridge.
--
-- Contains the bridge IP address and (optionally) credentials needed to turn lights on and off.
--
-- See "Hue.Config"
data HueConfig = HueConfig {
  configIP :: BridgeIP
, configCredentials :: HueCredentials
} deriving (Show)

-- | Used to read config from file.
instance FromJSON HueConfig where
  parseJSON = withObject "HueConfig object" $ \o ->
    HueConfig
    <$> o .: "bridge-ip-address"
    <*> (HueCredentials <$> o .: "app-credentials")

-- | Used to write the config to file.
instance ToJSON HueConfig where
  toJSON (HueConfig ip creds) = object
    [ ("bridge-ip-address", toJSON ip)
    , ("app-credentials", toJSON creds)
    ]

-- | The IP address of the bridge to send requests to.
newtype BridgeIP = BridgeIP {
  ipAddress :: ByteString
} deriving (Show, Eq, Ord)

-- | Used to read the bridge IP from a config file
instance FromJSON BridgeIP where
  parseJSON = withText "Bridge IP address" $ pure . BridgeIP . encodeUtf8

-- | Used to read the bridge IP to a config file
instance ToJSON BridgeIP where
  toJSON (BridgeIP ip) = String $ decodeUtf8 ip

-- | Identifies a bridge uniquely.
data Bridge = Bridge {
    bridgeIP :: BridgeIP
  , bridgeSerial :: Text
  , bridgeIconURL :: Text
} deriving (Show)

-- | Bridge equality is determined by the serial number.
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

-- | Used to write credentials to a config file.
instance ToJSON HueCredentials where
  toJSON (HueCredentials c) = String c

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
data HueApiException = HueApiException [HueError] deriving (Show, Typeable)

-- | We might throw HueApiException.
instance Exception HueApiException

-- | When the bridge response cannot be parsed...
newtype JSONParseException = JSONParseException Text deriving (Show, Typeable)

-- | We might throw JSONParseException.
instance Exception JSONParseException
