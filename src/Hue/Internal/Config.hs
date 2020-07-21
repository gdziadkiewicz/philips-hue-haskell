-- |
-- Module: Hue.Internal.Config
-- Copyright: (c) 2018 Thomas Smith
-- License: BSD3
-- Maintainer: Thomas Smith <tnsmith@live.nl>
-- Stability: experimental
--
-- Definitions for obtaining and storing a bridge configuration.
--
-- This is an internal module.
--
-- Please use "Hue.Config" instead.
module Hue.Internal.Config where

import Prelude hiding (readFile, writeFile, putStrLn)

import Path
import Path.IO

import Data.ByteString.Lazy hiding (putStrLn)

import Data.Text.IO (putStrLn)
import qualified Data.Text as Text

import Data.Maybe

import Control.Exception.Safe
import Control.Applicative
import Control.Monad.Except
import Control.Monad.Trans.Maybe

import Data.Aeson

import Hue.Internal
import Hue.Auth
import Hue.Discover

-- | Create a HueConfig with an explicit bridge IP address.
--
-- Credentials will be set to a default that can only be used against unauthenticated resources.
--
-- To get a HueConfig with credentials, use 'getHueCredentials' or 'registerApp'
configWithIP :: BridgeIP -> HueConfig
configWithIP ip = HueConfig ip (HueCredentials "-")

-- | Get a configuration.
--
-- This function first tries to read the config from @~\/.hue\/config.json@.
-- If this fails, it invokes 'interactiveConfigureHue' and the result will be stored on disk.
getHueConfig :: IO HueConfig
getHueConfig = do
  mConf <- runMaybeT $ fetchConfigFromDisk <|> interactiveConfigureHue
  maybe (throwString "Hue bridge configuration failed. No bridges could be found") pure mConf

-- | Interactively get a fresh configuration.
--
-- This function invokes 'upnpDiscoverBridges' and 'registerApp' for the first bridge found.
-- Results are stored in @~\/.hue\/config.json@.
interactiveConfigureHue :: MaybeT IO HueConfig
interactiveConfigureHue = do
  liftIO $ putStrLn
    "It seems like there is no configuration present.\n\
    \Searching for bridge, this may take a minute..."
  Bridge ip serial _ <- MaybeT $ listToMaybe <$> (upnpDiscoverBridges <> nupnpDiscoverBridges)
  liftIO $ putStrLn $ "Found bridge with id: " `Text.append` serial
  credentials <- unsafeEvalHue (configWithIP ip) registerApp
  let config = HueConfig ip credentials
  liftIO $ writeConfigToDisk config
  pure config

-- | Fetch the path where the config files for this library are cached.
getHueDir :: MonadIO m => m (Path Abs Dir)
getHueDir = do
  hueDir <- liftIO $ getAppUserDataDir "hue"
  createDirIfMissing False hueDir
  pure hueDir

-- | Get the file path where the Hue access token is stored.
getHueConfigPath :: MonadIO m => m (Path Abs File)
getHueConfigPath = do
  hueDir <- getHueDir
  liftIO $ resolveFile hueDir "config.json"


-- | Fetch the Hue access token from disk.
fetchConfigFromDisk :: MaybeT IO HueConfig
fetchConfigFromDisk = do
  configPath <- getHueConfigPath
  doesFileExist configPath >>= guard
  rawConfig <- liftIO $ readFile $ toFilePath configPath
  MaybeT $ pure $ decodeStrict $ toStrict rawConfig

-- | Write a configuration to disk.
-- This will overwrite any content in the existing configuration file.
writeConfigToDisk :: HueConfig -> IO ()
writeConfigToDisk config = do
  configPath <- getHueConfigPath
  writeFile (toFilePath configPath) (encode config)

