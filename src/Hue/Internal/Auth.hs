-- | 
-- Module: Hue.Internal.Light 
-- Copyright: (c) 2017 Thomas Smith
-- License: BSD3
-- Maintainer: Thomas Smith <tnsmith@live.nl>
-- Stability: experimental
--
-- Everything needed to authenticate with the Hue Bridge
-- and store the credentials.
-- 
-- This is an internal module.
-- 
-- Please use "Hue.Auth" instead. 
module Hue.Internal.Auth where
  
import Network.HostName (getHostName)

import Path
import Path.IO
import qualified Data.Text.IO as T

import Data.Text (strip)
import qualified Data.Text as Text

import Data.Maybe (fromJust)

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad.Loops (untilJust)
import Control.Monad.Except
import Control.Monad.Trans.Maybe

import Hue.Internal
import Hue.Internal.Request

-- | The authentication request.
-- 
-- Send this request after the user pushes the bridge button to get fresh credentials.
--
-- See also: 'registerApp' 
auth :: HueDeviceType -> Request HueCredentials
auth d = post (Body d) ParseResult root

-- | Get an authentication token.
-- This function first tries to read the token from @~\/.hue\/credentials@.
-- If this fails, 'registerApp' is called and the result will be stored on disk. 
getHueCredentials :: Hue HueCredentials
getHueCredentials = do
  fromJust <$> runMaybeT (
    do 
      liftIO $ putStrLn "Reading credentials from file..."
      fetchCredentialsFromDisk
    <|> 
    do 
      liftIO $ putStrLn "Credentials not present in file, registering with bridge..."
      u <- lift $ registerApp
      writeCredentialsToDisk u
      pure u
    )


-- | Register the application with the bridge
-- 
-- This function waits until the user pushes the bridge button
-- and registers with the current hostname as device name.
registerApp :: Hue HueCredentials
registerApp = do
  liftIO $ putStrLn "Push the button on your bridge..."
  untilJust $ do 
    liftIO $ threadDelay 1000000
    tryRegisterWithBridge
  
  where
    tryRegisterWithBridge :: Hue (Maybe HueCredentials)
    tryRegisterWithBridge = (do 
        deviceName <- liftIO getHostName
        Just <$> (request $ auth $ HueDeviceType $ Text.pack deviceName)
      ) `catchError` handleApiException

    handleApiException :: (MonadIO m, MonadError HueApiException m) => HueApiException -> m (Maybe a)
    handleApiException err@(HueApiException [HueError code _]) = 
      if buttonNotYetPushed
        then pure Nothing
        else throwError err
      where
        buttonNotYetPushed = code == 101
  
-- | Fetch the path where the config files for this library are cached.
getHueDir :: MonadIO m => m (Path Abs Dir)
getHueDir = do
  hueDir <- liftIO $ getAppUserDataDir "hue"
  createDirIfMissing False hueDir
  pure hueDir

-- | Get the file path where the Hue access token is stored.
getHueCredentialsPath :: MonadIO m => m (Path Abs File)
getHueCredentialsPath = do
  hueDir <- getHueDir
  liftIO $ resolveFile hueDir "credentials"

-- | Fetch the Hue access token from disk.
fetchCredentialsFromDisk :: (MonadIO m, MonadPlus m) => m HueCredentials
fetchCredentialsFromDisk = do
  usernamePath <- getHueCredentialsPath
  doesFileExist usernamePath >>= guard
  hueCredentials <- liftIO $ T.readFile $ toFilePath usernamePath
  guard $ (not . Text.null $ hueCredentials)
  pure $ HueCredentials (strip hueCredentials)

-- | Write a newly obtained access token to disk.
-- This will overwrite any content in the credentials file.
writeCredentialsToDisk :: MonadIO m => HueCredentials -> m ()
writeCredentialsToDisk (HueCredentials u) = do
  usernamePath <- getHueCredentialsPath
  liftIO $ T.writeFile (toFilePath usernamePath) u
