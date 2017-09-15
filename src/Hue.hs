-- | 
-- Module: Hue.Internal.Light 
-- Copyright: (c) 2017 Thomas Smith
-- License: BSD3
-- Maintainer: Thomas Smith <tnsmith@live.nl>
-- Stability: experimental
--
-- Control Philips Hue lights. 
module Hue (
-- * How to use this library
-- $usage
  request
, runHue
, evalHue
-- * Basic setup types
, HueCredentials
, BridgeIP(..)
, HueDeviceType(..)
, defaultConfig
, HueConfig(..)
-- * The Hue monad
, Hue(..)
-- * Response and Error representations
, HueSuccess(..)
, HueException(..)
, HueError(..)
) where

import Control.Monad.Except
import Hue.Internal
import Hue.Auth (getHueCredentials)

-- $usage
-- Hello!


-- | Evaluates a Hue action.
-- 
-- Fetches credentials from file if present,
-- otherwise performs pushlink registration.
runHue :: MonadIO m => Hue a -> m a
runHue h = do
  creds <- evalHue defaultConfig getHueCredentials
  evalHue (defaultConfig { configCredentials = creds }) h
