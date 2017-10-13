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
-- * Running Hue actions
-- $evaluating
, runHue
, evalHue
-- * Configuration
, HueConfig(..)
, configWithIP
, HueCredentials
, BridgeIP(..)
-- * The Hue monad
, Hue(..)
-- * Response and Error representations
, HueSuccess(..)
, HueApiException(..)
, HueError(..)
, x
) where

import Control.Monad.Except
import Hue.Internal
import Hue.Light
import Hue.Request
import Hue.Auth (getHueCredentials)

-- $usage
-- There are two ways in which you can use this library:
-- 
--  * Use pre-defined actions of type @('Hue' a)@, e.g.: 'fetchLights',
-- 
--  * Call an endpoint with 'request' to construct your own 'Hue' actions.
-- 
-- See 'runHue' and 'evalHue' for details on evaluating the actions in @IO@.

-- $evaluating
-- Any value of type @('Hue' a)@ can be evaluated in IO with 'runHue' or 'evalHue':

-- | Evaluates a Hue action.
-- 
-- Fetches credentials from file if present,
-- otherwise performs pushlink registration.
-- Requires only the bridge IP since the credentials are fetched for you.
runHue :: MonadIO m 
       => BridgeIP -- ^ The bridge IP address
       -> Hue a -- ^ The action to evaluate 
       -> m a
runHue ip h = do
  let conf = configWithIP ip
  creds <- evalHue conf getHueCredentials
  evalHue (conf { configCredentials = creds }) h

x :: IO ()
x = runHue "192.168.1.100" $ do
  Just l <- lightWithName "Ufo"
  void $ request $ setLight on l