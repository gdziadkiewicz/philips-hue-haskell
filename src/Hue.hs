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
-- * Running Hue actions
-- $evaluating
  runHue
, runHue_
, evalHue
-- * Configuration
-- $conf
, HueConfig(..)
, HueCredentials
, BridgeIP(..)
-- * The Hue monad
, Hue(..)
-- * Response and Error representations
, HueSuccess(..)
, HueApiException(..)
, HueError(..)
) where

import Control.Monad.Except
import Hue.Internal
import Hue.Config

-- $usage
-- There are two ways in which you can use this library:
--
--  * Use pre-defined actions of type @('Hue' a)@, e.g.: 'Hue.Light.lights',
--
--  * Call an endpoint with the "Hue.Request" module to construct your own 'Hue' actions.
--
-- See 'runHue' and 'evalHue' for details on evaluating the actions in @IO@.

-- $evaluating
-- Any value of type @('Hue' a)@ can be evaluated in IO with 'runHue' or 'evalHue':

-- | Evaluates a Hue action.
--
-- Fetches config from file if present,
-- otherwise performs interactive bridge discovery and registration.
runHue :: MonadIO m
       => Hue a -- ^ The action to evaluate
       -> m (Either HueApiException a)
runHue h = do
  conf <- liftIO $ getHueConfig
  evalHue conf h

-- | Run a Hue action, throwing an error on API exceptions.
runHue_ :: MonadIO m
        => Hue a -- ^ The action to evaluate
        -> m a
runHue_ h = do
  conf <- liftIO $ getHueConfig
  unsafeEvalHue conf h

-- $conf
-- These are all data types necessary for constructing a configuration.
-- See "Hue.Config" for automatically obtaining a 'Hue.Config.HueConfig'.
