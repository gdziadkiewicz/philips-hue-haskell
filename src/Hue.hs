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
-- Running Hue actions
-- $evaluating 
  request
, runHue
, evalHue
-- * Configuration
-- $config 
, HueCredentials
, BridgeIP(..)
, configWithIP
, HueConfig(..)
-- * The Hue monad
, Hue(..)
-- * Response and Error representations
, HueSuccess(..)
, HueApiException(..)
, HueError(..)
) where

import Control.Monad.Except
import Hue.Internal
import Hue.Light
import Hue.Request
import Hue.Auth (getHueCredentials)

-- $usage
-- This library mainly revolves around one function: 'request'. 
--
-- This function needs to be supplied with a 'Request'.
-- Requests can be found in the module that they functionally belong to.
-- There are requests for authentication, fetching and changing lights and mode.
-- For example see 'lights' in "Hue.Light". 
-- The result of calling 'request' depends on the type  of the supplied 'Request'. 
-- There are two possible return types for 'request':
-- 
--  * If the body type is '()', request immediately results in a Hue action.
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

-- $evaluating
-- Any value of type @'Hue' a@ can be evaluated in IO with 'runHue' or 'evalHue'
-- 
-- In order to run an action with 'evalHue', you must have a valid 'HueConfig' object.
-- 'configWithIP' defines a HueConfig for accessing unauthenticated endpoints.
-- 
-- The config object contains the bridge IP address and (optionally) credentials
-- needed to turn lights on and off.
-- To get a HueConfig with credentials, can use the functions from "Hue.Auth".
-- For example, if you wish to do pushlink authentication, use 'registerApp'.
-- 'getHueCredentials' will use 'registerApp' once and store the credentials for
-- subsequent uses.
-- 
-- $config
-- All types needed to configure requests to the Hue bridge.


-- | Evaluates a Hue action.
-- 
-- Fetches credentials from file if present,
-- otherwise performs pushlink registration.
runHue :: MonadIO m 
       => BridgeIP -- ^ The bridge IP address
       -> Hue a -- ^ The action to evaluate 
       -> m a
runHue ip h = do
  let conf = configWithIP ip
  creds <- evalHue conf getHueCredentials
  evalHue (conf { configCredentials = creds }) h