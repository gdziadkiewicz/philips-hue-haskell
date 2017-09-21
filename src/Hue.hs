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
-- * Configuration
-- $config 
, HueCredentials
, BridgeIP(..)
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
import Hue.Light
import Hue.Endpoint
import Hue.Auth (getHueCredentials)

-- $usage
-- This library mainly revolves around one function: 'request'.
-- The request function can be used in with types from 
--
-- This function needs to be supplied with an 'Endpoint'.
-- Endpoints can be found in the module that they functionally belong to, for example see 'lights' in "Hue.Light"
-- The result of calling 'request' depends on the type of the supplied Endpoint.
-- There are two possible return types for 'request':
-- 
--  * If the body type is '()', requesting the endpoint immediately results in a Hue action.
-- 
--    >>> let someEndpoint = ... :: Endpoint 'GET () SomeResult
--    >>> :t request someEndpoint
--    request someEndpoint :: Hue SomeResult
-- 
--  * Otherwise, 'request' will instead return a function asking for the body:
-- 
--    >>> let someEndpoint = ... :: Endpoint 'POST Int SomeResult
--    >>> :t request someEndpoint
--    request someEndpoint :: Int -> Hue SomeResult
-- 
-- This makes 'request' type safe with respect to the request method, body and result.
-- For example, 'request'ing and endpoint that performs a @GET@ with a request body yields a type error:
-- 
-- >>> let someEndpoint = ... :: Endpoint 'GET Int SomeResult
-- >>> :t request someEndpoint
-- <interactive>:1:1: error:
--     • HTTP method GET does not allow attaching a request body.
--     • In the expression: request someEndpoint
-- 
-- See "Hue.Endpoint" for more details.

-- $config
-- All types needed to configure requests to the Hue bridge.
-- 


-- | Evaluates a Hue action.
-- 
-- Fetches credentials from file if present,
-- otherwise performs pushlink registration.
runHue :: MonadIO m => Hue a -> m a
runHue h = do
  creds <- evalHue defaultConfig getHueCredentials
  evalHue (defaultConfig { configCredentials = creds }) h
