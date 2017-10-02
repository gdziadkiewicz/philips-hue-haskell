-- | 
-- Module: Hue.Light 
-- Copyright: (c) 2017 Thomas Smith
-- License: BSD3
-- Maintainer: Thomas Smith <tnsmith@live.nl>
-- Stability: experimental
--
-- Everything needed to authenticate with the Hue Bridge,
-- and store the credentials.
module Hue.Auth (
  auth
, getHueCredentials
, registerApp
) where 
  
import Hue.Internal.Auth