-- |
-- Module: Hue.Light
-- Copyright: (c) 2017 Thomas Smith
-- License: BSD3
-- Maintainer: Thomas Smith <tnsmith@live.nl>
-- Stability: experimental
--
-- Automatically discover Hue bridges on a local network.
module Hue.Discover (
    upnpDiscoverBridges
) where

import Hue.Internal.Discover
