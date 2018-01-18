-- |
-- Module: Hue.Config
-- Copyright: (c) 2017 Thomas Smith
-- License: BSD3
-- Maintainer: Thomas Smith <tnsmith@live.nl>
-- Stability: experimental
--
-- Everything needed for obtaining and storing a bridge configuration.
module Hue.Config (
    configWithIP
  , getHueConfig
  , interactiveConfigureHue
) where

import Hue.Internal.Config