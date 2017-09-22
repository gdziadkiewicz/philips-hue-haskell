-- | 
-- Module: Hue.Light 
-- Copyright: (c) 2017 Thomas Smith
-- License: BSD3
-- Maintainer: Thomas Smith <tnsmith@live.nl>
-- Stability: experimental
--
-- Everything for the Hue Lights API.
module Hue.Light (
  -- * Fetching lights
  lights
, fetchLights
, lightWithName
-- * Changing light state
, setLight
, Hue.Internal.Light.SetLightState
, on, off, toggle
, brightness, alterBrightness, increaseBrightness, decreaseBrightness
, setHue, alterHue, increaseHue, decreaseHue
, saturation, alterSaturation, increaseSaturation, decreaseSaturation
, xy, alterXY, increaseXY, decreaseXY
, temperature, alterTemperature, increaseTemperature, decreaseTemperature
, alert, colorLoop, noEffect
, Hue.Internal.Light.Alert(..)
, Hue.Internal.Light.Effect(..)
-- * Renaming / deleting lights
, renameLight
, deleteLight
-- * Searching for new lights
, searchNewLights
, newLights
, ScanResult(..)
, ScanStatus(..)
-- * Common types
, Light
, lightState
, lightName
, lightType
, modelid
, swversion
, lightId
, LightState
, isOn
, lightBrightness
, alertState
, isReachable
, colorTemperature
, colorState
, ColorState
, colorHue
, colorSaturation
, colorXY
, colorEffect
, colorMode
, ColorMode(..)
, LightType(..)
, LightName(..)
, LightID
, lightIDInt
, LightIDType
, KnownID(..)
) where 

import Hue.Internal.Light
