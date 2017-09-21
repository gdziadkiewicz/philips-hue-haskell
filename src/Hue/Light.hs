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
-- $changingLightState
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

-- $changingLightState
-- To change the state of a single light, a 'SetLightState' object
-- has to be sent to the 'setLight' endpoint.  
-- 
-- Example:
-- 
-- @
--  lightKitchen = do
--    Just l <- lightWithName \"Kitchen\"
--    request (setLight l) on
-- @
-- 
-- Note that because 'SetLightState' represents a set of state 
-- changes, it also supports monoidially combining:
--  
-- @
--  purpleKitchen = do
--    Just l <- lightWithName \"Kitchen\"
--    request (setLight l) (on <> brightness 255 <> setHue 48500 <> saturation 255)
-- @
-- 