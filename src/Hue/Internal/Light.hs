{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
-- |
-- Module: Hue.Internal.Light 
-- Copyright: (c) 2017 Thomas Smith
-- License: BSD3
-- Maintainer: Thomas Smith <tnsmith@live.nl>
-- Stability: experimental
--
-- Types representing commands to change the state of a light.
module Hue.Internal.Light where

import Prelude hiding (fail)

import Data.Aeson
import Data.Time (LocalTime)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.HashMap.Lazy as HashMap
import Data.Word
import Data.Foldable

import Control.Monad.Fail

import Hue.Internal
import Hue.Internal.Request

-- ----------------------------------------------------------------
-- Fetching lights
-- ----------------------------------------------------------------

-- | Request for fetching all lights.
-- 
-- By default, the API returns this as a Map of lights where the 
-- key of the Map is the light ID.
-- 
-- To fetch all lights with their ID, use 'fetchLights'.
lights :: Request (Map LightID (Light 'WithoutID))
lights = get ParseResult $ api /~ credentials /: "lights"

-- | Fetch a single light by it's name.
-- 
-- 'Nothing' when there exists no light with the given name.
lightWithName :: Text -> Hue (Maybe (Light 'WithID))
lightWithName n = find ((LightName n ==) . lightName) <$> fetchLights
  
-- | Fetch all lights including their IDs.
fetchLights :: Hue [Light 'WithID]
fetchLights = fmap (uncurry setLightID) <$> Map.toList <$> request lights 
  where 
    setLightID i l = l { lightId = i }

-- ----------------------------------------------------------------
-- Changing light status
-- ----------------------------------------------------------------

-- | To change the state of a single light, a 'SetLightState' object
-- has to be sent with the 'setLight' request.  
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
-- changes, it also supports monoidial combining:
--  
-- @
--  veryPurpleKitchen = do
--    Just l <- lightWithName \"Kitchen\"
--    request (setLight l) (on <> brightness 255 <> setHue 48500 <> saturation 255)
-- @
--
setLight :: SetLightState -> Light 'WithID -> Request ()
setLight newState Light{..} = put (Body newState) IgnoreResult $ api /~ credentials /: "lights" /~ lightId /: "state"

-- | Turn the light on.
on :: SetLightState
on = mkSetLightState $ SetOn True

-- | Turn the light off.
off :: SetLightState
off = mkSetLightState $ SetOn False

-- | If the light is off then turn it on, otherwise turn it off.
toggle :: LightState -> SetLightState
toggle LightState{..} = mkSetLightState $ SetOn (not isOn)


-- | Set the brightness to a specific value.
brightness :: Word8 -> SetLightState
brightness = mkSetLightState . SetBrightness

-- | Apply a funciton to the current brightness.
alterBrightness :: LightState -> (Word8 -> Word8) -> SetLightState
alterBrightness current change = mkSetLightState $ SetBrightness $ change $ lightBrightness current

-- | Increase the current brightness.
increaseBrightness :: Word8 -> SetLightState
increaseBrightness = mkSetLightState . IncBrightness . fromEnum

-- | Decrease the current brightness level.
decreaseBrightness :: Word8 -> SetLightState
decreaseBrightness = mkSetLightState . IncBrightness . negate . fromEnum


-- | Set the color hue to a specific value.
setHue :: Word16 -> SetLightState
setHue = mkSetLightState . SetHue

-- | Apply a function to the current color hue.
alterHue :: ColorState -> (Word16 -> Word16) -> SetLightState
alterHue current change = mkSetLightState $ SetHue $ change $ colorHue current

-- | Increase the current color hue.
increaseHue :: Word16 -> SetLightState
increaseHue = mkSetLightState . IncHue . fromEnum

-- | Decrease the current color hue.
decreaseHue :: Word16 -> SetLightState
decreaseHue = mkSetLightState . IncHue . negate . fromEnum


-- | Set the color saturation to a specific value.
saturation :: Word8 -> SetLightState
saturation = mkSetLightState . SetSaturation

-- | Apply a function to the current color saturation.
alterSaturation :: ColorState -> (Word8 -> Word8) -> SetLightState
alterSaturation current change = mkSetLightState $ SetSaturation $ change $ colorSaturation current

-- | Increase the current color saturation.
increaseSaturation :: Word8 -> SetLightState
increaseSaturation = mkSetLightState . IncSaturation . fromEnum

-- | Decrease the current color saturation.
decreaseSaturation :: Word8 -> SetLightState
decreaseSaturation = mkSetLightState . IncSaturation . negate . fromEnum

-- | Set the [CIE color space coordinates](https://www.developers.meethue.com/documentation/core-concepts#color_gets_more_complicated) 
-- to a specific value.
xy :: (Float, Float) -> SetLightState
xy = mkSetLightState . SetXY

-- | Apply a function to the current CIE coordinates.
alterXY :: ColorState -> ((Float, Float) -> (Float, Float)) -> SetLightState
alterXY current change = mkSetLightState $ SetXY $ change $ colorXY current

-- | Increase the current CIE color coordinates.
increaseXY :: (Float, Float) -> SetLightState
increaseXY = mkSetLightState . IncXY

-- | Decrease the current CIE color coordinates.
decreaseXY :: (Float, Float) -> SetLightState
decreaseXY (x,y) = mkSetLightState $ IncXY $ (negate x, negate y)

-- | Set the [Mired color temperature](https://en.wikipedia.org/wiki/Mired) 
-- to a specific value.
temperature :: Word16 -> SetLightState
temperature = mkSetLightState . SetTemperature

-- | Apply a function to the current color temperature.
alterTemperature :: LightState -> (Word16 -> Word16) -> SetLightState
alterTemperature current change = case colorTemperature current of
  Just ct -> mkSetLightState $ SetTemperature $ change ct
  Nothing -> SetLightState Set.empty

-- | Increase the current color temperature.
increaseTemperature :: Word16 -> SetLightState
increaseTemperature = mkSetLightState . IncTemperature . fromEnum

-- | Decrease the current color temperature.
decreaseTemperature :: Word16 -> SetLightState
decreaseTemperature = mkSetLightState . IncTemperature . negate . fromEnum


-- | Make the light flash
alert :: Alert -> SetLightState
alert = mkSetLightState . SetAlert

-- | Make the light loop through colors.
-- 
-- Only supported on lights with @'lightType' >= 'ColorLight'@.
colorLoop :: SetLightState
colorLoop = mkSetLightState $ SetEffect ColorLoop

-- | Stop the light from 'ColorLoop'ing. The light will return to it's original state.
noEffect :: SetLightState
noEffect = mkSetLightState $ SetEffect NoEffect


-- ----------------------------------------------------------------
-- Renaming / deleting lights
-- ----------------------------------------------------------------

-- | Request for when you wish to change the name of a light.
-- 
-- A light can have its name changed even when it is unreachable 
-- or turned off.
renameLight :: Light 'WithID -> LightName -> Request ()
renameLight Light{..} name = put (Body name) IgnoreResult $ api /~ credentials /: "lights" /~ lightId

-- | Request for deleting a light.
-- 
-- The light will be removed from the list of lights from and any
-- groups in the bridge. This will cause Scenes to be updated.
-- 
-- Note that the device is not physically removed from the ZigBee
-- network.
deleteLight :: Light 'WithID -> Request ()
deleteLight Light{..} = delete IgnoreResult $ api /~ credentials /: "lights" /~ lightId


-- ----------------------------------------------------------------
-- Searching for unregistered lights
-- ----------------------------------------------------------------

-- | Starts a search for new lights. Also finds switches like the
-- Hue tap.
-- 
-- The bridge will open the network for 40s. The overall search
-- might take longer since the configuration of (multiple) new
-- devices can take longer. If many devices are found the command
-- will have to be issued a second time after discovery time has
-- elapsed. If the command is received again during search the
-- search will continue for at least an additional 40s.
-- 
-- When the search has finished, new lights will be available using
-- the 'newLights' request. In addition, the new lights will now
-- be available by calling 'lights' or 'fetchLights' or by calling 
-- get group attributes on group 0. 
-- 
-- Group 0 is a special group that cannot be 
-- deleted and will always contain all lights known by the bridge.
searchNewLights ::  Request ()
searchNewLights = post NoBody IgnoreResult $ api /~ credentials /: "lights"

-- | Gets a 'ScanResult' since the last time 'searchNewLights'
-- was requested.
-- 
-- The returned scan results are reset when a new scan is started.
newLights :: Request ScanResult
newLights = get ParseResult $ api /~ credentials /: "lights" /: "new"


-- ----------------------------------------------------------------
-- Light datatypes
-- ----------------------------------------------------------------

-- | Indicate, at the type level,
-- whether a light has an identifier.
data KnownID = 
    WithID
  | WithoutID

-- | Type-level function computing the type of light identifier for a 'KnownID'.
type family LightIDType (a :: KnownID) where
  LightIDType 'WithID = LightID
  LightIDType 'WithoutID = ()

-- | Represents a light.
-- 
-- This type is indexed by a 'KnownID' because some requests
-- return lights without their ID.
-- 
-- See 'lights' vs 'fetchLights'.
data Light (a :: KnownID) = Light { 
    lightState :: LightState -- ^ The current state of a bulb.
  , lightName :: LightName -- ^ Get the current display name of a bulb.
  , lightType :: LightType -- ^ Get the supported features of a bulb.
  , modelid :: Text -- ^ Get the model identifier of a bulb.
  , swversion :: Text -- ^ Get the current software version of a bulb.
  , lightId :: LightIDType a -- ^ Get the unique identifier of a light. Note that @'Light' 'WithoutID'@ has a @()@ identifier.
}

deriving instance Show (Light 'WithID)
deriving instance Show (Light 'WithoutID)

-- | The display name of a light.
newtype LightName = LightName {
  lightNameText :: Text -- ^ Get the textual representation of a light name for human consumption.
} deriving (Eq, Ord, Show, FromJSON)

-- | Used to identify an individual light with the bridge.
newtype LightID = LightID {
  lightIDInt :: Int -- ^ Get the integer value representing a LigthID.
} deriving (Eq, Ord, Show, FromJSONKey)

-- | Determines which features a bulb supports.
data LightType =
    DimmableLight -- ^ Lights for which the 'brightness' can be adjusted.
  | ColorLight -- ^ Lights for which 'brightness' and 'ColorState' are available.
  | ColorTemperatureLight -- ^ Lights which support only 'brightness' and 'temperature'.
  | ExtendedColorLight -- ^ Lights which support all the above.
  deriving (Eq, Ord, Show)

-- | Fully describes the current state of a light.
-- Note that some properties (e.g. brightness) can be non-zero
-- even thought the light is tuned off.
data LightState = LightState {
    isOn :: Bool -- ^ Whether the light is on or not.
  , lightBrightness :: Word8 -- ^ The currently set brightness.  
  , alertState :: Alert -- ^ Whether the light is performing an @alert@ or not
  , isReachable :: Bool -- ^ Whether the light is reachable or not.
  , colorTemperature :: Maybe Word16 -- ^ Current [Mired color temperature](https://en.wikipedia.org/wiki/Mired).
  , colorState :: Maybe ColorState -- ^ Indicates the current color status for lights with @'lightType' >= 'ColorLight'@.
} deriving (Show)

-- | Indicates the current color status of a light.
--
-- Only lights with @'lightType' >= 'ColorLight'@ have a ColorState.
data ColorState = ColorState {
    colorHue :: Word16 -- ^ Current color hue.
  , colorSaturation :: Word8 -- ^ Current color saturation.
  , colorXY :: (Float, Float) -- ^ Current [CIE color space coordinates](https://www.developers.meethue.com/documentation/core-concepts#color_gets_more_complicated).
  , colorEffect :: Effect -- ^ Whether the light is in a color loop or not.
  , colorMode :: ColorMode -- ^ Current color mode in which the light is operating. Determined by the last received command.
} deriving (Eq, Show)

-- | The current mode for setting colors for a light.
data ColorMode =
    HueSaturationMode -- ^ Colors are set by controlling hue/saturation.
  | XYMode -- ^ Colors are set by specifying the coordinates in [CIE color space coordinates](https://www.developers.meethue.com/documentation/core-concepts#color_gets_more_complicated).
  | ColorTemperatureMode -- ^ Color is set by [Mired color temperature](https://en.wikipedia.org/wiki/Mired).
  deriving (Eq, Ord, Show)

-- | Results obtained from the last time a scan for new lights was
-- performed. See 'searchNewLights' for more details.
data ScanResult = ScanResult {
    foundLights :: [(LightID, LightName)] -- ^ All new lights that have been found.
  , lastScan :: ScanStatus -- ^ The status of the last scan.
} deriving (Eq, Show)

-- | The state of a scan for new lights.
data ScanStatus = 
    NoScanResult -- ^ No scan has been perfomed recently.
  | ScanActive -- ^ A scan is currently active.
  | LastScan LocalTime -- ^ A scan was last perfomed at 'LocalTime'.
  deriving (Eq, Show)

-- ----------------------------------------------------------------
-- Updating Lights
--   Internal types
-- ----------------------------------------------------------------
-- | A set of state change commands to send to the bridge.
newtype SetLightState = SetLightState (Set SetStateComponent) deriving (Show, Monoid)

-- | Convenience constructor for 'SetLightState'
mkSetLightState :: SetStateComponent -> SetLightState
mkSetLightState = SetLightState . Set.singleton

-- | Commands that can be sent to the bridge to change the light state.
data SetStateComponent =
    SetOn Bool
  | SetBrightness Word8
  | IncBrightness Int
  | SetHue Word16 
  | IncHue Int
  | SetSaturation Word8
  | IncSaturation Int
  | SetXY (Float, Float)
  | IncXY (Float, Float)
  | SetTemperature Word16
  | IncTemperature Int
  | SetAlert Alert
  | SetEffect Effect
  deriving (Eq, Ord, Show)

-- | Turn the light into @alert@ mode, making it flash for one or more cycles.
data Alert = 
    NoAlert
  | OneCycle
  | MultipleCycles
  deriving (Eq, Ord, Show)

-- | Hue built-in gimmick that lets the light loop through it's color range.   
data Effect = 
    NoEffect
  | ColorLoop
  deriving (Eq, Ord, Show)

-- ----------------------------------------------------------------
-- JSON Conversion instances
-- ----------------------------------------------------------------

instance FromJSON (Light 'WithoutID) where
  parseJSON = withObject "Light state object" $ \v -> do
    state <- v .: "state"
    Light 
      <$> parseLightState state
      <*> v .: "name"
      <*> v .: "type"
      <*> v .: "modelid"
      <*> v .: "swversion"
      <*> pure () 
    
    where
      parseLightState v  = LightState 
        <$> v .: "on"
        <*> v .: "bri"
        <*> v .: "alert"
        <*> v .: "reachable"
        <*> v .:? "ct"
        <*> parseColorState v
        
      parseColorState v = do
        h <- v .:? "hue"
        sat <- v .:? "sat"
        xyPair <- v .:? "xy"
        eff <- v .:? "effect"
        colormode <- v .:? "colormode"
        pure $ ColorState <$> h <*> sat <*> xyPair <*> eff <*> colormode


instance ToJSON LightName where
  toJSON (LightName name) = object [("name", String name)]

instance FromJSON LightType where
  parseJSON = withText "Light type" $ \case
    "Dimmable light" -> pure DimmableLight
    "Color light" -> pure ColorLight
    "Color temperature light" -> pure ColorTemperatureLight
    "Extended color light" -> pure ExtendedColorLight
    x -> fail $ "Unknown light type: " ++ Text.unpack x     

instance FromJSON ColorMode where
  parseJSON = withText "Light colormode" $ \case 
    "hs" -> pure HueSaturationMode
    "xy" -> pure XYMode
    "ct" -> pure ColorTemperatureMode
    x -> fail $ "Unknown colormode: " ++ Text.unpack x

instance FromJSON ScanResult where
  parseJSON = withObject "Scan result object" $ \v -> do
    lastScanTime <- v .: "lastscan"
    lightMap <- parseJSON $ Object $ HashMap.delete "lastscan" v
    new <- Map.toList <$> traverse parseName lightMap
    pure $ ScanResult new lastScanTime
    where 
      parseName = withObject "Light name object" (\v' -> LightName <$> v' .: "name")

instance FromJSON ScanStatus where
  parseJSON = withText "light scan status" $ \case
    "none" -> pure NoScanResult
    "active" -> pure ScanActive
    time -> LastScan <$> parseJSON (String time) 

-- | LightID can be used as part of an API path.
instance ToPathSegment LightID where
  toSegment (LightID lId) = toSegment $ Text.pack $ show lId


instance FromJSON Alert where
  parseJSON = withText "Light alert status" $ \case 
    "none" -> pure NoAlert
    "select" -> pure OneCycle
    "lselect" -> pure MultipleCycles
    x -> fail $ "Unknown alert status: " ++ Text.unpack x

instance ToJSON Alert where
  toJSON NoAlert = String "none"
  toJSON OneCycle = String "select"
  toJSON MultipleCycles = String "lselect"
  
instance FromJSON Effect where
  parseJSON = withText "Light effect" $ \case 
    "none" -> pure NoEffect
    "colorloop" -> pure ColorLoop
    x -> fail $ "Unknown light effect: " ++ Text.unpack x
    
instance ToJSON Effect where
  toJSON NoEffect = String "none"
  toJSON ColorLoop = String "colorloop"

instance ToJSON SetLightState where
  toJSON (SetLightState components) = 
    object $ componentToTuple <$> Set.toList components 
    where
      componentToTuple (SetOn onState) = ("on", toJSON onState)
      componentToTuple (SetBrightness bri) = ("bri", toJSON bri)
      componentToTuple (IncBrightness bri) = ("bri_inc", toJSON bri)
      componentToTuple (SetHue h) = ("hue", toJSON h)
      componentToTuple (IncHue s) = ("hue_inc", toJSON s)
      componentToTuple (SetSaturation s) = ("sat", toJSON s)
      componentToTuple (IncSaturation s) = ("sat_inc", toJSON s)
      componentToTuple (SetXY xyPair) = ("xy", toJSON xyPair)
      componentToTuple (IncXY xyPair) = ("xy_inc", toJSON xyPair)
      componentToTuple (SetTemperature t) = ("ct", toJSON t)
      componentToTuple (IncTemperature s) = ("ct_inc", toJSON s)
      componentToTuple (SetAlert a) = ("alert", toJSON a)
      componentToTuple (SetEffect e) = ("effect", toJSON e)

  