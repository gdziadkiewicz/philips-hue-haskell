{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
-- | 
-- Module: Hue.Internal.Light 
-- Copyright: (c) 2017 Thomas Smith
-- License: BSD3
-- Maintainer: Thomas Smith <tnsmith@live.nl>
-- Stability: experimental
--
-- Everything for the Hue Lights API.
module Hue.Light (
  Hue.Internal.Light.Alert(..)
, Hue.Internal.Light.Effect(..)
, Hue.Internal.Light.SetLightState
, module Hue.Light
) where 

import Prelude hiding (fail)

import Data.Aeson
import Data.Time (LocalTime)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.HashMap.Lazy as HashMap
import Data.Word
import Data.Foldable

import Control.Monad.Fail

import Hue.Internal
import Hue.Internal.Endpoint
import Hue.Internal.Light

lightWithName :: Text -> Hue (Maybe (Light 'WithID))
lightWithName n = find ((LightName n ==) . lightName) <$> fetchLights
  
fetchLights :: Hue [Light 'WithID]
fetchLights = fmap (uncurry setLightID) <$> Map.toList <$> request lights 
  where 
    setLightID i l = l { lightId = i }

-- ----------------------------------------------------------------
-- Endpoints
--   for light manipulation
-- ----------------------------------------------------------------

lights :: Endpoint 'GET () (Map LightID (Light 'WithoutID))
lights = root /: "api" /~ credentials /: "lights"

setLight :: Light 'WithID -> Endpoint 'PUT SetLightState ()
setLight Light{..} = root /: "api" /~ credentials /: "lights" /~ lightId /: "state"

searchNewLights ::  Endpoint 'POST () ()
searchNewLights = root /: "api" /~ credentials /: "lights"

newLights :: Endpoint 'GET () ScanResult
newLights = root /: "api" /~ credentials /: "lights" /: "new"

renameLight :: Light 'WithID -> Endpoint 'PUT LightName ()
renameLight Light{..} = root /: "api" /~ credentials /: "lights" /~ lightId

deleteLight :: Light 'WithID -> Endpoint 'DELETE () ()
deleteLight Light{..} = root /: "api" /~ credentials /: "lights" /~ lightId

-- ----------------------------------------------------------------
-- Light datatypes
-- ----------------------------------------------------------------

data KnownID = 
    WithID
  | WithoutID

type family LightIDType a where
  LightIDType 'WithID = LightID
  LightIDType 'WithoutID = ()

data Light (a :: KnownID) = Light
  { lightState :: LightState
  , lightName :: LightName
  , lightType :: LightType
  , modelid :: Text
  , swversion :: Text
  , lightId :: LightIDType a
  }

deriving instance Show (Light 'WithID)
deriving instance Show (Light 'WithoutID)

newtype LightName = LightName Text deriving (Eq, Ord, Show, FromJSON)
newtype LightID = LightID Int deriving (Eq, Ord, Show, FromJSONKey)

data LightType =
    DimmableLight
  | ColorLight
  | ColorTemperatureLight
  | ExtendedColorLight
  deriving (Eq, Show)

data LightState = LightState {
    isOn :: Bool
  , lightBrightness :: Word8
  , alertState :: Alert
  , isReachable :: Bool
  , colorTemperature :: Maybe Word16
  , colorState :: Maybe ColorState
} deriving (Show)

data ColorState = ColorState {
    colorHue :: Word16
  , colorSaturation :: Word8
  , colorXY :: (Float, Float)
  , colorEffect :: Effect
  , colorMode :: ColorMode
} deriving (Eq, Show)

data ColorMode =
    HueSaturationMode
  | XYMode
  | ColorTemperatureMode
  deriving (Eq, Ord, Show)

data ScanResult = ScanResult {
    foundLights :: [(LightID, LightName)] 
  , lastScan :: ScanStatus
} deriving (Eq, Show)

data ScanStatus = 
    NoScanResult
  | ScanActive
  | LastScan LocalTime
  deriving (Eq, Show)

-- ----------------------------------------------------------------
-- Updating Lights
--   Utility functions and types
-- ----------------------------------------------------------------

on :: SetLightState
on = mkSetLightState $ SetOn True

off :: SetLightState
off = mkSetLightState $ SetOn False

toggle :: LightState -> SetLightState
toggle LightState{..} = mkSetLightState $ SetOn (not isOn)


brightness :: Word8 -> SetLightState
brightness = mkSetLightState . SetBrightness

alterBrightness :: LightState -> (Word8 -> Word8) -> SetLightState
alterBrightness current change = mkSetLightState $ SetBrightness $ change $ lightBrightness current

increaseBrightness :: Word8 -> SetLightState
increaseBrightness = mkSetLightState . IncBrightness . fromEnum

decreaseBrightness :: Word8 -> SetLightState
decreaseBrightness = mkSetLightState . IncBrightness . negate . fromEnum


setHue :: Word16 -> SetLightState
setHue = mkSetLightState . SetHue

alterHue :: ColorState -> (Word16 -> Word16) -> SetLightState
alterHue current change = mkSetLightState $ SetHue $ change $ colorHue current

increaseHue :: Word16 -> SetLightState
increaseHue = mkSetLightState . IncHue . fromEnum

decreaseHue :: Word16 -> SetLightState
decreaseHue = mkSetLightState . IncHue . negate . fromEnum


saturation :: Word8 -> SetLightState
saturation = mkSetLightState . SetSaturation

alterSaturation :: ColorState -> (Word8 -> Word8) -> SetLightState
alterSaturation current change = mkSetLightState $ SetSaturation $ change $ colorSaturation current

increaseSaturation :: Word8 -> SetLightState
increaseSaturation = mkSetLightState . IncSaturation . fromEnum

decreaseSaturation :: Word8 -> SetLightState
decreaseSaturation = mkSetLightState . IncSaturation . negate . fromEnum


xy :: (Float, Float) -> SetLightState
xy = mkSetLightState . SetXY

alterXY :: ColorState -> ((Float, Float) -> (Float, Float)) -> SetLightState
alterXY current change = mkSetLightState $ SetXY $ change $ colorXY current

increaseXY :: (Float, Float) -> SetLightState
increaseXY = mkSetLightState . IncXY

decreaseXY :: (Float, Float) -> SetLightState
decreaseXY (x,y) = mkSetLightState $ IncXY $ (negate x, negate y)


temperature :: Word16 -> SetLightState
temperature = mkSetLightState . SetTemperature

alterTemperature :: LightState -> (Word16 -> Word16) -> SetLightState
alterTemperature current change = case colorTemperature current of
  Just ct -> mkSetLightState $ SetTemperature $ change ct
  Nothing -> SetLightState Set.empty

increaseTemperature :: Word16 -> SetLightState
increaseTemperature = mkSetLightState . IncTemperature . fromEnum

decreaseTemperature :: Word16 -> SetLightState
decreaseTemperature = mkSetLightState . IncTemperature . negate . fromEnum


alert :: Alert -> SetLightState
alert = mkSetLightState . SetAlert

colorLoop :: SetLightState
colorLoop = mkSetLightState $ SetEffect ColorLoop

noEffect :: SetLightState
noEffect = mkSetLightState $ SetEffect NoEffect

alterEffect :: ColorState -> (Effect -> Effect) -> SetLightState
alterEffect current change = mkSetLightState $ SetEffect $ change $ colorEffect current

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

instance FromJSON ColorMode where
  parseJSON = withText "Light colormode" $ \case 
    "hs" -> pure HueSaturationMode
    "xy" -> pure XYMode
    "ct" -> pure ColorTemperatureMode
    x -> fail $ "Unknown colormode: " ++ Text.unpack x

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

instance FromJSON ScanResult where
  parseJSON = withObject "Scan result object" $ \v -> do
    lastScanTime <- v .: "lastscan"
    lightMap <- parseJSON $ Object $ HashMap.delete "lastscan" v
    newLights <- Map.toList <$> traverse parseName lightMap
    pure $ ScanResult newLights lastScanTime
    where 
      parseName = withObject "Light name object" (\v' -> LightName <$> v' .: "name")

instance FromJSON ScanStatus where
  parseJSON = withText "light scan status" $ \case
    "none" -> pure NoScanResult
    "active" -> pure ScanActive
    time -> LastScan <$> parseJSON (String time) 

instance ToEndpointSegment LightID where
  toSegment (LightID lId) = toSegment $ Text.pack $ show lId
