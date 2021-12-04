-- |
-- Module: Hue.Internal.Sensor
-- Copyright: (c) 2021 Grzegorz Dziadkiewicz
-- License: BSD3
-- Maintainer: Grzegorz Dziadkiewicz <grzegorz@dziadkiewicz.com>
-- Stability: experimental
--
-- Types representing commands to change the state of a sensor.]
--
-- This is an internal module.
--
-- Please use "Hue.Sensor" instead.
module Hue.Internal.Sensor where

import Prelude hiding (fail)

import Data.Aeson
import Data.Time (LocalTime)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Read (decimal)

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Data.HashMap.Lazy as HashMap
import Data.Word
import Data.Function
import Data.Foldable

import Control.Applicative
import Control.Monad.Fail

import Hue.Internal
import Hue.Request

-- ----------------------------------------------------------------
-- Fetching sensors
-- ----------------------------------------------------------------

-- | Fetch all sensors.
sensors :: Hue [Sensor]
sensors = unSensors <$> get parseResult (api /~ credentials /: "sensors")

-- | Fetch a single sensor by it's name.
--
-- 'Nothing' when there exists no sensor with the given name.
sensorWithName :: Text -> Hue (Maybe Sensor)
sensorWithName n = find ((SensorName n ==) . sensorName) <$> sensors

allSensorsWithName :: Text -> Hue [Sensor]
allSensorsWithName n = filter ((SensorName n ==) . sensorName) <$> sensors

-- ----------------------------------------------------------------
-- Renaming / deleting sensors
-- ----------------------------------------------------------------

-- | Change the name of a sensor.
--
-- A sensor can have its name changed when it is in any state, 
-- unreachable/off etc.
renameSensor :: SensorName -> Sensor -> Hue ()
renameSensor name Sensor{..} = put (body name) ignoreResult $ api /~ credentials /: "sensors" /~ sensorId

-- | Request for deleting a sensor.
deleteSensor :: Sensor -> Hue ()
deleteSensor Sensor{..} = delete ignoreResult $ api /~ credentials /: "sensors" /~ sensorId


-- ----------------------------------------------------------------
-- Searching for unregistered sensors
-- ----------------------------------------------------------------

-- | Starts a search for new sensors.
--
-- The bridge will open the network for 40s. The overall search
-- might take longer since the configuration of (multiple) new
-- devices can take longer. If many devices are found the command
-- will have to be issued a second time after discovery time has
-- elapsed. If the command is received again during search the
-- search will continue for at least an additional 40s.
--
-- When the search has finished, new sensors will be available using
-- the 'newSensors' request. In addition, the new sensors will now
-- be available by calling 'sensors'.
searchNewSensors ::  Hue ()
searchNewSensors = post noBody ignoreResult $ api /~ credentials /: "sensors"

-- | Gets a 'ScanResult' since the last time 'searchNewLights'
-- was requested.
--
-- The returned scan results are reset when a new scan is started.
newSensors :: Hue ScanResult
newSensors = get parseResult $ api /~ credentials /: "sensors" /: "new"


-- ----------------------------------------------------------------
-- Sensor datatypes
-- ----------------------------------------------------------------

-- | Represents a single sensor.
data Sensor  = Sensor {
    sensorName :: SensorName -- ^ Get the current display name of a sensor.
  , sensorType :: SensorType -- ^ Get the name of type of a sensor.
  , modelid :: Text -- ^ Get the model identifier of a sensor.
  , swversion :: Text -- ^ Get the current software version of a sensor.
  , sensorManufacturer :: Text -- ^ Get the name of the manufacturer of the device.
  , sensorId :: SensorID -- ^ Get the unique identifier of a sensor.
} deriving Show

-- | Sensors are equal iff their 'sensorId's are equal.
instance Eq Sensor where
  l == l' = sensorId l == sensorId l'

instance Ord Sensor where
  compare l l' = compare (sensorId l) (sensorId l')

-- | The display name of a sensor.
newtype SensorName = SensorName {
  sensorNameText :: Text -- ^ Get the textual representation of a sensor name for human consumption.
} deriving (Eq, Ord, Show, FromJSON)

-- | Used to identify an individual sensor with the bridge.
newtype SensorID = SensorID {
  sensorIDInt :: Int -- ^ Get the integer value representing a SensorID.
} deriving (Eq, Ord, Show, FromJSONKey, ToJSON)

-- | The display name of a sensor.
newtype SensorType = SensorType {
  sensorTypeText :: Text -- ^ Get the textual representation of a sensor's type name.
} deriving (Eq, Ord, Show, FromJSON)

-- | Results obtained from the last time a scan for new sensors was
-- performed. See 'searchNewSensors' for more details.
data ScanResult = ScanResult {
    foundSensors :: [(SensorID, SensorName)] -- ^ All new sensors that have been found.
  , lastScan :: ScanStatus -- ^ The status of the last scan.
} deriving (Eq, Show)

-- | The state of a scan for new sensors.
data ScanStatus =
    NoScanResult -- ^ No scan has been perfomed recently.
  | ScanActive -- ^ A scan is currently active.
  | LastScan LocalTime -- ^ A scan was last perfomed at 'LocalTime'.
  deriving (Eq, Show)

-- ----------------------------------------------------------------
-- JSON Conversion instances
-- ----------------------------------------------------------------

-- | Intermediate form for a list of 'Light's.
-- Used to parse the Bridge response for 'lights'.
newtype Sensors = Sensors {
  unSensors :: [Sensor]
}

instance FromJSON Sensors where
  parseJSON o = do
    sensorMap :: [(SensorID, Value)] <- Map.toList <$> parseJSON o
    Sensors <$> traverse (\(sensorId, sensorJson) -> (&) sensorId <$> parseSensor sensorJson) sensorMap
    where
      parseSensor = withObject "Sensor object" $ \v -> do
        Sensor
          <$> v .: "name"
          <*> v .: "type"
          <*> v .: "modelid"
          <*> v .: "swversion"
          <*> v .: "manufacturername"

-- | Parse a SensorID from Text
instance FromJSON SensorID where
  parseJSON = withText "SensorID" $
    either
      (\err -> fail $ "Unable to parse sensor id: " ++ err)
      (pure . SensorID . fst)
    . decimal

instance ToJSON SensorName where
  toJSON (SensorName name) = object [("name", String name)]

instance FromJSON ScanResult where
  parseJSON = withObject "Scan result object" $ \v -> do
    lastScanTime <- v .: "lastscan"
    sensorMap <- parseJSON $ Object $ HashMap.delete "lastscan" v
    new <- Map.toList <$> traverse parseName sensorMap
    pure $ ScanResult new lastScanTime
    where
      parseName = withObject "Sensor name object" (\v' -> SensorName <$> v' .: "name")

instance FromJSON ScanStatus where
  parseJSON = withText "sensor scan status" $ \case
    "none" -> pure NoScanResult
    "active" -> pure ScanActive
    time -> LastScan <$> parseJSON (String time)

-- | SensorID can be used as part of an API path.
instance ToPathSegment SensorID where
  toSegment (SensorID lId) = toSegment $ Text.pack $ show lId
