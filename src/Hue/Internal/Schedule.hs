
module Hue.Internal.Schedule where

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
-- Schedules
-- ----------------------------------------------------------------

-- | Get all schedules.
schedules :: Hue [Schedule]
schedules = unLights <$> get parseResult (api /~ credentials /: "schedules")

-- | Get schedule by id.
scheduleByID :: ScheduleID -> Hue [Schedule]
scheduleByID scheduleId = unLights <$> get parseResult (api /~ credentials /: "schedules" /~ scheduleId)

data ScheduleCreationResult =
     NewScheduleId ScheduleID
   | ScheduleListIsFull -- ^ Get the current display name of a bulb.
   -- We can propably remove those two 
   | ScheduleTimezoneInvalid -- ^ Get the current display name of a bulb.
   | TimeAndLocalTimeSetAtTheSameTime -- ^ Get the current display name of a bulb.

-- | Create new schedule.
createSchedule :: NewSchedule -> Hue ScheduleCreationResult
createSchedule = unLights <$> post parseResult (api /~ credentials /: "schedules")

newtype ScheduleChanges = GroupChanges (Set ScheduleChange)
data ScheduleChange =
    ChangeName ScheduleName
  | ChangeDescription ScheduleDescription
  deriving Show

-- | Deletes the specified schedule from the bridge.
changeSchedule :: Set ScheduleChange -> Schedule -> Hue ()
changeSchedule changes Schedule{..} = put
  (body $ ScheduleChanges changes)
  ignoreResult
  (api /~ credentials /: "schedules" /~ scheduleId)

-- | Deletes the specified schedule from the bridge.
deleteSchedule :: Schedule -> Hue ()
deleteSchedule Schedule{..} = delete ignoreResult $ api /~ credentials /: "schedules" /~ scheduleId

-- ----------------------------------------------------------------
-- Schedule datatypes
-- ----------------------------------------------------------------

-- | Represents a single schedule.
data Schedule  = Schedule {
    scheduleName :: ScheduleName -- ^ Get the name of the schedule.
  , scheduleDescription :: ScheduleDescription -- ^ Get the current display name of a bulb.
  , scheduleCommand :: ScheduleCommand
  , scheduleLocalTime :: Text
  , scheduleTime :: Text
  , scheduleCreated :: Text
  , scheduleStatus :: ScheduleStatus
  , scheduleAutodelete :: Bool
  , scheduleRecycle :: Bool
  , scheduleId :: ScheduleID -- ^ Get the unique identifier of a light.
} deriving Show

data ScheduleCommand = ScheduleCommand Text deriving Show
data ScheduleStatus = Enabled | Disabled deriving Show
--  "1": {
--         "name": "Dimmer Switch 2.reset",
--         "description": "Resets dimmer switch scene cycler",
--         "command": {
--             "address": "/api/XXXXXXXXXXXXXXXXXXXXXXXXXX/sensors/3/state",
--             "body": {
--                 "status": 0
--             },
--             "method": "PUT"
--         },
--         "localtime": "PT00:00:10",
--         "time": "PT00:00:10",
--         "created": "2020-11-14T15:12:10",
--         "status": "disabled",
--         "autodelete": false,
--         "starttime": "2020-11-14T15:12:10",
--         "recycle": true
--     },
type NewSchedule = Schedule

-- | Lights are equal iff their 'ligthId's are equal.
instance Eq Schedule where
  l == l' = scheduleId l == scheduleId l'

instance Ord Schedule where
  compare l l' = compare (scheduleId l) (scheduleId l')

-- | The display name of a light.
newtype ScheduleName = ScheduleName {
  scheduleNameText :: Text -- ^ Get the textual representation of a light name for human consumption.
} deriving (Eq, Ord, Show, FromJSON)

newtype ScheduleDescription = ScheduleDescription {
  scheduleDescriptionText :: Text -- ^ Get the textual representation of a light name for human consumption.
} deriving (Eq, Ord, Show, FromJSON)

-- | Used to identify an individual light with the bridge.
newtype ScheduleID = ScheduleID {
  scheduleIdInt :: Int -- ^ Get the integer value representing a LigthID.
} deriving (Eq, Ord, Show, FromJSONKey, ToJSON)