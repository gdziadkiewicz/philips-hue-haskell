-- |
-- Module: Hue.Internal.Group
-- Copyright: (c) 2018 Thomas Smith
-- License: BSD3
-- Maintainer: Thomas Smith <tnsmith@live.nl>
-- Stability: experimental
--
-- Types representing commands to change the state of a light.
--
-- This is an internal module.
--
-- Please use "Hue.Group" instead.
module Hue.Internal.Group where

import Prelude hiding (fail)

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Maybe
import Data.Either
import Data.Foldable
import Data.Function
import Data.Tuple

import Control.Applicative
import Control.Monad.Fail

import Hue.Internal
import Hue.Request
import Hue.Light

-- | Fetch all groups known to the bridge.
--
-- This includes 'Room's, 'LightGroup's, 'Luminaire's and 'LuminairePart's.
-- Keep in mind that the Philips Hue app only shows groups of type 'Room'.
groups :: Hue [Group]
groups = unGroups <$> get parseResult (api /~ credentials /: "groups")

-- | Fetch only groups of type 'Room'.
--
-- Keep in mind that the Philips Hue app only shows groups of this type.
rooms :: Hue [Group]
rooms = filter ((Right Room ==) . groupType) <$> groups

-- | Fetch only groups of type 'LightGroup'.
lightGroups :: Hue [Group]
lightGroups = filter ((Right LightGroup ==) . groupType) <$> groups

-- | Fetch all groups except those that constitute a 'Luminaire' or 'LuminairePart'.
groupsWithoutLuminaires :: Hue [Group]
groupsWithoutLuminaires = filter (isRight . groupType) <$> groups

-- | Fetch only groups of type 'Luminaire'.
luminaires :: Hue [Group]
luminaires = filter ((Left Luminaire ==) . groupType) <$> groups

-- | Fetch only groups of type 'LuminairePart'.
luminaireParts :: Hue [Group]
luminaireParts = filter ((Left LuminairePart ==) . groupType) <$> groups

-- | Fetch a group by it's name.
--
-- 'Nothing' when there exists no group with the given name.
groupWithName :: Text -> Hue (Maybe Group)
groupWithName n = find ((GroupName n ==) . groupName) <$> groups


-- | Change the name of a group.
--
-- If the name is already taken a space and number will be appended by the bridge e.g. “Custom Group 1”.
renameGroup :: GroupName -> Group -> Hue ()
renameGroup = changeGroup . Set.singleton . ChangeName

-- | Alter which lights are part of a group.
--
-- Any lights in the given list will be added to the group.
-- Lights not in the list will be removed from the group.
changeGroupMembers :: [Light] -> Group -> Hue ()
changeGroupMembers = changeGroup . Set.singleton . ChangeGroupMembers . map lightId

-- | Alter the class of a group.
--
-- Changing the class also change the icon displayed for the group in the Philips Hue app.
changeGroupClass :: GroupClass -> Group -> Hue ()
changeGroupClass = changeGroup . Set.singleton . ChangeClass

-- | Send a set of group change commands to the bridge.
--
-- Useful for when you want to change multiple attributes at once.
-- Otherwise, just use 'renameGroup', 'changeGroupMembers' or 'changeGroupClass'.
changeGroup :: Set GroupChange -> Group -> Hue ()
changeGroup changes Group{..} = put
  (body $ GroupChanges changes)
  ignoreResult
  (api /~ credentials /: "groups" /~ groupId)

-- | Deletes the specified group from the bridge.
deleteGroup :: Group -> Hue ()
deleteGroup Group{..} = delete ignoreResult $ api /~ credentials /: "groups" /~ groupId


-- | Represents a group of lights.
data Group = Group {
    groupName :: GroupName -- ^ Get the current display name of a group.
  , groupLights :: [LightID] -- ^ Get all the lights that belong to this group.
  , groupType :: Either LuminaireGroup NormalGroup -- ^ Get the type that this group belongs to.
  , groupState :: GroupState -- ^ Get the state of the group.
  , groupClass :: Maybe GroupClass -- ^ Get the class of the group, only available when 'groupType' is 'Room'.
  , groupWillRecycle :: Bool
  , groupId :: GroupID -- ^ Get the unique identifier of a group.
} deriving Show

-- | Groups are iff their 'groupId's are equal
instance Eq Group where
  g == g' = groupId g == groupId g'

-- | Groups are ordered by their 'groupId'
instance Ord Group where
  compare g g' = compare (groupId g) (groupId g')


-- | The type of a group of lights.
--
-- Unlike 'LuminaireGroup' these groups can be manually created by a user.
data NormalGroup =
  -- | A group of lights that can be controlled together.
  --
  -- A light can be in multiple LightGroups.
  LightGroup

  -- | A group of lights that are physically located in the same place in the house.
  --
  -- Rooms behave similar to 'LightGroup's, except:
  --
  -- A room can be empty, a light is only allowed in one room and
  -- a room isn't automatically deleted when all lights in that room are deleted.
  --
  -- Keep in mind that the Philips Hue app only shows and creates groups of this type.
  | Room
  deriving (Show, Eq)

-- | Special type of light group for [multisource luminaires](https://developers.meethue.com/documentation/multisource-luminaires)
--
-- The bridge will pre-install these groups for ease of use.
-- This type of groups cannot be created manually.
-- A light can only be in a maximum of one luminaire group.
data LuminaireGroup =
  -- | Group that represents an entire multisource luminaire.
  Luminaire

  -- | A sub group of multisource 'Luminaire' lights.
  --
  -- These typically represent the separate sections of a multisource luminaire.
  | LuminairePart
  deriving (Show, Eq)

-- | The display name of a group.
newtype GroupName = GroupName {
  groupNameText :: Text -- ^ Get the textual representation of a group name for human consumption.
} deriving (Eq, Ord, Show, FromJSON)

-- | Used to identify a group of lights with the bridge.
newtype GroupID = GroupID {
  groupIDInt :: Int -- ^ Get the integer value representing a GroupID.
} deriving (Eq, Ord, Show, FromJSONKey)

-- | Indicates whether any or all of the lights in a group are currently turned on.
data GroupState =
    NoneOn -- ^ None of the lights in the group are on.
  | SomeOn -- ^ Some light in the group is on.
  | AllOn -- ^ The entire group is on.
  deriving (Eq, Show)

-- | Describes the catagory of a group.
--
-- Used in the Hue app to show an appropriate icon.
data GroupClass =
    LivingRoomGroup | KitchenGroup | DiningGroup | BedroomGroup
  | KidsBedroomGroup | BathroomGroup | NurseryGroup | RecreationGroup
  | OfficeGroup | GymGroup | HallwayGroup | ToiletGroup | FrontDoorGroup
  | GarageGroup | TerraceGroup | GardenGroup | DrivewayGroup
  | CarportGroup | OtherGroup
  deriving (Show, Eq, Ord)

newtype GroupChanges = GroupChanges (Set GroupChange)
data GroupChange =
    ChangeName GroupName
  | ChangeGroupMembers [LightID]
  | ChangeClass GroupClass
  deriving Show

-- | Two group changes are equal iff their constructor is equal.
--
-- This ensures that when we have Set GroupChange, each type of change can only occur once.
instance Eq GroupChange where
  (ChangeName _) == (ChangeName _) = True
  (ChangeGroupMembers _) == (ChangeGroupMembers _) = True
  (ChangeClass _) == (ChangeClass _) = True
  _ == _ = False

-- | Ordering is done only based on constructor name:
--
-- ChangeName < ChangeGroupMembers < ChangeClass
-- This ensures that when we have Set GroupChange, each type of change can only occur once.
instance Ord GroupChange where
  compare x y = compare (toInt x) (toInt y)
    where
      toInt :: GroupChange -> Int
      toInt (ChangeName _) = 0
      toInt (ChangeGroupMembers _) = 1
      toInt (ChangeClass _) = 2



-- ----------------------------------------------------------------
-- JSON Conversion instances
-- ----------------------------------------------------------------

newtype Groups = Groups {
  unGroups :: [Group]
}

instance FromJSON Groups where
  parseJSON o = do
    groupMap :: [(GroupID, Value)] <- Map.toList <$> parseJSON o
    Groups <$> traverse (uncurry parseGroupWithId) groupMap

    where
      parseGroupWithId groupId groupJson = (groupId &) <$> parseGroup groupJson

      parseGroup = withObject "Group object" $ \v ->
        Group
          <$> v .: "name"
          <*> v .: "lights"
          <*> ((Left <$> v .: "type") <|> (Right <$> v .: "type"))
          <*> v .: "state"
          <*> v .:! "class"
          <*> (fromMaybe False <$> v .:? "recycle")

instance FromJSON NormalGroup where
  parseJSON = withText "Normal group type" $ \case
    "Room" -> pure Room
    "LightGroup" -> pure LightGroup
    v -> fail $ "Unknown normal group type. Should be either 'Room' or 'LightGroup': " ++ Text.unpack v

instance FromJSON LuminaireGroup where
  parseJSON = withText "Luminaire group type" $ \case
    "Luminaire" -> pure Luminaire
    "Lightsource" -> pure LuminairePart
    v -> fail $ "Unknown luminaire group type. Should be either 'Luminaire' or 'Lightsource': " ++ Text.unpack v

instance FromJSON GroupState where
  parseJSON = withObject "Group state object" $ \o -> do
    anyOn <- o .: "any_on"
    allOn <- o .: "all_on"
    pure $ if allOn then AllOn else if anyOn then SomeOn else NoneOn

instance FromJSON GroupClass where
  parseJSON = withText "Group class" $ \t -> case lookup t groupClasses of
    Nothing -> fail $ "Unknown group class: " ++ Text.unpack t
    Just groupClass -> pure groupClass


groupClasses :: [(Text, GroupClass)]
groupClasses = [
    ("Living room", LivingRoomGroup)
  , ("Kitchen", KitchenGroup)
  , ("Dining", DiningGroup)
  , ("Bedroom", BedroomGroup)
  , ("Kids bedroom", KidsBedroomGroup)
  , ("Bathroom", BathroomGroup)
  , ("Nursery", NurseryGroup)
  , ("Recreation", RecreationGroup)
  , ("Office", OfficeGroup)
  , ("Gym", GymGroup)
  , ("Hallway", HallwayGroup)
  , ("Toilet", ToiletGroup)
  , ("Front door", FrontDoorGroup)
  , ("Garage", GarageGroup)
  , ("Terrace", TerraceGroup)
  , ("Garden", GardenGroup)
  , ("Driveway", DrivewayGroup)
  , ("Carport", CarportGroup)
  , ("Other", OtherGroup)
  ]

reverseGroupClasses :: [(GroupClass, Text)]
reverseGroupClasses = swap <$> groupClasses

-- | LightID can be used as part of an API path.
instance ToPathSegment GroupID where
  toSegment (GroupID gId) = toSegment $ Text.pack $ show gId


instance ToJSON GroupChanges where
  toJSON (GroupChanges changeSet) = object $ changeKeyValue <$> Set.toList changeSet
    where
      changeKeyValue (ChangeName (GroupName newName)) =
        ("name", toJSON newName)
      changeKeyValue (ChangeGroupMembers members) =
        ("lights", toJSON members)
      changeKeyValue (ChangeClass groupClass) =
        ("class", toJSON $ fromJust $ lookup groupClass reverseGroupClasses)

