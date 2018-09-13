-- |
-- Module: Hue.Group
-- Copyright: (c) 2018 Thomas Smith
-- License: BSD3
-- Maintainer: Thomas Smith <tnsmith@live.nl>
-- Stability: experimental
--
-- Everything for the Hue Groups API.
module Hue.Group (
  -- * Fetching groups
  groups
, rooms
, lightGroups
, groupsWithoutLuminaires
, luminaires
, luminaireParts
, groupWithName
-- * Changing group properties
, renameGroup
, changeGroupMembers
, changeGroupClass
, changeGroup
, deleteGroup
-- * Common types
, Group
, groupName
, groupLights
, groupType
, groupState
, groupClass
, groupWillRecycle
, groupId
, GroupID
, NormalGroup(..)
, LuminaireGroup(..)
, GroupName(..)
, GroupState(..)
, GroupClass(..)
, GroupChange(..)
) where

import Hue.Internal.Group
