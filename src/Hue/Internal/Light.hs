{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module: Hue.Internal.Light 
-- Copyright: (c) 2017 Thomas Smith
-- License: BSD3
-- Maintainer: Thomas Smith <tnsmith@live.nl>
-- Stability: experimental
--
-- Types representing commands to change the state of a light.
module Hue.Internal.Light where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word

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
