-- |
-- Module: Hue.Sensor
-- Copyright: (c) 2021 Grzegorz Dziadkiewicz
-- License: BSD3
-- Maintainer: Grzegorz Dziadkiewicz <grzegorz@dziadkiewicz.com>
-- Stability: experimental
--
-- Everything for the Hue Sensors API.
module Hue.Sensor (
  -- * Fetching sensors
  sensors
, sensorWithName
-- * Renaming / deleting sensors
, renameSensor
, deleteSensor
-- * Searching for new sensors
, searchNewSensors
, newSensors
, ScanResult(..)
, ScanStatus(..)
-- * Common types
, Sensor
, sensorName
, sensorType
, modelid
, swversion
, SensorType(..)
, SensorName(..)
, SensorID
, sensorIDInt
) where

import Hue.Internal.Sensor
