
module Hue.Examples where

import Data.Foldable
import Data.Maybe
import Data.Monoid

import Control.Monad
import Control.Monad.Reader
import Control.Concurrent (threadDelay)

import Hue
import Hue.Light

-- blinkAll :: Hue ()
-- blinkAll = forever $ do
--   allLights on
--   liftIO $ threadDelay 3000000
--   allLights off
--   liftIO $ threadDelay 3000000

-- renameTo name newName = do
--   Just l <- lightWithName name
--   renameLight l `request` LightName (newName)

-- doEffect name = do
--   Just l <- lightWithName name
--   setLight l `request` colorLoop 

-- doAlert name = do
--   Just l <- lightWithName name
--   setLight l `request` alert MultipleCycles 

-- colorShift name = do
--   Just l <- lightWithName name
--   forever $ do
--     setLight l `request` (on <> increaseHue 1000 <> increaseSaturation 32)
--     liftIO $ threadDelay 500000

-- upstairs :: Hue ()
-- upstairs = do
--   ls <- catMaybes <$> traverse lightWithName ["Ufo", "Slaapkamer plafond", "Hal boven"]
--   traverse_ (\l -> request (setLight l) (toggle $ lightState l)) ls 

-- turn :: SetLightState -> Light 'WithID -> Hue ()
-- turn newState light = request (setLight light) newState

-- allLights :: SetLightState -> Hue ()
-- allLights action = void $ fetchLights >>= traverse (turn action)
