module Hue.Examples where

import Hue
import Hue.Light

import Control.Monad

x :: IO ()
x = runHue_ $ do
  Just l <- lightWithName "Ufo"
  void $ setLight on l