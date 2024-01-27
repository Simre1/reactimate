{-# LANGUAGE OverloadedStrings #-}

module Reactimate.Game (module Colour, V2(..), module Setup, module Graphics) where

import Reactimate.Game.Graphics as Graphics
import Reactimate.Game.Setup as Setup
import Linear.V2
import Data.Colour as Colour
import Data.Colour.Names as Colour

import qualified SDL
import Control.Concurrent (threadDelay)

test :: IO ()
test = do
  SDL.initializeAll
  SDL.createWindow "test" SDL.defaultWindow
  threadDelay (10^6 * 5)
  pure ()
