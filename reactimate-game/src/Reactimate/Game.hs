{-# LANGUAGE OverloadedStrings #-}

module Reactimate.Game (module Colour, V2(..), module Setup, module Graphics) where

import Reactimate.Game.Graphics as Graphics
import Reactimate.Game.Setup as Setup
import Reactimate.Game.Window as Window
import Linear.V2
import Data.Colour as Colour
import Data.Colour.Names as Colour

import Control.Concurrent (threadDelay)
