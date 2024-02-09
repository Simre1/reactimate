module Reactimate.Game.Environment (GameEnv (..)) where

import Reactimate.Game.Assets (Assets)
import SDL qualified
import Reactimate.Time (Time)

data GameEnv = GameEnv
  { window :: !SDL.Window,
    renderer :: !SDL.Renderer,
    assets :: !Assets,
    time :: !Time
  }
