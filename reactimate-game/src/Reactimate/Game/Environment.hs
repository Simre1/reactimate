module Reactimate.Game.Environment (GameEnv (..)) where

import SDL qualified

newtype GameEnv = GameEnv
  { window :: SDL.Window
  }
  deriving (Eq, Show)
