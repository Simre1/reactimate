module Reactimate.Game
  ( runGame,
    module Graphics,
    module Shapes,
    module Projection2D,
    module Input,
    module Frametime,
    module Assets,
    V2 (..),
    V4 (..),
  )
where

import Data.Text
import Linear
import Reactimate
import Reactimate.Game.Assets as Assets
import Reactimate.Game.Frametime as Frametime
import Reactimate.Game.Graphics as Graphics
import Reactimate.Game.Input as Input
import Reactimate.Game.Projection2D as Projection2D
import Reactimate.Game.Shapes as Shapes
import SDL qualified

-- | Convenience function which runs multiple game effects at once.
runGame ::
  (IOE :> es) =>
  -- | Window name
  Text ->
  -- | SDL configuration for the window
  SDL.WindowConfig ->
  -- | Desired FPS
  Int ->
  Signal (Input : Time : Graphics : es) a b ->
  Signal es a b
runGame windowName windowConfig fps =
  runGraphics windowName windowConfig
    . runFrametime fps
    . runInput
