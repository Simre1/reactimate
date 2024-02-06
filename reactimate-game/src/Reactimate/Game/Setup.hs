module Reactimate.Game.Setup where

import Data.Text (Text)
import GHC.Generics (Generic)
import Reactimate
import Reactimate.Game.Environment
import SDL qualified
import Data.Functor (($>))

data GameConfig = GameConfig
  { name :: !Text,
    window :: !SDL.WindowConfig
  }
  deriving (Eq, Show, Generic)

data SDLEnv r = SDLEnv
  { env :: !r,
    window :: !SDL.Window
  }

-- | Initializes the game context and provides you with a `Window` which can be used for rendering.
setupGame :: GameConfig -> (GameEnv -> Signal a b) -> Signal a b
setupGame config signal = allocateResource
  ( \fin -> do
      SDL.initializeAll
      window <- SDL.createWindow config.name config.window
      addFinalizer fin $ SDL.destroyWindow window
      pure window
  )
  $ \window ->
    arrIO (SDL.pumpEvents $>) >>> signal (GameEnv window)
