module Reactimate.Game.Setup where

import Data.Text (Text)
import GHC.Generics (Generic)
import Reactimate
import Reactimate.Game.Environment
import SDL qualified
import Data.Functor (($>))
import Reactimate.Game.Assets (makeAssets)

data GameConfig = GameConfig
  { name :: !Text,
    window :: !SDL.WindowConfig
  }
  deriving (Eq, Show, Generic)

-- | Initializes the game context and provides you with a `Window` which can be used for rendering.
setupGame :: GameConfig -> (GameEnv -> Signal a b) -> Signal a b
setupGame config signal = allocateResource
  ( \fin -> do
      SDL.initializeAll
      window <- SDL.createWindow config.name config.window
      addFinalizer fin $ SDL.destroyWindow window

      renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
      addFinalizer fin $ SDL.destroyRenderer renderer

      GameEnv window renderer <$> makeAssets
  )
  $ \gameEnv ->
    arrIO (SDL.pumpEvents $>) >>> signal gameEnv
