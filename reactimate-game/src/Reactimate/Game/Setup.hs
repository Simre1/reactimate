module Reactimate.Game.Setup
  ( setupGame,
    GameConfig (..),
    SDL.WindowConfig (..),
    SDL.defaultWindow,
  )
where

import Data.Functor (($>))
import Data.Text (Text)
import GHC.Generics (Generic)
import Reactimate
import Reactimate.Game.Assets (makeAssets)
import Reactimate.Game.Environment
import SDL qualified

data GameConfig = GameConfig
  { name :: !Text,
    window :: !SDL.WindowConfig
  }
  deriving (Eq, Show, Generic)

-- | Initializes the game environment and provides you the `GameEnv`. You will need this for rendering and grabbing input.
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
