module Reactimate.Game.Setup where

import GHC.Generics (Generic)
import Reactimate
import Reactimate.Environment (allocateResource)
import Data.Text (Text)
import Linear (V2)
import Reactimate.Signal (addFinalizer)
import qualified SDL

data GameConfig = GameConfig
  { name :: !Text,
    windowSize :: !(V2 Int)
  }
  deriving (Eq, Show, Generic)

data SDLEnv r = SDLEnv {
  env :: !r,
  window :: !SDL.Window
}

-- | Initializes the game context and provides you with a `Window` which can be used for rendering. 
setupGame :: GameConfig -> (Event SDL.Event -> Signal () -> Signal a b
setupGame config updateEnv = allocateResource $ \fin r1 -> do
  SDL.initializeAll
  window <- SDL.createWindow
      config.name
      SDL.defaultWindow
        { SDL.windowInitialSize = fromIntegral <$> config.windowSize
        }
  addFinalizer fin $ SDL.destroyWindow window
  pure $ updateEnv r1 window
