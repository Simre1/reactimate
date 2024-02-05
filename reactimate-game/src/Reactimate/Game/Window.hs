module Reactimate.Game.Window (Window (..), createWindow, destroyWindow) where

import Data.Text (Text, unpack)
import Linear (V2 (..))
import SDL qualified

newtype Window = Window
  { windowResources :: SDL.Window
  }

createWindow :: V2 Int -> Text -> IO Window
createWindow windowSize name =
  Window
    <$> SDL.createWindow
      name
      SDL.defaultWindow
        { SDL.windowInitialSize = fromIntegral <$> windowSize
        }

destroyWindow :: Window -> IO ()
destroyWindow (Window window) = SDL.destroyWindow window
