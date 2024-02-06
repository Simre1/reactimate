module Reactimate.Game.Input (inputEvents, keyboardState, mousePosition, mouseButtons, module SDL.Event, module SDL.Input.Keyboard, module SDL.Input.Mouse) where

import Linear (V2 (..))
import Linear.V2 (V2)
import Reactimate
import Reactimate.Game.Environment (GameEnv (..))
import Reactimate.Game.Graphics (Camera (..))
import SDL qualified
import SDL.Event qualified
import SDL.Input.Keyboard qualified
import SDL.Input.Mouse qualified

inputEvents :: GameEnv -> Event SDL.Event
inputEvents _ = callback $ \fin fire -> do
  eventWatch <- SDL.addEventWatch fire
  addFinalizer fin $ SDL.delEventWatch eventWatch

keyboardState :: GameEnv -> Behavior (SDL.Scancode -> Bool)
keyboardState _ = makeBehavior $ arrIO $ \_ -> do
  SDL.getKeyboardState

mousePosition :: GameEnv -> Signal Camera (V2 Int)
mousePosition gameEnv = arrIO $ \camera -> do
  windowSize <- fmap fromIntegral <$> SDL.get (SDL.windowSize gameEnv.window)
  realMousePosition <- fmap fromIntegral . SDL.unP <$> SDL.getAbsoluteMouseLocation
  let (V2 x y) = quot <$> (realMousePosition * camera.viewport) <*> windowSize
      (V2 _ vy) = camera.viewport
  pure $ V2 x (vy - y)

mouseButtons :: GameEnv -> Behavior (SDL.MouseButton -> Bool)
mouseButtons _ = makeBehavior $ arrIO $ const $ do
  SDL.getMouseButtons
