module Reactimate.Game.Input
  ( inputEvents,
    keyboardState,
    mousePosition,
    mouseButtons
  )
where

import Linear (V2 (..))
import Reactimate
import Reactimate.Game.Environment (GameEnv (..))
import Reactimate.Game.Graphics (Camera (..))
import SDL qualified

-- | Handle SDL events as they happen. This can be useful if you want to catch events which happen in between simulations. 
inputEvents :: GameEnv -> Event SDL.Event
inputEvents _ = callback $ \fin fire -> do
  eventWatch <- SDL.addEventWatch fire
  addFinalizer fin $ SDL.delEventWatch eventWatch

-- | Get the current keyboard state.
keyboardState :: GameEnv -> Behavior (SDL.Scancode -> Bool)
keyboardState _ = makeBehavior $ arrIO $ \_ -> do
  SDL.getKeyboardState

-- | Get the position of the mouse relative to the camera.
mousePosition :: GameEnv -> Signal Camera (V2 Int)
mousePosition gameEnv = arrIO $ \camera -> do
  windowSize <- fmap fromIntegral <$> SDL.get (SDL.windowSize gameEnv.window)
  realMousePosition <- fmap fromIntegral . SDL.unP <$> SDL.getAbsoluteMouseLocation
  let (V2 x y) = quot <$> (realMousePosition * camera.viewport) <*> windowSize
      (V2 _ vy) = camera.viewport
  pure $ V2 x (vy - y)

-- | Get the current mouse button state
mouseButtons :: GameEnv -> Behavior (SDL.MouseButton -> Bool)
mouseButtons _ = makeBehavior $ arrIO $ const $ do
  SDL.getMouseButtons
