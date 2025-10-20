{-# LANGUAGE NoFieldSelectors #-}

module Reactimate.Game.Input
  ( inputEvents,
    keyboardState,
    mousePosition,
    absoluteMousePosition,
    mouseButtons,
    gameShouldQuit,

    -- * Input effect
    Input,
    runInput,
  )
where

import Linear (V2 (..))
import Reactimate
import Reactimate.Game.Graphics (Camera (..), Graphics, getWindowSize)
import SDL qualified

data Input s = Input
  { events :: Ref s [SDL.Event],
    getKeyboard :: Step s (SDL.Scancode -> Bool),
    getMousePosition :: Step s (V2 Int),
    getMouseButtons :: Step s (SDL.MouseButton -> Bool),
    graphics :: Graphics s
  }

-- | Handle SDL events as they happen. This can be useful if you want to catch events which happen in between simulations.
inputEvents :: (Input :> es) => Signal es () (Event SDL.Event)
inputEvents = arrStep $ \(Handle (Input {events})) _ -> Event <$> readRef events

-- | Get the current keyboard state.
keyboardState :: (Input :> es) => Signal es () (SDL.Scancode -> Bool)
keyboardState = arrStep $ \(Handle (Input {getKeyboard})) _ -> getKeyboard

-- | Get the position of the mouse relative to the camera.
mousePosition :: (Input :> es) => Signal es Camera (V2 Int)
mousePosition = arrStep $ \(Handle Input {getMousePosition, graphics}) camera -> do
  realMousePosition <- getMousePosition
  windowSize <- getWindowSize graphics
  let (V2 x y) = quot <$> (realMousePosition * camera.viewport) <*> windowSize
      (V2 _ vy) = camera.viewport
  pure $ V2 x (vy - y)

-- | Get the absolute position of the mouse.
absoluteMousePosition :: (Input :> es) => Signal es () (V2 Int)
absoluteMousePosition = arrStep $ \(Handle Input {getMousePosition}) _ -> getMousePosition

-- | Get the current mouse button state
mouseButtons :: (Input :> es) => Signal es () (SDL.MouseButton -> Bool)
mouseButtons = arrStep $ \(Handle Input {getMouseButtons}) _ -> getMouseButtons

-- | Checks if a SDL Quit event was triggered
gameShouldQuit :: (Input :> es) => Signal es () Bool
gameShouldQuit =
  (foldl' (||) False . fmap ((== SDL.QuitEvent) . SDL.eventPayload)) <$> inputEvents

runInput :: (Graphics :> es, IOE :> es) => Signal (Input : es) a b -> Signal es a b
runInput signal = makeSignal $ do
  eventsRef <- newRef []
  IOE lift <- getHandle
  graphicsRef <- getHandle

  let input =
        Input
          eventsRef
          (lift SDL.getKeyboardState)
          (fmap fromIntegral . SDL.unP <$> lift SDL.getAbsoluteMouseLocation)
          (lift SDL.getMouseButtons)
          graphicsRef

  f <- runHandle input (unSignal signal)

  pure $ \a -> do
    events <- lift SDL.pollEvents
    writeRef eventsRef events
    f a
