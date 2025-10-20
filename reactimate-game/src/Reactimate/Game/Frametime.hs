module Reactimate.Game.Frametime where

import Control.Concurrent
import Reactimate
import Reactimate.Game.Graphics
import Reactimate.Time (makeTimeHandle)
import SDL qualified

-- | Waits at the end of each iteration to reach the given target FPS. It also runs the `Time` effect with `SDL.time`
runFrametime :: (Graphics :> es, IOE :> es) => Int -> Signal (Time : es) a b -> Signal es a b
runFrametime fps signal = makeSignal $ do
  !buildTime <- SDL.time
  let timeInFuture = buildTime + 100000000
  dTimeRef <- newRef 0
  cTimeRef <- newRef timeInFuture
  wantedTimeRef <- newRef timeInFuture
  let time = makeTimeHandle dTimeRef cTimeRef
  IOE lift <- getHandle

  f <- runHandle time (unSignal signal)

  pure $ \a -> do
    oldTime <- readRef cTimeRef
    !newTime <- lift $ SDL.time
    let !dTime = max 0 $ newTime - oldTime
    writeRef cTimeRef newTime
    writeRef dTimeRef dTime

    b <- f a

    wantedTime <- readRef wantedTimeRef
    lift $ threadDelay $ round $ min frameTime (max 0 $ wantedTime - newTime) * 10 ^ (6 :: Int)

    writeRef wantedTimeRef (max newTime $ wantedTime + frameTime)

    pure b
  where
    frameTime = 1 / fromIntegral fps
