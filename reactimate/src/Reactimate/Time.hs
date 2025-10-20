{-# LANGUAGE TypeFamilies #-}

module Reactimate.Time (Time, withTime, withFixedTime, interposeFixedTime, makeTimeHandle, deltaTime, currentTime, integrate) where

import Control.Arrow
import Control.Category (Category (id))
import GHC.Clock (getMonotonicTime)
import Reactimate.Basic (actionStep)
import Reactimate.Handles
import Reactimate.Signal
import Reactimate.Stateful (feedback)
import Prelude hiding (id)

-- | The 'Time' effect tracks time within the simulation.
-- It contains the total runtime as well as a delta time which is the time between executions of signal functions.
-- You can either run it with real time ('withTime') or with fixed time steps ('withFixedTime').
data Time s = Time
  { deltaTimeRef :: Ref s Double,
    currentTimeRef :: Ref s Double
  }

-- | Create a time handle from references. You need to update the references yourself, so you can implement any time progression.
makeTimeHandle ::
  -- | Delta time
  Ref s Double ->
  -- | Current time relative to some fixed point in the past (e.g. relative to program startup)
  Ref s Double ->
  Time s
makeTimeHandle = Time

-- | Gives you a 'Time' based on real data.
-- The total time relies on GHC time tracking, so time 0 is when your whole program starts and not when you run `withTime`.
withTime :: (IOE :> es) => Signal (Time : es) a b -> Signal es a b
withTime signal = makeSignal $ do
  dTimeRef <- newRef 0
  timeRef <- newRef (-1)
  let time = Time dTimeRef timeRef

  step <- runHandle time $ unSignal signal
  IOE liftIO <- getHandle

  pure $ \a -> do
    oldTime <- readRef timeRef
    !newTime <- liftIO getMonotonicTime
    let !dTime = if oldTime >= 0 then newTime - oldTime else 0
    writeRef timeRef newTime
    writeRef dTimeRef dTime

    (step a)

-- | Gives you a 'Time' based on a fixed delta. Each execution will have the same delta and the runtime
-- will be @deltaTime * runNumber@.
withFixedTime :: Double -> Signal (Time : es) a b -> Signal es a b
withFixedTime fixedDelta signal = makeSignal $ do
  timeRef <- newRef (-fixedDelta)
  dTimeRef <- newRef fixedDelta

  let time = Time dTimeRef timeRef

  step <- runHandle time $ unSignal signal
  pure $ \a -> do
    oldTime <- readRef timeRef
    let !newTime = oldTime + fixedDelta
    writeRef timeRef newTime
    (step a)

-- | Interposes the `Time` effect such that the given signal believes it runs on fixed time steps,
-- no matter what happens outside.
interposeFixedTime :: (Time :> es) => Double -> Signal es a b -> Signal es a b
interposeFixedTime fixedDelta signal = makeSignal $ do
  timeRef <- newRef 0
  deltaTimeRef <- newRef fixedDelta
  let time = Time deltaTimeRef timeRef
  step <- replaceHandle time $ unSignal signal
  pure $ \a -> do
    b <- step a
    oldTime <- readRef timeRef
    let !newTime = oldTime + fixedDelta
    writeRef timeRef newTime
    pure b

-- | Get the duration of the last execution which can be used to do some interpolation based on time.
deltaTime :: (Time :> es) => Signal es x Double
deltaTime = actionStep (\(Handle Time {deltaTimeRef}) -> readRef deltaTimeRef)
{-# INLINE deltaTime #-}

-- | Get the total runtime.
currentTime :: (Time :> es) => Signal es x Double
currentTime = actionStep (\(Handle Time {currentTimeRef}) -> readRef currentTimeRef)
{-# INLINE currentTime #-}

-- | Integrate a value based on 'deltaTime'. The first parameter is the needed scalar multiplication for `a`.
integrate :: (Time :> es, Num a) => (Double -> a -> a) -> Signal es a a
integrate scale =
  deltaTime &&& id
    >>> feedback
      0
      ( arr $ \((dt, value), !previous) ->
          let !next = previous + scale dt value in next
      )
{-# INLINE integrate #-}
