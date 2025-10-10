{-# LANGUAGE TypeFamilies #-}

module Reactimate.Time (Time (..), withTime, withFixedTime, interposeFixedTime, deltaTime, currentTime, integrate) where

import Control.Arrow
import Control.Category (Category (id))
import GHC.Clock (getMonotonicTime)
import Reactimate.Basic (actionStep, arrStep)
import Reactimate.Signal
import Reactimate.Stateful (feedback)
import Reactimate.Union
import Prelude hiding (id)

-- | The 'Time' effect tells you the current time. You can either use real time with 'withTime' or fixed time steps with 'withFixedTime'.
data Time s = Time
  { deltaTimeRef :: Ref s Double,
    currentTimeRef :: Ref s Double
  }

-- | Gives you a 'Time' based on real data.
-- 'Time' contains the total runtime as well as a delta time which is the time between executions of signal functions.
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

-- | Gives you a 'Time' based on a fixed delta. Each execution will have the same delta and the runtime
-- will be @deltaTime * runNumber@.
interposeFixedTime :: (Time :> es) => Double -> Signal es a b -> Signal es a b
interposeFixedTime fixedDelta signal = undefined

-- Signal $ withRef (-fixedDelta) $ \timeRef -> do
--   step <- unSignal signal
--   pure $ \a -> do
--     oldTime <- readRef timeRef
--     let !newTime = oldTime + fixedDelta
--     writeRef timeRef newTime
--     (step a)

-- | Get the duration of the last execution which can be used to do some interpolation based on time.
deltaTime :: (Time :> es) => Signal es x Double
deltaTime = actionStep (\Time {deltaTimeRef} -> readRef deltaTimeRef)
{-# INLINE deltaTime #-}

-- | Get the total runtime.
currentTime :: (Time :> es) => Signal es x Double
currentTime = actionStep (\Time {currentTimeRef} -> readRef currentTimeRef)
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
