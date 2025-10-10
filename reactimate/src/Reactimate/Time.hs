{-# LANGUAGE TypeFamilies #-}

module Reactimate.Time (Time (..), withTime, withFixedTime, interposeFixedTime, deltaTime, currentTime, integrate) where

import Control.Arrow
import Control.Category (Category (id))
import Effectful
import Effectful.Dispatch.Dynamic (interpose_, interpret_, send)
import GHC.Clock (getMonotonicTime)
import Reactimate.Basic (arrEff)
import Reactimate.Signal
import Reactimate.Stateful (feedback)
import Prelude hiding (id)

-- | The 'Time' effect tells you the current time. You can either use real time with 'withTime' or fixed time steps with 'withFixedTime'.
data Time es a where
  GetDeltaTime :: Time es Double
  GetCurrentTime :: Time es Double

type instance DispatchOf Time = Dynamic

-- | Gives you a 'Time' based on real data.
-- 'Time' contains the total runtime as well as a delta time which is the time between executions of signal functions.
withTime :: (IOE :> es) => Signal (Time : es) a b -> Signal es a b
withTime signal = Signal $ withRef 0 $ \dTimeRef -> withRef (-1) $ \timeRef -> do
  step <- unSignal signal
  pure $ \a -> do
    oldTime <- readRef timeRef
    !newTime <- liftIO getMonotonicTime
    let !dTime = if oldTime >= 0 then newTime - oldTime else 0
    writeRef timeRef newTime
    writeRef dTimeRef dTime

    interpret_
      ( \time -> case time of
          GetDeltaTime -> readRef dTimeRef
          GetCurrentTime -> readRef timeRef
      )
      (step a)

-- | Gives you a 'Time' based on a fixed delta. Each execution will have the same delta and the runtime
-- will be @deltaTime * runNumber@.
withFixedTime :: Double -> Signal (Time : es) a b -> Signal es a b
withFixedTime fixedDelta signal = Signal $ withRef (-fixedDelta) $ \timeRef -> do
  step <- unSignal signal
  pure $ \a -> do
    oldTime <- readRef timeRef
    let !newTime = oldTime + fixedDelta
    writeRef timeRef newTime
    interpret_
      ( \time -> case time of
          GetDeltaTime -> pure fixedDelta
          GetCurrentTime -> readRef timeRef
      )
      (step a)

-- | Gives you a 'Time' based on a fixed delta. Each execution will have the same delta and the runtime
-- will be @deltaTime * runNumber@.
interposeFixedTime :: (Time :> es) => Double -> Signal es a b -> Signal es a b
interposeFixedTime fixedDelta signal = Signal $ withRef (-fixedDelta) $ \timeRef -> do
  step <- unSignal signal
  pure $ \a -> do
    oldTime <- readRef timeRef
    let !newTime = oldTime + fixedDelta
    writeRef timeRef newTime
    interpose_
      ( \time -> case time of
          GetDeltaTime -> pure fixedDelta
          GetCurrentTime -> readRef timeRef
      )
      (step a)

-- | Get the duration of the last execution which can be used to do some interpolation based on time.
deltaTime :: (Time :> es) => Signal es x Double
deltaTime = arrEff (\_ -> send GetDeltaTime)
{-# INLINE deltaTime #-}

-- | Get the total runtime.
currentTime :: (Time :> es) => Signal es x Double
currentTime = arrEff (\_ -> send GetCurrentTime)
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
