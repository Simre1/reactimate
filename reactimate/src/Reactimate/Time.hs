module Reactimate.Time (Time(..), withTime, withFixedTime, deltaTime, currentTime, integrate) where

import Control.Arrow
import Control.Category (Category (id))
import Data.IORef (IORef, newIORef, readIORef)
import GHC.Clock (getMonotonicTime)
import GHC.IORef (writeIORef)
import GHC.Records
import Reactimate.Basic (arrIO)
import Reactimate.Signal
import Reactimate.Stateful (feedbackState)
import Prelude hiding (id)

-- | Tracks `currentTime` and `deltaTime`.
data Time = Time
  { dTime :: {-# UNPACK #-} !(IORef Double),
    cTime :: {-# UNPACK #-} !(IORef Double)
  }
  deriving (Eq)

-- | Gives you a 'Time' based on real data.
-- 'Time' contains the total runtime as well as a delta time which is the time between executions of signal functions.
withTime :: (Time -> Signal a b) -> Signal a b
withTime signal = Signal $ \fin -> do
  !buildTime <- getMonotonicTime
  dTimeRef <- newIORef 0
  timeRef <- newIORef buildTime
  let time = Time dTimeRef timeRef
  step <- unSignal (signal time) fin
  pure $ \a -> do
    oldTime <- readIORef timeRef
    !newTime <- getMonotonicTime
    let !dTime = newTime - oldTime
    writeIORef timeRef newTime
    writeIORef dTimeRef dTime

    step a
{-# INLINE withTime #-}

-- | Gives you a 'Time' based on a fixed delta. Each execution will have the same delta and the runtime
-- will be @deltaTime * runNumber@.
withFixedTime :: Double -> (Time -> Signal a b) -> Signal a b
withFixedTime fixedDelta signal = Signal $ \fin -> do
  dTimeRef <- newIORef fixedDelta
  timeRef <- newIORef 0
  let time = Time dTimeRef timeRef
  step <- unSignal (signal time) fin 
  pure $ \a -> do
    oldTime <- readIORef timeRef
    let !newTime = oldTime + fixedDelta
    writeIORef timeRef newTime
    step a
{-# INLINE withFixedTime #-}

-- | Get the duration of the last execution which can be used to do some interpolation based on time.
deltaTime :: Time -> Signal x Double
deltaTime time = arrIO (const $ readIORef $ getField @"dTime" time)
{-# INLINE deltaTime #-}

-- | Get the total runtime.
currentTime :: Time -> Signal x Double
currentTime time = arrIO (const $ readIORef $ getField @"cTime" time)
{-# INLINE currentTime #-}

-- | Integrate a value based on 'deltaTime'. The first parameter is the needed scalar multiplication for `a`.
integrate :: (Num a) => Time -> (Double -> a -> a) -> Signal a a
integrate time scale =
  deltaTime time &&& id
    >>> feedbackState
      0
      ( arr $ \((dt, value), !previous) ->
          let !next = previous + scale dt value in (next, next)
      )
{-# INLINE integrate #-}
