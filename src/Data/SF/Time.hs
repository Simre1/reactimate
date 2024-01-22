module Data.SF.Time (Time, withTime, withFixedTime, deltaTime, currentTime, integrate) where

import Control.Arrow
import Control.Category (Category (id))
import Data.IORef (IORef, newIORef, readIORef)
import Data.SF.Combinators (feedback)
import Data.SF.Core
import GHC.Clock (getMonotonicTime)
import GHC.IORef (writeIORef)
import GHC.Records
import Prelude hiding (id)

data Time = Time
  { dTime :: {-# UNPACK #-} !(IORef Double),
    cTime :: {-# UNPACK #-} !(IORef Double)
  }
  deriving (Eq)

-- | Add a 'Time' to your environment based on real data.
-- 'Time' contains the total runtime as well as a delta time which is the time between executions of signal functions.
withTime :: (r1 -> Time -> r2) -> SF r2 a b -> SF r1 a b
withTime f (SF sf) = SF $ \r1 -> do
  !buildTime <- getMonotonicTime
  deltaTime <- newIORef 0
  time <- newIORef buildTime
  let r2 = f r1 (Time deltaTime time)
  step <- sf r2
  pure $ \a -> do
    oldTime <- readIORef time
    !newTime <- getMonotonicTime
    let !dTime = newTime - oldTime
    writeIORef time newTime
    writeIORef deltaTime dTime

    step a
{-# INLINE withTime #-}

-- | Add a 'Time' to your environment based on a fixed delta. Each execution will have the same delta and the runtime
-- will be deltaTime * runNumber.
withFixedTime :: Double -> (r1 -> Time -> r2) -> SF r2 a b -> SF r1 a b
withFixedTime fixedDelta f (SF sf) = SF $ \r1 -> do
  deltaTime <- newIORef fixedDelta
  time <- newIORef 0
  let r2 = f r1 (Time deltaTime time)
  step <- sf r2
  pure $ \a -> do
    oldTime <- readIORef time
    let !newTime = oldTime + fixedDelta
    writeIORef time newTime
    step a
{-# INLINE withFixedTime #-}

-- | Get the duration of the last execution which can be used to do some interpolation based on time.
--
-- This function needs a "time" record field within your environment.
deltaTime :: (HasField "time" r Time) => SF r x Double
deltaTime = useEnv const $ arrIO (readIORef . getField @"dTime" . getField @"time")
{-# INLINE deltaTime #-}

-- | Get the total runtime.
--
-- This function needs a "time" record field within your environment.
currentTime :: (HasField "time" r Time) => SF r x Double
currentTime = useEnv const $ arrIO (readIORef . getField @"cTime" . getField @"time")
{-# INLINE currentTime #-}

-- | Integrate a value based on 'deltaTime'. The first parameter is the needed scalar multiplication for `a`.
integrate :: (HasField "time" r Time, Num a) => (Double -> a -> a) -> SF r a a
integrate scale =
  deltaTime &&& id
    >>> feedback
      0
      ( arr $ \((dt, value), !previous) ->
          let !next = previous + scale dt value in (next, next)
      )
{-# INLINE integrate #-}
