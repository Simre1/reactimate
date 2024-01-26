module Data.SF.Time (Time, withTime, withFixedTime, TimeEnv (..), deltaTime, currentTime, integrate) where

import Control.Arrow
import Control.Category (Category (id))
import Control.Concurrent (threadDelay)
import Data.IORef (IORef, newIORef, readIORef)
import Data.SF.Basic (arrIO)
import Data.SF.Core
import Data.SF.Environment (useEnv)
import Data.SF.Stateful (feedback)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTime, getMonotonicTimeNSec)
import GHC.IORef (writeIORef)
import GHC.Records
import Prelude hiding (id)

data Time = Time
  { dTime :: {-# UNPACK #-} !(IORef Double),
    cTime :: {-# UNPACK #-} !(IORef Double)
  }
  deriving (Eq)

newtype TimeEnv = TimeEnv {time :: Time} deriving (Eq)

-- | Add a 'Time' to your environment based on real data.
-- 'Time' contains the total runtime as well as a delta time which is the time between executions of signal functions.
withTime :: (r1 -> Time -> r2) -> SF r2 a b -> SF r1 a b
withTime f (SF sf) = SF $ \fin r1 -> do
  !buildTime <- getMonotonicTime
  dTimeRef <- newIORef 0
  timeRef <- newIORef buildTime
  let r2 = f r1 (Time dTimeRef timeRef)
  step <- sf fin r2
  pure $ \a -> do
    oldTime <- readIORef timeRef
    !newTime <- getMonotonicTime
    let !dTime = newTime - oldTime
    writeIORef timeRef newTime
    writeIORef dTimeRef dTime

    step a
{-# INLINE withTime #-}

-- | Add a 'Time' to your environment based on a fixed delta. Each execution will have the same delta and the runtime
-- will be deltaTime * runNumber.
withFixedTime :: Double -> (r1 -> Time -> r2) -> SF r2 a b -> SF r1 a b
withFixedTime fixedDelta f (SF sf) = SF $ \fin r1 -> do
  dTimeRef <- newIORef fixedDelta
  timeRef <- newIORef 0
  let r2 = f r1 (Time dTimeRef timeRef)
  step <- sf fin r2
  pure $ \a -> do
    oldTime <- readIORef timeRef
    let !newTime = oldTime + fixedDelta
    writeIORef timeRef newTime
    step a
{-# INLINE withFixedTime #-}

-- | Get the duration of the last execution which can be used to do some interpolation based on time.
--
-- This function needs a "time" record field within your environment.
deltaTime :: SF Time x Double
deltaTime = useEnv const $ arrIO (readIORef . getField @"dTime")
{-# INLINE deltaTime #-}

-- | Get the total runtime.
--
-- This function needs a "time" record field within your environment.
currentTime :: SF Time x Double
currentTime = useEnv const $ arrIO (readIORef . getField @"cTime")
{-# INLINE currentTime #-}

-- | Integrate a value based on 'deltaTime'. The first parameter is the needed scalar multiplication for `a`.
integrate :: (Num a) => (Double -> a -> a) -> SF Time a a
integrate scale =
  deltaTime &&& id
    >>> feedback
      0
      ( arr $ \((dt, value), !previous) ->
          let !next = previous + scale dt value in (next, next)
      )
{-# INLINE integrate #-}
