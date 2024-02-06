module Reactimate.Sampling where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad (forever, when)
import Data.IORef
import Data.Sequence (Seq)
import Data.Sequence qualified as S
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Reactimate.Signal
import Reactimate.Time

-- | Resamples a Signal with the first argument as the specified @frameTime@ within the same thread. The resampled Signal will have a fixed time delta of @frameTime@.
-- The inputs and outputs are collected in a sequence.
resample :: Double -> Time -> (Time -> Signal (Seq a) b) -> Signal a (Seq b)
resample frameTime time signal = Signal $ \fin -> do
  f <- unSignal (withFixedTime frameTime signal) fin
  nextSampleTimeRef <- newIORef 0
  inputRef <- newIORef S.empty
  outputRef <- newIORef S.empty
  ct <- unSignal (currentTime time) fin

  pure $ \a -> do
    modifyIORef' inputRef (S.:|> a)

    initialSampleTime <- readIORef nextSampleTimeRef
    when (initialSampleTime <= 1) $ do
      -- no sampling has been done yet in this branch, so schedule the next sample to now
      ct () >>= writeIORef nextSampleTimeRef

    let go = do
          nextSampleTime <- readIORef nextSampleTimeRef
          now <- ct ()

          when (now >= nextSampleTime) $ do
            inputs <- readIORef inputRef
            writeIORef inputRef S.empty
            b <- f inputs
            modifyIORef' outputRef (S.:|> b)
            writeIORef nextSampleTimeRef (nextSampleTime + frameTime)
            go

    go

    bs <- readIORef outputRef
    writeIORef outputRef S.empty
    pure bs

-- \| Samples a signal function in another thread. You may want to limit sampling speed with `limitSampleRate`.
resampleInThread :: Signal (Seq a) b -> Signal a (Seq b)
resampleInThread signal = Signal $ \fin -> do
  f <- unSignal signal fin
  inputRef <- newIORef S.empty
  outputRef <- newIORef S.empty

  asyncRef <- async $ forever $ do
    inputs <- atomicModifyIORef' inputRef (S.empty,)
    output <- f inputs
    modifyIORef' outputRef (S.:|> output)

  addFinalizer fin (cancel asyncRef)

  pure $ \a -> do
    modifyIORef' inputRef (S.:|> a)
    atomicModifyIORef' outputRef (S.empty,)

-- | Limit the real world samples per second. The first argument is samples per second.
limitSampleRate :: Double -> Signal a b -> Signal a b
limitSampleRate frameTime' (Signal signal) = Signal $ \fin -> do
  f <- signal fin
  timeRef <- newIORef 0
  pure $ \a -> do
    b <- f a
    oldTime <- readIORef timeRef
    cTime <- getMonotonicTimeNSec
    let !waitTime = nanos - fromIntegral (cTime - oldTime)
    if waitTime > 0
      then do
        writeIORef timeRef $ oldTime + nanosW64
        threadDelay (waitTime `quot` 1000)
      else writeIORef timeRef cTime
    pure b
  where
    frameTime = 1 / frameTime'
    nanos :: Int
    nanos = round $ frameTime * 10 ^ (9 :: Int)
    nanosW64 :: Word64
    nanosW64 = round $ frameTime * 10 ^ (9 :: Int)
{-# INLINE limitSampleRate #-}
