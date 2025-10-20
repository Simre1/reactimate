module Reactimate.Sampling (resample, resampleInThread, limitSampleRate) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad (forever, when)
import Data.Sequence (Seq)
import Data.Sequence qualified as S
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Reactimate.Handles
import Reactimate.Signal
import Reactimate.Time

-- | Resamples a Signal with the first argument as the specified @frameTime@ within the same thread.
-- The resampled Signal will have a fixed time delta of @frameTime@ and may run zero to multiple times per outer signal iteration
-- in order to approach the desired sampling rate.
resample :: (Time :> es) => Double -> (Signal es (Seq a) b) -> Signal es a (Seq b)
resample frameTime signal = makeSignal $ do
  nextSampleTimeRef <- newRef 0
  inputRef <- newRef S.empty
  outputRef <- newRef S.empty
  f <- unSignal (interposeFixedTime frameTime signal)
  ct <- unSignal currentTime

  pure $ \a -> do
    modifyRef' inputRef (S.:|> a)

    initialSampleTime <- readRef nextSampleTimeRef
    when (initialSampleTime <= 1) $ do
      -- no sampling has been done yet in this branch, so schedule the next sample to now
      ct () >>= writeRef nextSampleTimeRef

    let go = do
          nextSampleTime <- readRef nextSampleTimeRef
          now <- ct ()

          when (now >= nextSampleTime) $ do
            inputs <- readRef inputRef
            writeRef inputRef S.empty
            b <- f inputs
            modifyRef' outputRef (S.:|> b)
            writeRef nextSampleTimeRef (nextSampleTime + frameTime)
            go

    go

    bs <- readRef outputRef
    writeRef outputRef S.empty
    pure bs

-- | Sample a signal function in another thread at full speed. The thread is killed when the signal is switched out.
-- You may want to limit sampling speed with `limitSampleRate`.
resampleInThread :: (IOE :> es) => Signal es (Seq a) b -> Signal es a (Seq b)
resampleInThread signal = makeSignal $ do
  inputRef <- newRef S.empty
  outputRef <- newRef S.empty
  f <- unSignal signal
  IOE liftIO <- getHandle
  unlift <- unliftStep
  let action = unlift $ do
        inputs <- atomicModifyRef' inputRef (S.empty,)
        output <- f inputs
        modifyRef' outputRef (S.:|> output)

  asyncRef <- prestep $ liftIO $ async $ forever $ action
  finalize $ cancel asyncRef

  pure $ \a -> do
    modifyRef' inputRef (S.:|> a)
    atomicModifyRef' outputRef (S.empty,)

-- | Limit the real world samples per second. The first argument is samples per second.
limitSampleRate :: (IOE :> es) => Double -> Signal es a b -> Signal es a b
limitSampleRate frameTime' signal = makeSignal $ do
  timeRef <- newRef 0
  f <- unSignal signal
  IOE liftIO <- getHandle

  pure $ \a -> do
    b <- f a
    oldTime <- readRef timeRef
    cTime <- liftIO getMonotonicTimeNSec
    let !waitTime = nanos - fromIntegral (cTime - oldTime)
    if waitTime > 0
      then do
        writeRef timeRef $ oldTime + nanosW64
        liftIO $ threadDelay (waitTime `quot` 1000)
      else writeRef timeRef cTime
    pure b
  where
    frameTime = 1 / frameTime'
    nanos :: Int
    nanos = round $ frameTime * 10 ^ (9 :: Int)
    nanosW64 :: Word64
    nanosW64 = round $ frameTime * 10 ^ (9 :: Int)
{-# INLINE limitSampleRate #-}
