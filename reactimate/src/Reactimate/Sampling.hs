module Reactimate.Sampling where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad (forever, when)
import Data.IORef
import Data.Sequence (Seq)
import Data.Sequence qualified as S
import Data.Word (Word64)
import Effectful
import Effectful.Dispatch.Static
import GHC.Clock (getMonotonicTimeNSec)
import Reactimate.Setup (bracketSetup)
import Reactimate.Signal
import Reactimate.Time

-- | Resamples a Signal with the first argument as the specified @frameTime@ within the same thread. The resampled Signal will have a fixed time delta of @frameTime@.
-- The inputs and outputs are collected in a sequence.
resample :: (Time :> es) => Double -> (Signal es (Seq a) b) -> Signal es a (Seq b)
resample frameTime signal = Signal $ withRef 0 $ \nextSampleTimeRef ->
  withRef S.empty $ \inputRef -> withRef S.empty $ \outputRef -> do
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

-- | Samples a signal function in another thread. You may want to limit sampling speed with `limitSampleRate`.
resampleInThread :: (IOE :> es) => Signal es (Seq a) b -> Signal es a (Seq b)
resampleInThread signal = Signal $ withRef S.empty $ \inputRef -> withRef S.empty $ \outputRef -> do
  f <- unSignal signal

  unSignal $
    bracketSetup
      ( liftIO $ do
          mVar <- newEmptyMVar
          asyncRef <- async $ forever $ do
            unlift <- readMVar mVar
            unlift $ do
              inputs <- atomicModifyRef' inputRef (S.empty,)
              output <- f inputs
              modifyRef' outputRef (S.:|> output)
          pure (mVar, asyncRef)
      )
      (liftIO . cancel . snd)
      ( \(mVar, _) -> Signal $ do
          pure $ \a -> do
            -- TODO: Not sure if escaping the scope is safe ...docs do not say anything
            unlift <- unsafeEff $ \env -> concUnliftIO env Ephemeral (Limited 1) $ \unlift -> do
              pure unlift
            _ <- liftIO $ swapMVar mVar unlift

            modifyRef' inputRef (S.:|> a)
            atomicModifyRef' outputRef (S.empty,)
      )

-- | Limit the real world samples per second. The first argument is samples per second.
limitSampleRate :: (IOE :> es) => Double -> Signal es a b -> Signal es a b
limitSampleRate frameTime' (Signal signal) = Signal $ withRef 0 $ \timeRef -> do
  f <- signal
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
