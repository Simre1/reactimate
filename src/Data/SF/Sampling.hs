module Data.SF.Sampling where

import Control.Monad (when, forever)
import Data.IORef
import Data.SF.Core
import Data.SF.Time
import Data.Sequence (Seq)
import Data.Sequence qualified as S
import GHC.Records (HasField)
import Control.Concurrent.Async

-- | Resamples a SF with the first argument as the specified @frameTime@ within the same thread. The resampled SF will have a fixed time delta of @frameTime@.
-- The inputs and outputs are collected in a sequence.
resample :: (HasField "time" r1 Time) => Double -> (r1 -> Time -> r2) -> SF r2 (Seq a) b -> SF r1 a (Seq b)
resample frameTime setTime sf = SF $ \fin r -> do
  f <- unSF (withFixedTime frameTime setTime sf) fin r
  nextSampleTimeRef <- newIORef 0
  inputRef <- newIORef S.empty
  outputRef <- newIORef S.empty
  ct <- unSF currentTime fin r

  pure $ \a -> do
    modifyIORef' inputRef (S.:|> a)

    nextSampleTime <- readIORef nextSampleTimeRef

    when (nextSampleTime <= 1) $ do
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

 -- | Samples a signal function in another thread. You may want to limit sampling speed with `limitSampleRate`.

 -- Keep in mind that the environment is shared. Timing information from the original thread will be __wrong__ and must be replaced if needed.
 -- Also, some __environments might not be thread-safe__.
resampleInThread :: SF r (Seq a) b -> SF r a (Seq b)
resampleInThread sf = SF $ \fin r -> do
  f <- unSF sf fin r
  inputRef <- newIORef S.empty
  outputRef <- newIORef S.empty

  asyncRef <- async $ forever $ do
    inputs <- atomicModifyIORef' inputRef (S.empty, )
    output <- f inputs
    modifyIORef' outputRef (S.:|> output)

  addFinalizer fin (cancel asyncRef)

  pure $ \a -> do
    modifyIORef' inputRef (S.:|> a)
    atomicModifyIORef' outputRef (S.empty, )
