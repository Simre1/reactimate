module Data.Signal.Run where

import Control.Monad
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Signal.Core

-- | Run a signal function repeatedly until it produces a `Just` value.
--
-- You may want to combine `reactimate` with `limitSampleRate`.
reactimate :: Signal r () (Maybe a) -> r -> IO a
reactimate signal env =
  withFinalizer $ \fin -> do
    f <- unSignal signal fin env
    let loop = do
          v <- f ()
          maybe loop pure v
    loop
{-# INLINE reactimate #-}

-- | Sample a signal function until the input list is exhausted.
--
-- Beware that the whole [b] needs to be produced before it can return! This can lead to bad performance
-- in terms of memory and runtime.
sample :: Signal r a b -> r -> [a] -> IO [b]
sample signal env inputs = do
  withFinalizer
  $ \fin -> do
    f <- unSignal signal fin env
    traverse f inputs
{-# INLINE sample #-}

-- | Fold a signal function strictly until the input list is exhausted.
fold :: (x -> b -> x) -> x -> Signal r a b -> r -> [a] -> IO x
fold combine initial signal env inputs = do
  withFinalizer
  $ \fin -> do
    f <- unSignal signal fin env
    state <- newIORef initial
    forM_ inputs $ \a -> do
      b <- f a
      modifyIORef' state (`combine` b)
    readIORef state
{-# INLINE fold #-}
