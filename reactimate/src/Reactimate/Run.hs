module Reactimate.Run where

import Control.Monad
import Data.IORef (modifyIORef', newIORef, readIORef)
import Reactimate.Signal

-- | Run a signal function repeatedly until it produces a `Just` value.
--
-- You may want to combine `reactimate` with `limitSampleRate`.
reactimate :: r -> Signal r () (Maybe a) -> IO a
reactimate env signal =
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
sample :: r -> Signal r a b -> [a] -> IO [b]
sample env signal inputs = do
  withFinalizer
  $ \fin -> do
    f <- unSignal signal fin env
    traverse f inputs
{-# INLINE sample #-}

-- | Fold a signal function strictly until the input list is exhausted.
fold :: r -> (x -> b -> x) -> x -> Signal r a b -> [a] -> IO x
fold env combine initial signal inputs = do
  withFinalizer
  $ \fin -> do
    f <- unSignal signal fin env
    state <- newIORef initial
    forM_ inputs $ \a -> do
      b <- f a
      modifyIORef' state (`combine` b)
    readIORef state
{-# INLINE fold #-}
