module Reactimate.Run where

import Control.Monad
import Data.IORef (modifyIORef', newIORef, readIORef)
import Reactimate.Signal

-- | Run a signal function repeatedly until it produces a `Just` value.
--
-- You may want to combine `reactimate` with `limitSampleRate`.
reactimate :: Signal () (Maybe a) -> IO a
reactimate signal =
  withFinalizer $ \fin -> do
    f <- unSignal signal fin 
    let loop = do
          v <- f ()
          maybe loop pure v
    loop
{-# INLINE reactimate #-}

-- | Sample a signal function until the input list is exhausted.
--
-- Beware that the whole [b] needs to be produced before it can return! This can lead to bad performance
-- in terms of memory and runtime.
sample :: Signal a b -> [a] -> IO [b]
sample signal inputs = do
  withFinalizer
  $ \fin -> do
    f <- unSignal signal fin 
    traverse f inputs
{-# INLINE sample #-}

-- | Fold a signal function strictly until the input list is exhausted.
fold :: (x -> b -> x) -> x -> Signal a b -> [a] -> IO x
fold combine initial signal inputs = do
  withFinalizer
  $ \fin -> do
    f <- unSignal signal fin 
    state <- newIORef initial
    forM_ inputs $ \a -> do
      b <- f a
      modifyIORef' state (`combine` b)
    readIORef state
{-# INLINE fold #-}
