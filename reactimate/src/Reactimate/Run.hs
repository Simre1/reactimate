module Reactimate.Run where

import Control.Concurrent (newEmptyMVar, readMVar)
import Control.Concurrent.MVar (putMVar)
import Control.Monad
import Data.IORef (modifyIORef', newIORef, readIORef)
import Effectful
import Reactimate.Event
import Reactimate.Signal

-- | Run a signal function repeatedly until it produces a `Just` value.
--
-- You may want to combine `reactimate` with `limitSampleRate`.
reactimate :: Signal es () (Maybe a) -> Eff es a
reactimate signal = runSetup $ do
  f <- unSignal signal
  let loop = do
        v <- f ()
        maybe loop pure v
  pure $ loop
{-# INLINE reactimate #-}

-- | Sample a signal function until the input list is exhausted.
--
-- Beware that the whole [b] needs to be produced before it can return! This can lead to bad performance
-- in terms of memory and runtime.
sample :: Signal es a b -> [a] -> Eff es [b]
sample signal inputs = runSetup $ do
  f <- unSignal signal
  pure $ traverse f inputs
{-# INLINE sample #-}

-- | Fold a signal function strictly until the input list is exhausted.
fold :: (x -> b -> x) -> x -> Signal es a b -> [a] -> Eff es x
fold combine initial signal inputs = runSetup $ withRef initial $ \state -> do
  f <- unSignal signal
  pure $ do
    forM_ inputs $ \a -> do
      b <- f a
      modifyRef' state (`combine` b)
    readRef state
{-# INLINE fold #-}

-- reactimateEvent :: Event (Maybe a) -> IO a
-- reactimateEvent (Event signal hook) =
--   withFinalizer $ \fin -> do
--     mvar <- newEmptyMVar
--     f <- unSignal signal fin
--     hook fin $ \x -> do
--       maybeA <- f x
--       maybe mempty (putMVar mvar) maybeA
--     readMVar mvar
-- {-# INLINE reactimateEvent #-}
