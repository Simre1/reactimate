module Reactimate.Stateful where

import Control.Arrow ((>>>))
import Data.IORef
import Reactimate.Basic
import Reactimate.Signal

-- | Feeds the output state back as input. The state @s@ is strict.
feedback :: s -> Signal (a, s) (b, s) -> Signal a b
feedback !initial (Signal signal) = Signal $ \fin -> do
  f <- signal fin 
  stateRef <- newIORef initial
  pure $ \a -> do
    !s <- readIORef stateRef
    (b, !s') <- f (a, s)
    writeIORef stateRef s'
    pure b
{-# INLINE feedback #-}

-- | Feeds the output state back as input. The state @s@ is lazy, so beware space leaks.
lazyFeedback :: s -> Signal (a, s) (b, s) -> Signal a b
lazyFeedback initial (Signal signal) = Signal $ \fin -> do
  f <- signal fin 
  stateRef <- newIORef initial
  pure $ \a -> do
    s <- readIORef stateRef
    (b, s') <- f (a, s)
    writeIORef stateRef s'
    pure b
{-# INLINE lazyFeedback #-}

-- | Scan along time. The first function will be run each execution with the input @a@, produce the output @b@ and reuse @b@ as state for the next iteration.
scan :: (b -> a -> b) -> b -> Signal a b
scan f initial = feedback initial (arr2 (flip f) >>> dup)
{-# INLINE scan #-}

-- | Sums up the input values
sumUp :: (Num a) => Signal a a
sumUp = scan (+) 0
{-# INLINE sumUp #-}

-- | Computes a moving mean of the input values
--
-- The first parameter @alpha@ must be in the intervall [0,1] and controls how strongly recent samples are weighted.
-- Small @alpha@ near 0 leads to slower but smoother convergence. Big @alpha@ leads to quick convergence but a jagged curve.
movingMean :: (Fractional a) => a -> Signal a a
movingMean alpha = scan (\b a -> b + alpha * (a - b)) 0
{-# INLINE movingMean #-}
