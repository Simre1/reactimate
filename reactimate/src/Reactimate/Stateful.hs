module Reactimate.Stateful where

import Control.Arrow ((>>>))
import Data.IORef
import Reactimate.Basic
import Reactimate.Signal

-- | Feeds the output back as input.
feedback :: b -> Signal (a, b) b -> Signal a b
feedback !initial (Signal signal) = Signal $ \fin -> do
  f <- signal fin 
  stateRef <- newIORef initial
  pure $ \a -> do
    !b <- readIORef stateRef
    !nextB <- f (a,b)
    writeIORef stateRef nextB
    pure nextB
{-# INLINE feedback #-}

-- | Feeds the output state back as input. The state @s@ is strict.
feedbackState :: s -> Signal (a, s) (b, s) -> Signal a b
feedbackState !initial (Signal signal) = Signal $ \fin -> do
  f <- signal fin 
  stateRef <- newIORef initial
  pure $ \a -> do
    !s <- readIORef stateRef
    (b, !s') <- f (a, s)
    writeIORef stateRef s'
    pure b
{-# INLINE feedbackState #-}

-- | Feeds the output state back as input. The state @s@ is lazy, so beware space leaks.
feedbackLazyState :: s -> Signal (a, s) (b, s) -> Signal a b
feedbackLazyState initial (Signal signal) = Signal $ \fin -> do
  f <- signal fin 
  stateRef <- newIORef initial
  pure $ \a -> do
    s <- readIORef stateRef
    (b, s') <- f (a, s)
    writeIORef stateRef s'
    pure b
{-# INLINE feedbackLazyState #-}

-- | Scan along time. The first function will be run each execution with the input @a@, produce the output @b@ and reuse @b@ as state for the next iteration.
scan :: (b -> a -> b) -> b -> Signal a b
scan f initial = feedback initial (arr2 (flip f))
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
