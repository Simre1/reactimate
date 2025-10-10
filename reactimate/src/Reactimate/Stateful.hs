module Reactimate.Stateful where

import Reactimate.Basic
import Reactimate.Signal

-- | Feeds the output back as input.
feedback :: b -> Signal es (a, b) b -> Signal es a b
feedback !initial signal = makeSignal $ do
  stateRef <- newRef initial
  f <- unSignal signal
  pure $ \a -> do
    !b <- readRef stateRef
    !nextB <- f (a, b)
    writeRef stateRef nextB
    pure nextB
{-# INLINE feedback #-}

-- | Feeds the output state back as input. The state @s@ is strict.
feedbackState :: s -> Signal es (a, s) (b, s) -> Signal es a b
feedbackState !initial signal = makeSignal $ do
  stateRef <- newRef initial
  f <- unSignal signal
  pure $ \a -> do
    !s <- readRef stateRef
    (b, !s') <- f (a, s)
    writeRef stateRef s'
    pure b
{-# INLINE feedbackState #-}

-- | Feeds the output state back as input. The state @s@ is lazy, so beware space leaks.
feedbackLazyState :: s -> Signal es (a, s) (b, s) -> Signal es a b
feedbackLazyState initial signal = makeSignal $ do
  stateRef <- newRef initial
  f <- unSignal signal
  pure $ \a -> do
    s <- readRef stateRef
    (b, s') <- f (a, s)
    writeRef stateRef s'
    pure b
{-# INLINE feedbackLazyState #-}

-- | Scan along time. The first function will be run each execution with the input @a@, produce the output @b@ and reuse @b@ as state for the next iteration.
scan :: (b -> a -> b) -> b -> Signal es a b
scan f initial = feedback initial (arr2 (flip f))
{-# INLINE scan #-}

-- | Sums up the input values
sumUp :: (Num a) => Signal es a a
sumUp = scan (+) 0
{-# INLINE sumUp #-}

-- | Computes a moving mean of the input values
--
-- The first parameter @alpha@ must be in the intervall [0,1] and controls how strongly recent samples are weighted.
-- Small @alpha@ near 0 leads to slower but smoother convergence. Big @alpha@ leads to quick convergence but a jagged curve.
movingMean :: (Fractional a) => a -> Signal es a a
movingMean alpha = scan (\b a -> b + alpha * (a - b)) 0
{-# INLINE movingMean #-}

-- | Delay the value by one iteration
delay :: a -> Signal es a a
delay initial = makeSignal $ do
  delayRef <- newRef initial
  pure $ \a' -> do
    a <- readRef delayRef
    writeRef delayRef a'
    pure a
{-# INLINE delay #-}
