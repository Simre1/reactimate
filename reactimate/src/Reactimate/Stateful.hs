module Reactimate.Stateful
  ( feedback,
    feedbackState,
    feedbackLazyState,
    scan,
    sumUp,
    delay,
  )
where

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

-- | Delay the value by one iteration
delay :: a -> Signal es a a
delay initial = makeSignal $ do
  delayRef <- newRef initial
  pure $ \a' -> do
    a <- readRef delayRef
    writeRef delayRef a'
    pure a
{-# INLINE delay #-}
