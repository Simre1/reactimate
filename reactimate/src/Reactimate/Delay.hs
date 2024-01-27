module Reactimate.Delay where

import Data.IORef
import Reactimate.Signal

-- | Delay the execution by one sample
delaySample :: a -> Signal r a a
delaySample initial = Signal $ \_ _ -> do
  delayRef <- newIORef initial
  pure $ \a' -> do
    a <- readIORef delayRef
    writeIORef delayRef a'
    pure a
{-# INLINE delaySample #-}

-- | Evaluate the signal once and then return its result
once :: Signal r a b -> Signal r a b
once (Signal signal) = Signal $ \fin r -> do
  ref <- newIORef Nothing
  f <- signal fin r
  pure $ \a -> do
    maybeB <- readIORef ref
    case maybeB of
      Just b -> pure b
      Nothing -> do
        !b <- f a
        writeIORef ref (Just b)
        pure b
{-# INLINE once #-}
