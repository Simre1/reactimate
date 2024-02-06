module Reactimate.Delay where

import Data.IORef
import Reactimate.Signal

-- | Delay the execution by one sample
delaySample :: a -> Signal a a
delaySample initial = Signal $ \_ -> do
  delayRef <- newIORef initial
  pure $ \a' -> do
    a <- readIORef delayRef
    writeIORef delayRef a'
    pure a
{-# INLINE delaySample #-}

-- | Evaluate the signal once and then return its result
once :: Signal a b -> Signal a b
once (Signal signal) = Signal $ \fin -> do
  ref <- newIORef Nothing
  f <- signal fin 
  pure $ \a -> do
    maybeB <- readIORef ref
    case maybeB of
      Just b -> pure b
      Nothing -> do
        !b <- f a
        writeIORef ref (Just b)
        pure b
{-# INLINE once #-}
