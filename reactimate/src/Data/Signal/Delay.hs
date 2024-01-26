module Data.Signal.Delay where

import Data.IORef
import Data.Signal.Core

-- | Delay the execution by one sample
delaySample :: a -> Signal r a a
delaySample initial = Signal $ \_ _ -> do
  delayRef <- newIORef initial
  pure $ \a' -> do
    a <- readIORef delayRef
    writeIORef delayRef a'
    pure a
{-# INLINE delaySample #-}
