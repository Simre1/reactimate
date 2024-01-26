module Data.SF.Delay where

import Data.IORef
import Data.SF.Core

-- | Delay the execution by one sample
delaySample :: a -> SF r a a
delaySample initial = SF $ \_ _ -> do
  delayRef <- newIORef initial
  pure $ \a' -> do
    a <- readIORef delayRef
    writeIORef delayRef a'
    pure a
{-# INLINE delaySample #-}
