module Reactimate.Setup where

import Reactimate.Signal

-- | Allocate some resources and add them to the environment.
-- Use the `Finalizer` to destroy the resource.
allocateResource :: (Finalizer -> IO r) -> (r -> Signal a b) -> Signal a b
allocateResource f signal = Signal $ \fin -> do
  res <- f fin
  unSignal (signal res) fin
{-# INLINE allocateResource #-}

-- | Do some setup before any signal functions actually run.
-- The setup action will be run **once** before any signal functions produce outputs.
withSetup :: IO r -> (r -> Signal a b) -> Signal a b
withSetup setup signal = Signal $ \fin -> do
  r <- setup
  unSignal (signal r) fin
{-# INLINE withSetup #-}

