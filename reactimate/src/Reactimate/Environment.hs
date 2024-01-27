module Reactimate.Environment where

import Reactimate.Signal

-- | Access the environment when signal functions run
useEnv :: (r -> a -> c) -> Signal r c b -> Signal r a b
useEnv f (Signal signal) = Signal $ \fin r -> do
  step <- signal fin r
  pure $ \a -> step (f r a)
{-# INLINE useEnv #-}

-- | Modify the enviroment during the setup phase.
modifyEnv :: (r1 -> IO r2) -> Signal r2 a b -> Signal r1 a b
modifyEnv f (Signal signal) = Signal $ \fin r1 -> do
  r2 <- f r1
  signal fin r2
{-# INLINE modifyEnv #-}

-- | Allocate some resources and add them to the environment.
-- Use the `Finalizer` to destroy the resource.
allocateResource :: (Finalizer -> r1 -> IO r2) -> Signal r2 a b -> Signal r1 a b
allocateResource f (Signal signal) = Signal $ \fin r1 -> do
  r2 <- f fin r1
  signal fin r2
{-# INLINE allocateResource #-}

-- | Map the environment
mapEnv :: (r1 -> r2) -> Signal r2 a b -> Signal r1 a b
mapEnv f = modifyEnv (pure . f)
{-# INLINE mapEnv #-}

-- | Do some setup before any signal functions actually run.
-- The setup action will be run **once** before any signal functions produce outputs.
withSetup :: (r1 -> IO (r2, x)) -> Signal r2 (a, x) b -> Signal r1 a b
withSetup setup (Signal signal) = Signal $ \fin r1 -> do
  (r2, x) <- setup r1
  f <- signal fin r2
  pure $ \a -> f (a, x)
{-# INLINE withSetup #-}

-- | Allocate some resources before any signal functions run. 
-- The setup action will be run **once** before any signal functions produce outputs.
withResourceSetup :: (Finalizer -> r1 -> IO (r2, x)) -> Signal r2 (a, x) b -> Signal r1 a b
withResourceSetup setup (Signal signal) = Signal $ \fin r1 -> do
  (r2, x) <- setup fin r1
  f <- signal fin r2
  pure $ \a -> f (a, x)
{-# INLINE withResourceSetup #-}
