module Data.Signal.Environment where

import Data.Signal.Core

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
