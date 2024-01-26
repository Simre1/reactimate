module Data.SF.Environment where

import Data.SF.Core

-- | Access the environment when signal functions run
useEnv :: (r -> a -> c) -> SF r c b -> SF r a b
useEnv f (SF sf) = SF $ \fin r -> do
  step <- sf fin r
  pure $ \a -> step (f r a)
{-# INLINE useEnv #-}

-- | Modify the enviroment during the setup phase.
modifyEnv :: (r1 -> IO r2) -> SF r2 a b -> SF r1 a b
modifyEnv f (SF sf) = SF $ \fin r1 -> do
  r2 <- f r1
  sf fin r2
{-# INLINE modifyEnv #-}

-- | Map the environment
mapEnv :: (r1 -> r2) -> SF r2 a b -> SF r1 a b
mapEnv f = modifyEnv (pure . f)
{-# INLINE mapEnv #-}

-- | Do some setup before any signal functions actually run.
-- The setup action will be run **once** before any signal functions produce outputs.
withSetup :: (r1 -> IO (r2, x)) -> SF r2 (a, x) b -> SF r1 a b
withSetup setup (SF sf) = SF $ \fin r1 -> do
  (r2, x) <- setup r1
  f <- sf fin r2
  pure $ \a -> f (a, x)
{-# INLINE withSetup #-}
