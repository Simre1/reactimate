module Data.SF.Run where

import Control.Monad
import Data.SF.Core
import Data.IORef (newIORef, readIORef, modifyIORef')

-- | Run a signal function repeatedly until it produces a `Just` value.
reactimate :: SF r () (Maybe a) -> r -> IO a
reactimate sf env = do
  f <- unSF sf env
  let loop = do
        v <- f ()
        maybe loop pure v
  loop
{-# INLINE reactimate #-}

-- | Sample a signal function until the input list is exhausted.
--
-- | Beware that the whole [b] needs to be produced before it can return! This can lead to bad performance
-- in terms of memory and runtime.
sample :: SF r a b -> r -> [a] -> IO [b]
sample sf env inputs = do
  f <- unSF sf env
  traverse f inputs
{-# INLINE sample #-}

-- | Fold a signal function strictly until the input list is exhausted.
fold :: (x -> b -> x) -> x -> SF r a b -> r -> [a] -> IO x
fold combine initial sf env inputs = do
  f <- unSF sf env
  state <- newIORef initial
  forM_ inputs $ \a -> do
    b <- f a
    modifyIORef' state (`combine` b)
  readIORef state
{-# INLINE fold #-}
