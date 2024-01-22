module Data.SF.Run where

import Control.Monad
import Data.SF.Core
import Data.IORef (newIORef, readIORef, modifyIORef')

reactimate :: SF r () (Maybe a) -> r -> IO a
reactimate sf env = do
  f <- setupSF sf env
  let loop = do
        v <- f ()
        maybe loop pure v
  loop
{-# INLINE reactimate #-}

-- | The whole [b] needs to be produced before it returns!
sample :: SF r a b -> r -> [a] -> IO [b]
sample sf env inputs = do
  f <- setupSF sf env
  traverse f inputs
{-# INLINE sample #-}

fold :: (x -> b -> x) -> x -> SF r a b -> r -> [a] -> IO x
fold combine initial sf env inputs = do
  f <- setupSF sf env
  state <- newIORef initial
  forM_ inputs $ \a -> do
    b <- f a
    modifyIORef' state (`combine` b)
  readIORef state
{-# INLINE fold #-}
