module Reactimate.Basic where

import Control.Arrow (Arrow (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Reactimate.Signal
import Reactimate.Union

-- | Duplicating the input may be useful for various other arrow combinators.
dup :: Signal es a (a, a)
dup = arr (\a -> (a, a))
{-# INLINE dup #-}

-- | Same as `arr` but for functions with two arguments.
arr2 :: (a -> b -> c) -> Signal es (a, b) c
arr2 f = arr (uncurry f)
{-# INLINE arr2 #-}

-- | Same as `id` but for signal functions
identity :: Signal es a a
identity = arr id
{-# INLINE identity #-}

-- | Same as `const` but for signal functions
constant :: b -> Signal es a b
constant = pure
{-# INLINE constant #-}

-- | Run an effectful action during a signal function.
arrStep :: (Member e es) => (forall s. e s -> a -> Step s b) -> Signal es a b
arrStep f = makeSignal $ do
  e <- getHandle
  let g = f e
  pure $ \a -> g a
{-# INLINE arrStep #-}

-- | Run an IO action during a signal function.
arrIO :: (IOE :> es) => (a -> IO b) -> Signal es a b
arrIO f = makeSignal $ do
  IOE lift <- getHandle
  pure $ lift . f
{-# INLINE arrIO #-}

-- | Run an effectful action during a signal function without input.
actionStep :: (Member e es) => (forall s. e s -> Step s a) -> Signal es x a
actionStep action = makeSignal $ do
  e <- getHandle
  let a = action e
  pure $ \_ -> a
{-# INLINE actionStep #-}

-- | Run an effectful action during a signal function without input.
actionIO :: (IOE :> es) => IO a -> Signal es x a
actionIO action = makeSignal $ do
  IOE lift <- getHandle
  pure $ \_ -> lift action
{-# INLINE actionIO #-}
