module Reactimate.Basic where

import Control.Arrow (Arrow (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Effectful (Eff, IOE, (:>))
import Reactimate.Signal

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
arrEff :: (a -> Eff es b) -> Signal es a b
arrEff f = Signal $ pure f
{-# INLINE arrEff #-}

-- | Run an IO action during a signal function.
arrIO :: (IOE :> es) => (a -> IO b) -> Signal es a b
arrIO f = Signal $ pure $ liftIO . f
{-# INLINE arrIO #-}

-- | Run an effectful action during a signal function without input.
actionEff :: Eff es a -> Signal es x a
actionEff action = Signal $ pure (const action)
{-# INLINE actionEff #-}

-- | Run an effectful action during a signal function without input.
actionIO :: (IOE :> es) => IO a -> Signal es x a
actionIO action = Signal $ pure (const (liftIO action))
{-# INLINE actionIO #-}
