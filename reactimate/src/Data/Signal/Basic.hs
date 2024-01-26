module Data.Signal.Basic where

import Control.Arrow (Arrow (..))
import Data.Signal.Core

-- | Duplicating the input may be useful for various other arrow combinators.
dup :: Signal r a (a, a)
dup = arr (\a -> (a, a))
{-# INLINE dup #-}

-- | Same as `arr` but for functions with two arguments.
arr2 :: (a -> b -> c) -> Signal r (a, b) c
arr2 f = arr (uncurry f)
{-# INLINE arr2 #-}

-- | Same as `id` but for signal functions
identity :: Signal r a a
identity = arr id
{-# INLINE identity #-}

-- | Same as `const` but for signal functions
constant :: b -> Signal r a b
constant = pure
{-# INLINE constant #-}

-- | Run an IO action during a signal function.
arrIO :: (a -> IO b) -> Signal r a b
arrIO f = Signal $ \_ _ -> pure f
{-# INLINE arrIO #-}
