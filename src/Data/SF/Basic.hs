module Data.SF.Basic where

import Control.Arrow (Arrow (..))
import Data.SF.Core

-- | Duplicating the input may be useful for various other arrow combinators.
dup :: SF r a (a, a)
dup = arr (\a -> (a, a))
{-# INLINE dup #-}

-- | Same as `arr` but for functions with two arguments.
arr2 :: (a -> b -> c) -> SF r (a, b) c
arr2 f = arr (uncurry f)
{-# INLINE arr2 #-}

-- | Same as `id` but for signal functions
identity :: SF r a a
identity = arr id
{-# INLINE identity #-}

-- | Same as `const` but for signal functions
constant :: b -> SF r a b
constant = pure
{-# INLINE constant #-}

-- | Run an IO action during a signal function.
arrIO :: (a -> IO b) -> SF r a b
arrIO f = SF $ \_ _ -> pure f
{-# INLINE arrIO #-}
