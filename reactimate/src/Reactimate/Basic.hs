module Reactimate.Basic where

import Control.Arrow (Arrow (..))
import Reactimate.Signal

-- | Duplicating the input may be useful for various other arrow combinators.
dup :: Signal a (a, a)
dup = arr (\a -> (a, a))
{-# INLINE dup #-}

-- | Same as `arr` but for functions with two arguments.
arr2 :: (a -> b -> c) -> Signal (a, b) c
arr2 f = arr (uncurry f)
{-# INLINE arr2 #-}

-- | Same as `id` but for signal functions
identity :: Signal a a
identity = arr id
{-# INLINE identity #-}

-- | Same as `const` but for signal functions
constant :: b -> Signal a b
constant = pure
{-# INLINE constant #-}

-- | Run an IO action during a signal function.
arrIO :: (a -> IO b) -> Signal a b
arrIO f = Signal $ \_ -> pure f
{-# INLINE arrIO #-}

-- | Run an IO action during a signal function without input.
actionIO :: IO a -> Signal x a
actionIO action = Signal $ \_ -> pure (const action)
{-# INLINE actionIO #-}
