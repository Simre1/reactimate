module Data.SF.Combinators where

import Control.Arrow (Arrow (..), (>>>))
import Control.Monad (when)
import Data.Coerce (coerce)
import Data.IORef
import Data.SF.Core
import Data.Vector qualified as V

dup :: SF r a (a, a)
dup = arr (\a -> (a, a))
{-# INLINE dup #-}

arr2 :: (a -> b -> c) -> SF r (a, b) c
arr2 f = arr (uncurry f)
{-# INLINE arr2 #-}

identity :: SF r a a
identity = arr id
{-# INLINE identity #-}

constant :: b -> SF r a b
constant = pure
{-# INLINE constant #-}

caseOf :: forall c r a b. (Bounded c, Enum c) => SF r a c -> (c -> SF r a b) -> SF r a b
caseOf decider makeSF = SF $ \r -> do
  when (fromEnum (maxBound :: c) - fromEnum (minBound :: c) > 100) $
    fail "You probably do not want to use `caseSF` with so many cases. Use `manyCaseSF` if you really want to."
  coerce (manyCaseSF decider makeSF) r
{-# INLINE caseOf #-}

manyCaseSF :: (Bounded c, Enum c) => SF r a c -> (c -> SF r a b) -> SF r a b
manyCaseSF (SF makeDecider) makeSF = SF $ \r -> do
  decide <- makeDecider r
  sfs <- V.fromList <$> traverse (($ r) . coerce . makeSF) [minBound .. maxBound]
  pure $ \a -> do
    c <- decide a
    let step = sfs V.! fromEnum c
    step a
{-# INLINE manyCaseSF #-}

feedback :: s -> SF r (a, s) (b, s) -> SF r a b
feedback !initial (SF sf) = SF $ \r -> do
  f <- sf r
  stateRef <- newIORef initial
  pure $ \a -> do
    !s <- readIORef stateRef
    (b, !s') <- f (a, s)
    writeIORef stateRef s'
    pure b
{-# INLINE feedback #-}

lazyFeedback :: s -> SF r (a, s) (b, s) -> SF r a b
lazyFeedback initial (SF sf) = SF $ \r -> do
  f <- sf r
  stateRef <- newIORef initial
  pure $ \a -> do
    s <- readIORef stateRef
    (b, s') <- f (a, s)
    writeIORef stateRef s'
    pure b
{-# INLINE lazyFeedback #-}

scan :: (b -> a -> b) -> b -> SF r a b
scan f initial = feedback initial (arr2 (flip f) >>> dup)
{-# INLINE scan #-}
