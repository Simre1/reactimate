{-# LANGUAGE RecursiveDo #-}

module Reactimate.Switching where

import Control.Monad (when)
import Data.IORef
import Data.Vector qualified as V
import Reactimate.Signal

-- | 'caseOf' is a powerful combinator to implement switching behavior. It is similar to case expressions, but for signal functions.
--
-- The first argument determines some @c@. The second argument takes the @c@ and decides which signal function should be used.
-- The signal functions generated by the second function keep their state across executions, i.e. when they get selected multiple times,
-- they keep their state.
--
-- Beware that this function should not be used when @c@ has many (~dozens) cases, since the setup phase will be run for each case.
caseOf :: forall c a b. (Bounded c, Enum c) => Signal a c -> (c -> Signal a b) -> Signal a b
caseOf decider makeSignal = Signal $ \fin -> do
  when (fromEnum (maxBound :: c) - fromEnum (minBound :: c) > 100) $
    fail "You probably do not want to use `caseSignal` with so many cases. Use `manyCaseSignal` if you really want to."
  unSignal (manyCaseSignal decider makeSignal) fin
{-# INLINE caseOf #-}

-- | Same as `caseOf` but will not error when you have a @c@ with many cases.
manyCaseSignal :: (Bounded c, Enum c) => Signal a c -> (c -> Signal a b) -> Signal a b
manyCaseSignal (Signal makeDecider) makeSignal = Signal $ \fin -> do
  decide <- makeDecider fin
  signals <- V.fromList <$> traverse (\c -> unSignal (makeSignal c) fin) [minBound .. maxBound]
  pure $ \a -> do
    c <- decide a
    let step = signals V.! fromEnum c
    step a
{-# INLINE manyCaseSignal #-}

-- | Switch out a signal function with another when you produce a @Just c@ value.
-- The next signal function will become active instantly. After a `Signal` has been switched out, it's outputs might be corrupted.  
switch :: Signal a (b, Maybe c) -> (c -> Signal a b) -> Signal a b
switch signal kont = Signal $ \fin -> mdo
  newFin <- newFinalizer
  f <- unSignal signal newFin

  stepRef <- newIORef $ \a -> do
    (b, maybeC) <- f a
    case maybeC of
      Nothing -> pure b
      Just c -> do
        newStep <- unSignal (kont c) fin
        writeIORef stepRef newStep
        runFinalizer newFin
        newStep a

  readIORef stepRef
