module Reactimate.Setup where

import Effectful
import Effectful.Exception (bracket)
import Reactimate.Signal

-- | Evaluate the signal once and then return its result
once :: Signal es a b -> Signal es a b
once (Signal signal) = Signal $ withRef Nothing $ \ref -> do
  f <- signal
  pure $ \a -> do
    maybeB <- readRef ref
    case maybeB of
      Just b -> pure b
      Nothing -> do
        !b <- f a
        writeRef ref (Just b)
        pure b
{-# INLINE once #-}

withSetup :: Eff es x -> (x -> Signal es a b) -> Signal es a b
withSetup effX kont = Signal $ withSwitch $ \switchPoint -> do
  pure $
    ( Signal $ pure $ \a -> do
        x <- effX
        updateSwitch switchPoint (kont x)
        runSwitch switchPoint a,
      runSwitch switchPoint
    )
{-# INLINE withSetup #-}

bracketSetup :: Eff es x -> (x -> Eff es ()) -> (x -> Signal es a b) -> Signal es a b
bracketSetup effX finalize kont = Signal $ withSwitch $ \switchPoint -> do
  finalizer <- getFinalizer
  pure $
    ( Signal $ pure $ \a -> do
        x <- bracket effX (\x -> finalizer (finalize x)) pure
        updateSwitch switchPoint (kont x)
        runSwitch switchPoint a,
      runSwitch switchPoint
    )
{-# INLINE bracketSetup #-}
