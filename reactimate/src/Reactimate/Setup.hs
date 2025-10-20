module Reactimate.Setup (once, withSetup, bracketSetup) where

import Reactimate.Handles
import Reactimate.Signal

-- | Evaluate the signal once and then return its result
once :: Signal es a b -> Signal es a b
once signal = makeSignal $ do
  ref <- newRef Nothing
  f <- unSignal signal
  pure $ \a -> do
    maybeB <- readRef ref
    case maybeB of
      Just b -> pure b
      Nothing -> do
        !b <- f a
        writeRef ref (Just b)
        pure b
{-# INLINE once #-}

-- | Run a setup action and then continue with the signal.
withSetup :: (forall s. Setup es s x) -> (x -> Signal es a b) -> Signal es a b
withSetup setupX kont = makeSignal $ do
  x <- setupX
  unSignal (kont x)
{-# INLINE withSetup #-}

-- | Run a setup action with a finalizer and then continue with the signal. The finalizer runs when the signal is switched out.
bracketSetup :: (IOE :> es) => (forall s. (Setup es s x, x -> IO ())) -> (x -> Signal es a b) -> Signal es a b
bracketSetup aquireRelease kont = makeSignal $ do
  let (aquire, release) = aquireRelease
  x <- aquire
  finalize (release x)
  unSignal $ kont x
{-# INLINE bracketSetup #-}
