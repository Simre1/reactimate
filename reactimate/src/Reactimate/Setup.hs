module Reactimate.Setup where

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

withSetup :: (forall s. Setup es s x) -> (x -> Signal es a b) -> Signal es a b
withSetup setupX kont = makeSignal $ do
  x <- setupX
  unSignal (kont x)
{-# INLINE withSetup #-}

bracketSetup :: (forall s. (Setup es s x, x -> Setup es s ())) -> (x -> Signal es a b) -> Signal es a b
bracketSetup aquireRelease kont = makeSignal $ do
  let (aquire, release) = aquireRelease
  x <- aquire
  finalize (release x)
  unSignal $ kont x
{-# INLINE bracketSetup #-}
