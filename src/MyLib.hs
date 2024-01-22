module Data.SF.Core where

import Control.Arrow
import Control.Category
import Control.Monad ((>=>))
import Data.IORef (newIORef, readIORef)
import GHC.IORef (writeIORef)
import Prelude hiding (id, (.))

newtype SF a b = SF (IO (a -> IO b))

instance Functor (SF a) where
  fmap f (SF m) = SF $ fmap (fmap f .) m
  {-# INLINE fmap #-}

instance Applicative (SF a) where
  pure a = SF $ pure $ pure $ pure a
  (SF sf1) <*> (SF sf2) = SF $ do
    f1 <- sf1
    f2 <- sf2
    pure $ \a -> f1 a <*> f2 a
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Category SF where
  id = SF $ pure $ \a -> pure a
  (SF sf1) . (SF sf2) = SF $ do
    f1 <- sf1
    f2 <- sf2
    pure $ f2 >=> f1
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance Arrow SF where
  arr f = SF $ pure $ pure . f
  first (SF sf) = SF $ do
    f <- sf
    pure $ \(a, b) -> (,b) <$> f a
  second (SF sf) = SF $ do
    f <- sf
    pure $ \(a, b) -> (a,) <$> f b
  (SF sf1) *** (SF sf2) = SF $ do
    f1 <- sf1
    f2 <- sf2
    pure $ \(a, b) -> (,) <$> f1 a <*> f2 b
  (SF sf1) &&& (SF sf2) = SF $ do
    f1 <- sf1
    f2 <- sf2
    pure $ \a -> (,) <$> f1 a <*> f2 a
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (***) #-}
  {-# INLINE (&&&) #-}

withSetup :: IO x -> SF (a, x) b -> SF a b
withSetup setupValue (SF sf) = SF $ do
  f <- sf
  x <- setupValue
  pure $ \a -> f (a, x)
{-# INLINE withSetup #-}

lazyFeedback :: s -> SF (a, s) (b, s) -> SF a b
lazyFeedback initial (SF sf) = SF $ do
  f <- sf
  stateRef <- newIORef initial
  pure $ \a -> do
    s <- readIORef stateRef
    (b, s') <- f (a, s)
    writeIORef stateRef s'
    pure b
{-# INLINE lazyFeedback #-}

feedback :: s -> SF (a, s) (b, s) -> SF a b
feedback !initial (SF sf) = SF $ do
  f <- sf
  stateRef <- newIORef initial
  pure $ \a -> do
    !s <- readIORef stateRef
    (b, !s') <- f (a, s)
    writeIORef stateRef s'
    pure b
{-# INLINE feedback #-}

setupSF :: SF a b -> IO (a -> IO b)
setupSF (SF sf) = sf
{-# INLINE setupSF #-}
