module Data.SF.Core where

import Control.Arrow
import Control.Category
import Control.Monad ((>=>))
import Prelude hiding (id, (.))

newtype SF r a b = SF (r -> IO (a -> IO b))

instance Functor (SF r a) where
  fmap f (SF m) = SF $ \r -> fmap (fmap f .) (m r)
  {-# INLINE fmap #-}

instance Applicative (SF r a) where
  pure a = SF $ \_ -> pure $ pure $ pure a
  (SF sf1) <*> (SF sf2) = SF $ \r -> do
    f1 <- sf1 r
    f2 <- sf2 r
    pure $ \a -> f1 a <*> f2 a
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Category (SF r) where
  id = SF $ \_ -> pure $ \a -> pure a
  (SF sf1) . (SF sf2) = SF $ \r -> do
    f1 <- sf1 r
    f2 <- sf2 r
    pure $ f2 >=> f1
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance Arrow (SF r) where
  arr f = SF $ \_ -> pure $ pure . f
  first (SF sf) = SF $ \r -> do
    f <- sf r
    pure $ \(a, b) -> (,b) <$> f a
  second (SF sf) = SF $ \r -> do
    f <- sf r
    pure $ \(a, b) -> (a,) <$> f b
  (SF sf1) *** (SF sf2) = SF $ \r -> do
    f1 <- sf1 r
    f2 <- sf2 r
    pure $ \(a, b) -> (,) <$> f1 a <*> f2 b
  (SF sf1) &&& (SF sf2) = SF $ \r -> do
    f1 <- sf1 r
    f2 <- sf2 r
    pure $ \a -> (,) <$> f1 a <*> f2 a
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (***) #-}
  {-# INLINE (&&&) #-}

withSetup :: (r1 -> IO (r2, x)) -> SF r2 (a, x) b -> SF r1 a b
withSetup setup (SF sf) = SF $ \r1 -> do
  (r2,x) <- setup r1 
  f <- sf r2
  pure $ \a -> f (a, x)
{-# INLINE withSetup #-}

setupSF :: SF r a b -> r -> IO (a -> IO b)
setupSF (SF sf) = sf
{-# INLINE setupSF #-}

arrIO :: (a -> IO b) -> SF r a b
arrIO f = SF $ \_ -> pure f
{-# INLINE arrIO #-}

useEnv :: (r -> a -> c) -> SF r c b -> SF r a b
useEnv f (SF sf) = SF $ \r -> do
  step <- sf r
  pure $ \a -> step (f r a)
{-# INLINE useEnv #-}

modifyEnv :: (r1 -> IO r2) -> SF r2 a b -> SF r1 a b
modifyEnv f (SF sf) = SF $ \r1 -> do
  r2 <- f r1
  sf r2
{-# INLINE modifyEnv #-}

unSF :: SF r a b -> r -> IO (a -> IO b)
unSF (SF sf) = sf
{-# INLINE unSF #-}
