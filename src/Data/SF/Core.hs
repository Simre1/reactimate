module Data.SF.Core where

import Control.Arrow
import Control.Category
import Control.Exception (bracket)
import Control.Monad (join, (>=>))
import Data.IORef
import Prelude hiding (id, (.))

-- | A signal function takes @a@s and produces @b@. They can also do IO.
-- Typically, you would repeatedly get input and run the signal function continously it.
--
-- The @r@ argument is the environment for the setup phase. It may be used similar to the ReaderT pattern.
--
-- Signal functions have a __setup__ and a __run__ phase:
--
-- 1. The setup phase is run once at the beginning and produces a run function
-- 2. The run phase is run as often as you want
--
-- Notice that `SF` is an instance of `Functor`, `Applicative` and `Arrow`!
newtype SF r a b = SF (Finalizer -> r -> IO (a -> IO b))

instance Functor (SF r a) where
  fmap f (SF m) = SF $ \fin r -> fmap (fmap f .) (m fin r)
  {-# INLINE fmap #-}

instance Applicative (SF r a) where
  pure a = SF $ \_ _ -> pure $ \_ -> pure a
  (SF sf1) <*> (SF sf2) = SF $ \fin r -> do
    f1 <- sf1 fin r
    f2 <- sf2 fin r
    pure $ \a -> f1 a <*> f2 a
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Category (SF r) where
  id = SF $ \_ _ -> pure $ \a -> pure a
  (SF sf1) . (SF sf2) = SF $ \fin r -> do
    f1 <- sf1 fin r
    f2 <- sf2 fin r
    pure $ f2 >=> f1
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance Arrow (SF r) where
  arr f = SF $ \_ _ -> pure $ pure . f
  first (SF sf) = SF $ \fin r -> do
    f <- sf fin r
    pure $ \(a, b) -> (,b) <$> f a
  second (SF sf) = SF $ \fin r -> do
    f <- sf fin r
    pure $ \(a, b) -> (a,) <$> f b
  (SF sf1) *** (SF sf2) = SF $ \fin r -> do
    f1 <- sf1 fin r
    f2 <- sf2 fin r
    pure $ \(a, b) -> (,) <$> f1 a <*> f2 b
  (SF sf1) &&& (SF sf2) = SF $ \fin r -> do
    f1 <- sf1 fin r
    f2 <- sf2 fin r
    pure $ \a -> (,) <$> f1 a <*> f2 a
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (***) #-}
  {-# INLINE (&&&) #-}

instance ArrowChoice (SF r) where
  left sf = SF $ \fin r -> do
    f <- unSF sf fin r
    pure $ \ad -> case ad of
      Left a -> Left <$> f a
      Right d -> pure $ Right d

  right sf = SF $ \fin r -> do
    f <- unSF sf fin r
    pure $ \ad -> case ad of
      Right a -> Right <$> f a
      Left d -> pure $ Left d

  sf1 +++ sf2 = SF $ \fin r -> do
    f1 <- unSF sf1 fin r
    f2 <- unSF sf2 fin r
    pure $ \bb -> case bb of
      Left a -> Left <$> f1 a
      Right a -> Right <$> f2 a
  sf1 ||| sf2 = SF $ \fin r -> do
    f1 <- unSF sf1 fin r
    f2 <- unSF sf2 fin r
    pure $ \bb -> case bb of
      Left a -> f1 a
      Right a -> f2 a
  {-# INLINE left #-}
  {-# INLINE right #-}
  {-# INLINE (+++) #-}
  {-# INLINE (|||) #-}

-- | Unwrap a signal function and feed in the environment @r@. The outer `IO` is the setup, which produces the run action.
unSF :: SF r a b -> Finalizer -> r -> IO (a -> IO b)
unSF (SF sf) = sf
{-# INLINE unSF #-}

-- | A `Finalizer` contains some clean-up code.
-- Usually, they are run when the execution of the signal function stops
newtype Finalizer = Finalizer (IORef (IO ()))

-- | Add a clean-up function to a `Finalizer`
addFinalizer :: Finalizer -> IO () -> IO ()
addFinalizer (Finalizer ref) fin = modifyIORef' ref (>> fin)
{-# INLINE addFinalizer #-}

-- | Make a new `Finalizer`
newFinalizer :: IO Finalizer
newFinalizer = Finalizer <$> newIORef (pure ())
{-# INLINE newFinalizer #-}

-- | Run the clean-up code from finalizer
runFinalizer :: Finalizer -> IO ()
runFinalizer (Finalizer ref) = join $ readIORef ref
{-# INLINE runFinalizer #-}

-- | Run some code with a `Finalizer` and then run the clean-up code.
--
-- You should not return the `Finalizer` and run it again. This has already been done.
withFinalizer :: (Finalizer -> IO a) -> IO a
withFinalizer =
  bracket
    newFinalizer
    runFinalizer
{-# INLINE withFinalizer #-}
