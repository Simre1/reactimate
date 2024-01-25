module Data.SF.Core where

import Control.Arrow
import Control.Category
import Control.Exception (bracket)
import Control.Monad (join, (>=>))
import Data.IORef
import Prelude hiding (id, (.))

-- | A signal function takes @a@s and produces @b@.
-- Signal functions have a __setup__ and a __run__ phase:
--
-- 1. The setup phase is run once at the beginning and produces a run function
-- 2. The run phase is run as often as you want
--
-- The @r@ argument is the environment for the setup phase. It may be used similar to the ReaderT pattern.
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

-- | Do some setup before any signal functions actually run.
-- The setup action will be run **once** before any signal functions produce outputs.
withSetup :: (r1 -> IO (r2, x)) -> SF r2 (a, x) b -> SF r1 a b
withSetup setup (SF sf) = SF $ \fin r1 -> do
  (r2, x) <- setup r1
  f <- sf fin r2
  pure $ \a -> f (a, x)
{-# INLINE withSetup #-}

-- | Run an IO action during a signal function.
arrIO :: (a -> IO b) -> SF r a b
arrIO f = SF $ \_ _ -> pure f
{-# INLINE arrIO #-}

-- | Access the environment when signal functions run
useEnv :: (r -> a -> c) -> SF r c b -> SF r a b
useEnv f (SF sf) = SF $ \fin r -> do
  step <- sf fin r
  pure $ \a -> step (f r a)
{-# INLINE useEnv #-}

-- | Modify the enviroment during the setup phase.
modifyEnv :: (r1 -> IO r2) -> SF r2 a b -> SF r1 a b
modifyEnv f (SF sf) = SF $ \fin r1 -> do
  r2 <- f r1
  sf fin r2
{-# INLINE modifyEnv #-}

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

-- | Make a new `Finalizer`
newFinalizer :: IO Finalizer
newFinalizer = Finalizer <$> newIORef (pure ())

-- | Run the clean-up code from finalizer
runFinalizer :: Finalizer -> IO ()
runFinalizer (Finalizer ref) = join $ readIORef ref

-- | Run some code with a `Finalizer` and then run the clean-up code.
--
-- You should not return the `Finalizer` and run it again. This has already been done.
withFinalizer :: (Finalizer -> IO a) -> IO a
withFinalizer =
  bracket
    newFinalizer
    runFinalizer
