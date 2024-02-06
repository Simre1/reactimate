{-# LANGUAGE LambdaCase #-}
module Reactimate.Signal where

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
-- Notice that `Signal` is an instance of `Functor`, `Applicative` and `Arrow`!
newtype Signal a b = Signal (Finalizer -> IO (a -> IO b))

instance Functor (Signal a) where
  fmap f (Signal m) = Signal $ \fin -> fmap (fmap f .) (m fin)
  {-# INLINE fmap #-}

instance Applicative (Signal a) where
  pure a = Signal $ \_ -> pure $ \_ -> pure a
  (Signal signal1) <*> (Signal signal2) = Signal $ \fin -> do
    f1 <- signal1 fin 
    f2 <- signal2 fin 
    pure $ \a -> f1 a <*> f2 a
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Category Signal where
  id = Signal $ \_ -> pure $ \a -> pure a
  (Signal signal1) . (Signal signal2) = Signal $ \fin -> do
    f1 <- signal1 fin 
    f2 <- signal2 fin 
    pure $ f2 >=> f1
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance Arrow Signal where
  arr f = Signal $ \_ -> pure $ pure . f
  first (Signal signal) = Signal $ \fin -> do
    f <- signal fin 
    pure $ \(a, b) -> (,b) <$> f a
  second (Signal signal) = Signal $ \fin -> do
    f <- signal fin 
    pure $ \(a, b) -> (a,) <$> f b
  (Signal signal1) *** (Signal signal2) = Signal $ \fin -> do
    f1 <- signal1 fin 
    f2 <- signal2 fin 
    pure $ \(a, b) -> (,) <$> f1 a <*> f2 b
  (Signal signal1) &&& (Signal signal2) = Signal $ \fin -> do
    f1 <- signal1 fin 
    f2 <- signal2 fin 
    pure $ \a -> (,) <$> f1 a <*> f2 a
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (***) #-}
  {-# INLINE (&&&) #-}

instance ArrowChoice Signal where
  left signal = Signal $ \fin  -> do
    f <- unSignal signal fin 
    pure $ \case
      Left a -> Left <$> f a
      Right d -> pure $ Right d

  right signal = Signal $ \fin -> do
    f <- unSignal signal fin 
    pure $ \case
      Right a -> Right <$> f a
      Left d -> pure $ Left d

  signal1 +++ signal2 = Signal $ \fin -> do
    f1 <- unSignal signal1 fin 
    f2 <- unSignal signal2 fin 
    pure $ \case
      Left a -> Left <$> f1 a
      Right a -> Right <$> f2 a
  signal1 ||| signal2 = Signal $ \fin  -> do
    f1 <- unSignal signal1 fin 
    f2 <- unSignal signal2 fin 
    pure $ \case
      Left a -> f1 a
      Right a -> f2 a
  {-# INLINE left #-}
  {-# INLINE right #-}
  {-# INLINE (+++) #-}
  {-# INLINE (|||) #-}

-- | Unwrap a signal function and feed in the environment @r@. The outer `IO` is the setup, which produces the run action.
unSignal :: Signal a b -> Finalizer -> IO (a -> IO b)
unSignal (Signal signal) = signal
{-# INLINE unSignal #-}

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
