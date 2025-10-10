{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module Reactimate.Signal where

import Control.Arrow
import Control.Category
import Control.Exception (bracket)
import Control.Monad (join, (>=>))
import Control.Monad.Trans.Reader
import Data.IORef
import Effectful (Eff, IOE, runEff, runPureEff)
import Effectful.Dispatch.Static (unEff, unsafeEff, unsafeEff_)
import GHC.IO.Unsafe (unsafeInterleaveIO)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (id, (.))

-- | A signal function takes @a@s and produces @b@, similar to a function @a -> b@.
-- However, it can also remember previous iterations and is perfect for building simulation/game loops.
--
-- Notice that `Signal` is an instance of `Functor`, `Applicative` and `Arrow`!
newtype Signal es a b = Signal (Setup (a -> Eff es b))

newtype Setup a = Setup {unSetup :: Finalizer -> IO a}
  deriving (Functor, Applicative, Monad, MonadFail) via ReaderT Finalizer IO

instance Functor (Signal es a) where
  fmap f (Signal m) = Signal $ fmap (fmap f .) m
  {-# INLINE fmap #-}

instance Applicative (Signal es a) where
  pure a = Signal $ pure $ \_ -> pure a
  (Signal signal1) <*> (Signal signal2) = Signal $ do
    f1 <- signal1
    f2 <- signal2
    pure $ \a -> f1 a <*> f2 a
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Category (Signal es) where
  id = Signal $ pure $ \a -> pure a
  (Signal signal1) . (Signal signal2) = Signal $ do
    f1 <- signal1
    f2 <- signal2
    pure $ f2 >=> f1
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance Arrow (Signal es) where
  arr f = Signal $ pure $ pure . f
  first (Signal signal) = Signal $ do
    f <- signal
    pure $ \(a, b) -> (,b) <$> f a
  second (Signal signal) = Signal $ do
    f <- signal
    pure $ \(a, b) -> (a,) <$> f b
  (Signal signal1) *** (Signal signal2) = Signal $ do
    f1 <- signal1
    f2 <- signal2
    pure $ \(a, b) -> (,) <$> f1 a <*> f2 b
  (Signal signal1) &&& (Signal signal2) = Signal $ do
    f1 <- signal1
    f2 <- signal2
    pure $ \a -> (,) <$> f1 a <*> f2 a
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (***) #-}
  {-# INLINE (&&&) #-}

instance ArrowChoice (Signal es) where
  left signal = Signal $ do
    f <- unSignal signal
    pure $ \case
      Left a -> Left <$> f a
      Right d -> pure $ Right d

  right signal = Signal $ do
    f <- unSignal signal
    pure $ \case
      Right a -> Right <$> f a
      Left d -> pure $ Left d

  signal1 +++ signal2 = Signal $ do
    f1 <- unSignal signal1
    f2 <- unSignal signal2
    pure $ \case
      Left a -> Left <$> f1 a
      Right a -> Right <$> f2 a
  signal1 ||| signal2 = Signal $ do
    f1 <- unSignal signal1
    f2 <- unSignal signal2
    pure $ \case
      Left a -> f1 a
      Right a -> f2 a
  {-# INLINE left #-}
  {-# INLINE right #-}
  {-# INLINE (+++) #-}
  {-# INLINE (|||) #-}

-- | Unwrap a signal function. The outer setup happens only once and produces the step action in `Eff`.
unSignal :: Signal es a b -> Setup (a -> Eff es b)
unSignal (Signal signal) = signal
{-# INLINE unSignal #-}

-- | Do not leak 'Ref' from 'Setup'
newtype Ref s a = Ref (IORef a)

withRef :: a -> (forall s. Ref s a -> Setup x) -> Setup x
withRef a f = do
  ref <- Setup $ \_ -> Ref <$> newIORef a
  f ref
{-# INLINE withRef #-}

writeRef :: Ref s a -> a -> Eff es ()
writeRef (Ref ref) a = unsafeEff_ (writeIORef ref a)
{-# INLINE writeRef #-}

readRef :: Ref s a -> Eff es a
readRef (Ref ref) = unsafeEff_ (readIORef ref)
{-# INLINE readRef #-}

modifyRef :: Ref s a -> (a -> a) -> Eff es ()
modifyRef (Ref ref) f = unsafeEff_ (modifyIORef ref f)
{-# INLINE modifyRef #-}

modifyRef' :: Ref s a -> (a -> a) -> Eff es ()
modifyRef' (Ref ref) f = unsafeEff_ (modifyIORef' ref f)
{-# INLINE modifyRef' #-}

atomicModifyRef' :: Ref s a -> (a -> (a, b)) -> Eff es b
atomicModifyRef' (Ref ref) f = unsafeEff_ (atomicModifyIORef' ref f)
{-# INLINE atomicModifyRef' #-}

runSetup :: Setup (Eff es a) -> Eff es a
runSetup (Setup f) = do
  eff <- unsafeEff_ $ withFinalizer $ \fin -> f fin
  eff
{-# INLINE runSetup #-}

data Switch s es a b = Switch (IORef (a -> Eff es b)) (IORef Finalizer)

withSwitch :: (forall s. Switch s es a b -> Setup (Signal es a b, x)) -> Setup x
withSwitch f = Setup $ \globalFin -> mdo
  initialFin <- newFinalizer
  initialF <- unsafeInterleaveIO $ makeInitialSignal initialFin

  signalRef <- newIORef initialF
  finalizerRef <- newIORef initialFin
  addFinalizer globalFin (readIORef finalizerRef >>= runFinalizer)

  let switch = Switch signalRef finalizerRef
  (Signal (Setup (makeInitialSignal)), x) <- unSetup (f switch) globalFin
  pure x

updateSwitch :: Switch s es1 a b -> Signal es1 a b -> Eff es2 ()
updateSwitch (Switch signalRef finalizerRef) (Signal (Setup makeNewSignal)) = unsafeEff_ $ do
  newFin <- newFinalizer
  newF <- makeNewSignal newFin
  writeIORef signalRef newF
  readIORef finalizerRef >>= runFinalizer
  writeIORef finalizerRef newFin

runSwitch :: Switch s es a b -> a -> Eff es b
runSwitch (Switch signalRef _) a = do
  f <- unsafeEff_ $ readIORef signalRef
  f a
{-# INLINE runSwitch #-}

mapEffects :: (Eff es1 b -> Eff es2 c) -> Signal es1 a b -> Signal es2 a c
mapEffects mapper (Signal signal) = Signal $ do
  f <- signal
  pure $ mapper . f
{-# INLINE mapEffects #-}

-- | A `Finalizer` contains some clean-up code.
-- Usually, they are run when the execution of the signal function stops
newtype Finalizer = Finalizer (IORef (IO ()))

-- | Add a clean-up function to a `Finalizer`
addFinalizer :: Finalizer -> IO () -> IO ()
addFinalizer (Finalizer ref) fin = modifyIORef' ref (fin >>)
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

getFinalizer :: Setup (Eff es () -> Eff es ())
getFinalizer = Setup $ \fin ->
  pure $ \eff -> unsafeEff (\env -> addFinalizer fin $ unEff eff env)
{-# INLINE getFinalizer #-}
