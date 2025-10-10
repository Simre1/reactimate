{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RoleAnnotations #-}

module Reactimate.Signal where

import Control.Arrow
import Control.Category
import Control.Exception (bracket)
import Control.Monad (join, (>=>))
import Control.Monad.Fix
import Control.Monad.Trans.Reader
import Data.Coerce
import Data.IORef
import Data.Kind (Type)
import GHC.IO (unsafePerformIO)
import GHC.IO.Unsafe (unsafeInterleaveIO)
import Reactimate.Union
import Unsafe.Coerce
import Prelude hiding (id, (.))

-- | A signal function takes @a@s and produces @b@, similar to a function @a -> b@.
-- However, it can also remember previous iterations and is perfect for building simulation/game loops.
--
-- Notice that `Signal` is an instance of `Functor`, `Applicative` and `Arrow`!
newtype Signal es a b = Signal (Setup es X (a -> Step X b))

newtype Step (s :: Type) a = Step {unStep :: IO a} deriving (Functor, Applicative, Monad)

data Context es s = Context
  { handles :: Union es,
    finalizer :: Finalizer s
  }

newtype Setup es (s :: Type) a = Setup {unSetup :: Context es s -> IO a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadFail) via ReaderT (Context es s) IO

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

-- | Unwrap a signal function. The outer setup happens only once and produces the step action.
unSignal :: Signal es a b -> Setup es s (a -> Step s b)
unSignal (Signal signal) = coerce signal
{-# INLINE unSignal #-}

newtype IOE s = IOE (forall a. IO a -> Step s a)

makeSignal :: (forall s. Setup es s (a -> Step s b)) -> Signal es a b
makeSignal f = Signal f

getHandle :: forall e es s. (Member e es) => Setup es s (e s)
getHandle = Setup $ \Context {handles} -> do
  let ex :: e X = getMember handles
  pure (unsafeCoerce ex)

finalize :: Setup es s () -> Setup es s ()
finalize release = Setup $ \ctx@Context {finalizer} ->
  addFinalizer finalizer $ unSetup release ctx
{-# INLINE finalize #-}

newtype Ref s a = Ref (IORef a)

newRef :: a -> Setup es s (Ref s a)
newRef a = Setup $ \_ -> Ref <$> newIORef a
{-# INLINE newRef #-}

writeRef :: Ref s a -> a -> Step s ()
writeRef (Ref ref) a = Step (writeIORef ref a)
{-# INLINE writeRef #-}

readRef :: Ref s a -> Step s a
readRef (Ref ref) = Step (readIORef ref)
{-# INLINE readRef #-}

modifyRef :: Ref s a -> (a -> a) -> Step s ()
modifyRef (Ref ref) f = Step (modifyIORef ref f)
{-# INLINE modifyRef #-}

modifyRef' :: Ref s a -> (a -> a) -> Step s ()
modifyRef' (Ref ref) f = Step (modifyIORef' ref f)
{-# INLINE modifyRef' #-}

atomicModifyRef' :: Ref s a -> (a -> (a, b)) -> Step s b
atomicModifyRef' (Ref ref) f = Step (atomicModifyIORef' ref f)
{-# INLINE atomicModifyRef' #-}

runPureSetup :: Setup '[] s a -> a
runPureSetup (Setup f) = unsafePerformIO $ withFinalizer $ \fin -> f (Context EmptyUnion fin)

runSetup :: Setup '[IOE] s a -> IO a
runSetup (Setup f) = withFinalizer $ \fin -> f (Context (ConsUnion (IOE Step) EmptyUnion) fin)

runHandle :: e s -> Setup (e : es) s a -> Setup es s a
runHandle e (Setup f) = Setup $ \Context {handles, finalizer} ->
  f (Context (ConsUnion (unsafeCoerce e) handles) finalizer)

mapSignal :: (forall s. Setup es s (a -> Step s b) -> Setup es2 s (c -> Step s d)) -> Signal es a b -> Signal es2 c d
mapSignal f (Signal setup) = Signal $ f setup

mapEffects :: (forall s x. Setup es s x -> Setup es2 s x) -> Signal es a b -> Signal es2 a b
mapEffects f (Signal setup) = Signal $ f setup

prestep :: Step s a -> Setup es s a
prestep (Step io) = Setup (\_ -> io)

data Switch s es a b = Switch (IORef (a -> Step s b)) (IORef (Context es s))

newSwitch :: (Setup es s (a -> Step s b)) -> Setup es s (Switch s es a b)
newSwitch signal = Setup $ \ctx@Context {handles, finalizer = globalFinalizer} -> do
  initialFin <- newFinalizer
  initialF <- unSetup signal ctx

  signalRef <- newIORef initialF
  contextRef <- newIORef $ Context handles initialFin
  addFinalizer
    globalFinalizer
    (readIORef contextRef >>= runFinalizer . (\Context {finalizer} -> finalizer))

  let switch = Switch signalRef contextRef
  -- (Signal (, x) <- unSetup (f switch) globalFin
  pure switch

updateSwitch :: Switch s es a b -> Signal es a b -> Step s ()
updateSwitch (Switch signalRef contextRef) (Signal (Setup makeNewSignal)) = Step $ do
  newFin <- newFinalizer
  Context {handles, finalizer = oldFin} <- readIORef contextRef
  runFinalizer oldFin
  newF <- makeNewSignal (Context handles newFin)
  writeIORef signalRef $ coerce newF
  readIORef contextRef >>= runFinalizer . (\Context {finalizer} -> finalizer)
  writeIORef contextRef (Context handles $ coerce newFin)

runSwitch :: Switch s es a b -> a -> Step s b
runSwitch (Switch signalRef _) a = do
  f <- Step $ readIORef signalRef
  f a
{-# INLINE runSwitch #-}

-- mapEffects :: (Eff es1 b -> Eff es2 c) -> Signal es1 a b -> Signal es2 a c
-- mapEffects mapper (Signal signal) = Signal $ do
--   f <- signal
--   pure $ mapper . f
-- {-# INLINE mapEffects #-}

-- | A `Finalizer` contains some clean-up code.
-- Usually, they are run when the execution of the signal function stops
newtype Finalizer s = Finalizer (IORef (IO ()))

-- | Add a clean-up function to a `Finalizer`
addFinalizer :: Finalizer s -> IO () -> IO ()
addFinalizer (Finalizer ref) fin = modifyIORef' ref (fin >>)
{-# INLINE addFinalizer #-}

-- | Make a new `Finalizer`
newFinalizer :: IO (Finalizer s)
newFinalizer = Finalizer <$> newIORef (pure ())
{-# INLINE newFinalizer #-}

-- | Run the clean-up code from finalizer
runFinalizer :: Finalizer s -> IO ()
runFinalizer (Finalizer ref) = join $ readIORef ref
{-# INLINE runFinalizer #-}

-- | Run some code with a `Finalizer` and then run the clean-up code.
--
-- You should not return the `Finalizer` and run it again. This has already been done.
withFinalizer :: (Finalizer s -> IO a) -> IO a
withFinalizer =
  bracket
    newFinalizer
    runFinalizer
{-# INLINE withFinalizer #-}
