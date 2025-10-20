{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}

module Reactimate.Signal
  ( -- * Signal
    Signal,
    Step,
    Setup,
    unSignal,
    makeSignal,
    inheritScope,
    finalize,
    prestep,
    unliftStep,

    -- * Mutable references
    Ref,
    newRef,
    writeRef,
    readRef,
    modifyRef,
    modifyRef',
    atomicModifyRef,
    atomicModifyRef',

    -- * Switches
    Switch,
    newSwitch,
    updateSwitch,
    runSwitch,

    -- * Effects
    getHandle,
    getHandles,
    IOE (..),
    runHandle,
    replaceHandle,

    -- * Run Setup
    runPureSetup,
    runSetup,
    mapEffects,
  )
where

import Control.Arrow
import Control.Category
import Control.Exception (bracket)
import Control.Monad (join, (>=>))
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Coerce
import Data.IORef
import Data.Kind (Type)
import Data.Void
import GHC.IO (unsafePerformIO)
import Reactimate.Handles
import Unsafe.Coerce
import Prelude hiding (id, (.))

-- | A signal function takes @a@s and produces @b@, similar to a function @a -> b@.
-- However, it can also remember previous iterations and is perfect for building simulation/game loops.
--
-- 'Signal' is internally split into a 'Setup' phase which initializes the simulation and a 'Step' phase which runs in each iteration.
--
-- Notice that `Signal` is an instance of `Functor`, `Applicative` and `Arrow`!
newtype Signal es a b = Signal (Setup es Void (a -> Step Void b))

-- | 'Step' is an action which happens in each iteration of the simulation. The 's' is for scoping ('makeSignal').
newtype Step (s :: Type) a = Step {unStep :: IO a} deriving (Functor, Applicative, Monad)

-- | The 'Setup' context.
data Context es s = Context
  { handles :: Handles es s,
    finalizer :: Finalizer s
  }

-- | 'Setup' is an action which initializes the simulation. For example, you can setup a 'Ref' and then use it in 'Step'.
-- It is also possible to get the effect handles tracked in 'es' with 'getHandle'.
--
-- The 's' is for scoping ('makeSignal').
newtype Setup es (s :: Type) a = Setup {unSetup :: Context es s -> IO a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadFail) via ReaderT (Context es s) IO

instance (IOE :> es) => MonadIO (Setup es s) where
  liftIO io = do
    IOE lift <- getHandle
    prestep $ lift io

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

-- | Create a new signal from a setup action which produces a step function.
-- This needs to be done in a self-contained scope, hence the 'forall s' (ST trick). In particular, 'Ref's cannot leak into signal arguments.
makeSignal :: (forall s. Setup es s (a -> Step s b)) -> Signal es a b
makeSignal f = Signal f

-- | Inherits the scope for creating signals. This can be useful when you need to mix functions which take 'Signal's with 'Setup' code.
inheritScope :: Setup es s ((Setup es s (a -> Step s b)) -> Signal es a b)
inheritScope = pure (unsafeCoerce Signal)

-- | Unwrap a signal function. A signal is a 'Setup' action which produces the 'Step' function.
unSignal :: Signal es a b -> Setup es s (a -> Step s b)
unSignal (Signal signal) = unsafeCoerce signal
{-# INLINE unSignal #-}

-- | The 'IO' effect. Only signals with an 'IOE' in their effect list can execute 'IO' operations.
newtype IOE s = IOE (forall a. IO a -> Step s a)

-- | Get an effect handle if it is within the effect list.
getHandle :: (Member e es) => Setup es s (e s)
getHandle = Setup $ \Context {handles} -> pure $ getMember handles

-- | Get multiplie effect handles if they are all within the effect list.
getHandles :: (Members sub es) => Setup es s (Handles sub s)
getHandles = Setup $ \Context {handles} -> pure $ getMembers handles

-- | Register an action for finalization. Finalization happens when a signal gets switched out or the whole simulation terminates.
finalize :: (IOE :> es) => IO () -> Setup es s ()
finalize release = Setup $ \Context {finalizer} ->
  addFinalizer finalizer $ release
{-# INLINE finalize #-}

-- | A mutable reference which can be used to persist state over iterations. This is based on 'IORef', so not necessarily thread-safe.
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

atomicModifyRef :: Ref s a -> (a -> (a, b)) -> Step s b
atomicModifyRef (Ref ref) f = Step (atomicModifyIORef ref f)
{-# INLINE atomicModifyRef #-}

atomicModifyRef' :: Ref s a -> (a -> (a, b)) -> Step s b
atomicModifyRef' (Ref ref) f = Step (atomicModifyIORef' ref f)
{-# INLINE atomicModifyRef' #-}

-- | Run a pure 'Setup' action without any effects. Look at 'reactimate' or 'sample' to run signal functions.
runPureSetup :: (forall s. Setup '[] s a) -> a
runPureSetup (Setup f) = unsafePerformIO $ withFinalizer $ \fin -> f (Context NoHandles fin)

-- | Run a 'Setup' with 'IO'.
runSetup :: (forall s. Setup '[IOE] s a) -> IO a
runSetup (Setup f) = withFinalizer $ \fin -> f (Context (ConsHandle (IOE Step) NoHandles) fin)

-- | Provide a single effect handle. The effect 'e' can be used within the 'Setup' and the 'Step's which were set up as part of that 'Setup'.
runHandle :: e s -> Setup (e : es) s a -> Setup es s a
runHandle e (Setup f) = Setup $ \Context {handles, finalizer} ->
  f (Context (ConsHandle e handles) finalizer)

-- | Replace a single effect handle.
replaceHandle :: (Member e es) => e s -> Setup es s a -> Setup es s a
replaceHandle e (Setup f) = Setup $ \Context {handles, finalizer} ->
  f (Context (replaceMember e handles) finalizer)

-- | Map the effects of a signal. Use it in combination with 'runHandle'.
mapEffects :: (forall s x. Setup es s x -> Setup es2 s x) -> Signal es a b -> Signal es2 a b
mapEffects f (Signal setup) = Signal $ f setup

-- | Run a 'Step' combination in the 'Setup' phase.
prestep :: Step s a -> Setup es s a
prestep (Step io) = Setup (\_ -> io)

-- | Unlift a 'Step' into an 'IO' action.
unliftStep :: (IOE :> es) => Setup es s (Step s a -> IO a)
unliftStep = pure (\(Step io) -> io)

-- | A 'Switch' represents a signal function which can be switched out.
data Switch s es a b = Switch (IORef (a -> Step s b)) (IORef (Context es s))

-- | Creates a new 'Switch' based on a 'Setup' with a 'Step' function. It essentially takes a 'Signal' with a scope due to the shared 's'.
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
  pure switch

-- | Updates the signal within a 'Switch'. As soon as you update:
-- - The 'Setup' of that new signal is executed
-- - The old signal is finalized
-- - 'runSwitch' will use the new signal.
updateSwitch :: Switch s es a b -> (Setup es s (a -> Step s b)) -> Step s ()
updateSwitch (Switch signalRef contextRef) signal = Step $ do
  newFin <- newFinalizer
  Context {handles, finalizer = oldFin} <- readIORef contextRef
  runFinalizer oldFin
  newF <- unSetup signal (Context handles newFin)
  writeIORef signalRef newF
  readIORef contextRef >>= runFinalizer . (\Context {finalizer} -> finalizer)
  writeIORef contextRef (Context handles $ coerce newFin)

-- | Run the active signal for the 'Switch'.
runSwitch :: Switch s es a b -> a -> Step s b
runSwitch (Switch signalRef _) a = do
  f <- Step $ readIORef signalRef
  f a
{-# INLINE runSwitch #-}

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
