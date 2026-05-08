{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeAbstractions #-}

module Reactimate.Classical where
import Reactimate.Signal
import Reactimate.Basic (identity)
import Control.Monad ((>=>), forever)
import Control.Concurrent.Async (async, cancel)
import Reactimate.Handles
import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent


-- runSetup $ sample (arrIO (\_ -> threadDelay (10^6)) >>> sampleEventAsList (pulseEvent 0.3 10) >>> arrIO print) [(), ()]

-- | Events are like @Signal () a@, they produce values of @a@ and require no input.
-- Events occure at some unknown time, so they cannot simply be sampled with run functions like 'reactimate'.
-- Instead, you can think of @Event r a@ as a @Signal r () a@ which is whenever the event happens, completely independent from the main loop.
data Event es a where
  Event ::
    { signal :: !(Signal es x a),
      hook :: forall s. (x -> Step s ()) -> Setup es s ()
    } ->
    Event es a

-- | A `Dynamic` changes it's value over time based on an `Event`. They always have a value and you can get an `Event` to determine when this happens.
data Dynamic es a = Dynamic
  { event :: !(Event es a),
    initialValue :: !a
  }

-- | A `Behavior` changes it's value over time. However, you cannot know exactly when this happens. You cannot get an `Event` from a `Behavior`.
newtype Behavior es a = Behavior (Signal es () a) deriving (Functor, Applicative)

instance Functor (Event es) where
  fmap f (Event signal hook) = Event (fmap f signal) hook
  {-# INLINE fmap #-}

instance Semigroup (Event es a) where
  (Event signal1 hook1) <> (Event signal2 hook2) = Event identity $ \push -> do
    f1 <- unSignal signal1
    f2 <- unSignal signal2
    hook1 (f1 >=> push)
    hook2 (f2 >=> push)

instance Monoid (Event es a) where
  mempty = Event identity $ \_ -> pure ()

instance Functor (Dynamic es) where
  fmap f (Dynamic event a) = Dynamic (fmap f event) (f a)

instance Applicative (Dynamic es) where
  pure = Dynamic mempty
  liftA2
    f
    (Dynamic (Event signalA hookA) initialValueA)
    (Dynamic (Event signalB hookB) initialValueB) = Dynamic event (f initialValueA initialValueB)
      where
        event = Event identity $ \trigger -> do
          refA <- newRef initialValueA
          refB <- newRef initialValueB

          fA <- unSignal signalA
          fB <- unSignal signalB

          hookA $ \x -> do
            a <- fA x
            writeRef refA a
            b <- readRef refB
            trigger $ f a b
          hookB $ \x -> do
            b <- fB x
            writeRef refB b
            a <- readRef refA
            trigger $ f a b

-- | Emits an event and then waits @frameTime@ seconds.
-- If producing events is little work, this should approximate a frequency @1/frameTime@.
pulseEvent :: IOE :> es => Double -> a -> Event es a
pulseEvent frameTime a = Event identity $ \push -> do
  unliftIO <- unliftStep 
  asyncRef <- liftIO $ async $ forever $ do
    unliftIO $ push a
    threadDelay $ round (frameTime * 1000000)
  finalize (cancel asyncRef)

-- | Trigger exactly one event as soon as you sample
instantEvent :: a -> Event es a
instantEvent a = Event identity $ \push -> do
  prestep $ push a

-- | Create an event from a callback. The first argument should take a @a -> IO ()@ function and use it to trigger events.
--
-- You can also use the environment, though you need to keep thread-safety in mind. If you need to run some clean-up code,
-- add it with `finalize`.
--
-- @
-- callback $ \\triggerEvent -> do
--   cleanUp <- someFunctionTakingCallback triggerEvent
--   pure cleanUp
-- @
callback :: (forall s. (a -> Step s ()) -> Setup es s ()) -> Event es a
callback = Event identity

-- | Fold an event over time and return the latest value.
--
-- __The accumulator carries over to the next sampling step.__
accumulateEvent :: (b -> a -> b) -> b -> Event es a -> Signal es () b
accumulateEvent accumulate initial (Event signal hook) = makeSignal $ mdo
  ref <- newRef initial
  f <- unSignal signal
  hook $ \x -> do
    a <- f x
    modifyRef' ref (`accumulate` a)
  pure $ \_ -> readRef ref

-- | Fold all events which happened since the last sample.
--
-- __The accumulator will reset to the initial value at each sampling.__
sampleEvent :: (b -> a -> b) -> b -> Event es a -> Signal es () b
sampleEvent accumulate initial (Event signal hook) = makeSignal $ mdo
  ref <- newRef initial
  f <- unSignal signal 
  hook $ \x -> do
    a <- f x
    modifyRef' ref (`accumulate` a)
  pure $ \_ -> atomicModifyRef' ref (initial,)

-- | Get the most recent inner `Event` of the outer `Event`. This fires an event when the most recent inner `Event` fires.
-- switchEvents :: Event es (Event es a) -> Event es a
-- switchEvents (Event signal hook) = Event identity $ \trigger -> do
--   -- <- getAllHandles
--   makeEvent <- unSignal signal 
--   innerFinRef <- prestep (Step newFinalizer) >>= newRef
--   hook $ \x -> do
--     (Event innerSignal innerHook) <- makeEvent x

--     readRef innerFinRef >>= Step . runFinalizer
--     innerFin <- Step newFinalizer
--     writeRef innerFinRef innerFin
--     makeA <- case unSignal innerSignal of
--       Setup f -> Setup $ \ctx -> f ctx

--     innerHook innerFin $ \y -> do
--       makeA y >>= trigger

--   addFinalizer fin $
--     readIORef innerFinRef >>= runFinalizer

-- -- | Switch a `Dynamic`. Contrary to `switchEvents`, this will also trigger an event when `Event` switches to a new Dynamic.
-- switchDynamics :: Event es (Dynamic es a) -> Event es a
-- switchDynamics (Event signal hook) = Event identity $ \fin trigger -> do
--   makeEvent <- unSignal signal fin
--   innerFinRef <- newFinalizer >>= newIORef
--   hook fin $ \x -> do
--     (Dynamic (Event innerSignal innerHook) initialValue) <- makeEvent x

--     readIORef innerFinRef >>= runFinalizer
--     innerFin <- newFinalizer
--     writeIORef innerFinRef innerFin
--     makeA <- unSignal innerSignal innerFin

--     trigger initialValue

--     innerHook innerFin $ \y -> do
--       makeA y >>= trigger

--   addFinalizer fin $
--     readIORef innerFinRef >>= runFinalizer

-- -- | Get the currently active `Event`.
-- joinEvents :: Dynamic es (Event es a) -> Event es a
-- joinEvents (Dynamic (Event outerSignal outerHook) (Event startSignal startHook)) = Event identity $ \fin trigger -> do
--   makeEvent <- unSignal outerSignal fin

--   startFin <- newFinalizer
--   innerFinRef <- newIORef startFin

--   startF <- unSignal startSignal startFin
--   startHook startFin (startF >=> trigger)

--   outerHook fin $ \x -> do
--     (Event innerSignal innerHook) <- makeEvent x

--     readIORef innerFinRef >>= runFinalizer
--     innerFin <- newFinalizer
--     writeIORef innerFinRef innerFin
--     makeA <- unSignal innerSignal innerFin

--     innerHook innerFin $ \y -> do
--       makeA y >>= trigger

--   addFinalizer fin $
--     readIORef innerFinRef >>= runFinalizer

-- -- | Join a `Dynamic` to get the inner `Dynamic`.
-- joinDynamic :: Dynamic es (Dynamic es a) -> Dynamic es a
-- joinDynamic @es @a (Dynamic (Event outerSignal outerHook) (Dynamic (Event startSignal startHook) startValue)) = Dynamic joinedEvent startValue
--   where
--     joinedEvent :: Event es a
--     joinedEvent = Event identity $ \fin trigger -> do
--       makeEvent <- unSignal outerSignal fin

--       startFin <- newFinalizer
--       innerFinRef <- newIORef startFin
--       startF <- unSignal startSignal startFin

--       startHook startFin (startF >=> trigger)

--       outerHook fin $ \x -> do
--         (Dynamic (Event innerSignal innerHook) value) <- makeEvent x

--         trigger value

--         readIORef innerFinRef >>= runFinalizer
--         innerFin <- newFinalizer
--         writeIORef innerFinRef innerFin
--         makeA <- unSignal innerSignal innerFin

--         innerHook innerFin $ \y -> do
--           makeA y >>= trigger

--       addFinalizer fin $
--         readIORef innerFinRef >>= runFinalizer

-- | Grab all unseen events as a list
sampleEventAsList :: Event es a -> Signal es () [a]
sampleEventAsList = fmap ($ []) . sampleEvent (\f a -> f . (a :)) id

-- -- | Map a signal function over an event.
-- mapEvent :: Signal es a b -> Event es a -> Event es b
-- mapEvent signal2 (Event signal1 hook) = Event (signal1 >>> signal2) hook

-- -- | Sample a `Dynamic`.
-- sampleDynamic :: Dynamic es a -> Signal es () a
-- sampleDynamic (Dynamic event initialValue) = makeSignal $ \r -> unSignal (sampleEvent (\_ x -> x) initialValue event) r

-- -- | Sample a `Behavior`.
-- sampleBehavior :: Behavior es a -> Signal es () a
-- sampleBehavior (Behavior signal) = signal

-- -- | Map a signal function over a `Behavior`.
-- mapBehavior :: Signal es a b -> Behavior es a -> Behavior es b
-- mapBehavior signal2 (Behavior signal1) = Behavior $ signal1 >>> signal2

-- -- | Hold the value of an `Event` to a `Dynamic` with initial value @a@.
-- holdEvent :: a -> Event es a -> Dynamic es a
-- holdEvent a event = Dynamic event a

-- -- | Extracts the `Event` from a `Dynamic`
-- dynamicToEvent :: Dynamic es a -> Event es a
-- dynamicToEvent (Dynamic event _) = event

-- -- | Get a `Behavior` from a `Dynamic`
-- dynamicToBehavior :: Dynamic es a -> Behavior es a
-- dynamicToBehavior (Dynamic (Event signal hook) initialValue) = makeBehavior $ makeSignal $ \fin -> do
--   f <- unSignal signal fin

--   ref <- newIORef initialValue

--   hook fin (f >=> writeIORef ref)

--   pure $ \_ ->
--     readIORef ref

-- -- | Make a `Behavior`
-- makeBehavior :: Signal es () a -> Behavior es a
-- makeBehavior = Behavior
