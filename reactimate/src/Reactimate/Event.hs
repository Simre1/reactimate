{-# LANGUAGE RecursiveDo #-}

module Reactimate.Event where

import Control.Applicative (Alternative, liftA2)
import Control.Arrow ((>>>))
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (forever, (>=>))
import Data.IORef
import Reactimate.Basic (identity)
import Reactimate.Signal

newtype Event a = Event [a] deriving (Functor)

-- -- | Events are like @Signal () a@, they produce values of @a@ and require no input.
-- -- Events occure at some unknown time, so they cannot simply be sampled with run functions like 'reactimate'.
-- -- Instead, you can think of @Event r a@ as a @Signal r () a@ which is whenever the event happens, completely independent from the main loop.
-- data Event a where
--   Event ::
--     { signal :: !(Signal x a),
--       hook :: Finalizer -> (x -> IO ()) -> IO ()
--     } ->
--     Event a

-- -- | A `Dynamic` changes it's value over time based on an `Event`. They always have a value and you can get an `Event` to determine when this happens.
-- data Dynamic a = Dynamic
--   { event :: !(Event a),
--     initialValue :: !a
--   }

-- -- | A `Behavior` changes it's value over time. However, you cannot know exactly when this happens. You cannot get an `Event` from a `Behavior`.
-- newtype Behavior a = Behavior (Signal () a) deriving (Functor, Applicative)

-- instance Functor Event where
--   fmap f (Event signal hook) = Event (fmap f signal) hook
--   {-# INLINE fmap #-}

-- instance Semigroup (Event a) where
--   (Event signal1 hook1) <> (Event signal2 hook2) = Event identity $ \fin push -> do
--     f1 <- unSignal signal1 fin
--     f2 <- unSignal signal2 fin
--     hook1 fin (f1 >=> push)
--     hook2 fin (f2 >=> push)

-- instance Monoid (Event a) where
--   mempty = Event identity $ \_ _ -> pure ()

-- instance Functor Dynamic where
--   fmap f (Dynamic event a) = Dynamic (fmap f event) (f a)

-- instance Applicative Dynamic where
--   pure = Dynamic mempty
--   liftA2
--     f
--     (Dynamic (Event signalA hookA) initialValueA)
--     (Dynamic (Event signalB hookB) initialValueB) = Dynamic event (f initialValueA initialValueB)
--       where
--         event = Event identity $ \fin trigger -> do
--           refA <- newIORef initialValueA
--           refB <- newIORef initialValueB

--           fA <- unSignal signalA fin
--           fB <- unSignal signalB fin

--           hookA fin $ \x -> do
--             a <- fA x
--             writeIORef refA a
--             b <- readIORef refB
--             trigger $ f a b
--           hookB fin $ \x -> do
--             b <- fB x
--             writeIORef refB b
--             a <- readIORef refA
--             trigger $ f a b

-- -- | Emits an event and then waits @frameTime@ seconds.
-- -- If producing events is little work, this should approximate a frequency @1/frameTime@.
-- pulseEvent :: Double -> a -> Event a
-- pulseEvent frameTime a = Event identity $ \fin push -> do
--   asyncRef <- async $ forever $ do
--     push a
--     threadDelay $ round (frameTime * 1000000)
--   addFinalizer fin $ cancel asyncRef

-- -- | Trigger exactly one event as soon as you sample
-- instantEvent :: a -> Event a
-- instantEvent a = Event identity $ \_ push -> do
--   push a

-- -- | Create an event from a callback. The first argument should take a @a -> IO ()@ function and use it to trigger events.
-- --
-- -- You can also use the environment, though you need to keep thread-safety in mind. If you need to run some clean-up code,
-- -- add it to the `Finalizer`.
-- --
-- -- @
-- -- callback $ \\triggerEvent -> do
-- --   cleanUp <- someFunctionTakingCallback triggerEvent
-- --   pure cleanUp
-- -- @
-- callback :: (Finalizer -> (a -> IO ()) -> IO ()) -> Event a
-- callback = Event identity

-- -- | Fold an event over time and return the latest value.
-- --
-- -- __The accumulator carries over to the next sampling step.__
-- accumulateEvent :: (b -> a -> b) -> b -> Event a -> Signal () b
-- accumulateEvent accumulate initial (Event signal hook) = Signal $ \fin -> mdo
--   ref <- newIORef initial
--   f <- unSignal signal fin
--   hook fin $ \x -> do
--     a <- f x
--     modifyIORef' ref (`accumulate` a)
--   pure $ \_ -> readIORef ref

-- -- | Fold all events which happened since the last sample.
-- --
-- -- __The accumulator will reset to the initial value at each sampling.__
-- sampleEvent :: (b -> a -> b) -> b -> Event a -> Signal () b
-- sampleEvent accumulate initial (Event signal hook) = Signal $ \fin -> mdo
--   ref <- newIORef initial
--   f <- unSignal signal fin
--   hook fin $ \x -> do
--     a <- f x
--     modifyIORef' ref (`accumulate` a)
--   pure $ \_ -> atomicModifyIORef' ref (initial,)

-- -- | Get the most recent inner `Event` of the outer `Event`. This fires an event when the most recent inner `Event` fires.
-- switchEvents :: Event (Event a) -> Event a
-- switchEvents (Event signal hook) = Event identity $ \fin trigger -> do
--   makeEvent <- unSignal signal fin
--   innerFinRef <- newFinalizer >>= newIORef
--   hook fin $ \x -> do
--     (Event innerSignal innerHook) <- makeEvent x

--     readIORef innerFinRef >>= runFinalizer
--     innerFin <- newFinalizer
--     writeIORef innerFinRef innerFin
--     makeA <- unSignal innerSignal innerFin

--     innerHook innerFin $ \y -> do
--       makeA y >>= trigger

--   addFinalizer fin $
--     readIORef innerFinRef >>= runFinalizer

-- -- | Switch a `Dynamic`. Contrary to `switchEvents`, this will also trigger an event when `Event` switches to a new Dynamic.
-- switchDynamics :: forall a. Event (Dynamic a) -> Event a
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
-- joinEvents :: forall a. Dynamic (Event a) -> Event a
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
-- joinDynamic :: forall a. Dynamic (Dynamic a) -> Dynamic a
-- joinDynamic (Dynamic (Event outerSignal outerHook) (Dynamic (Event startSignal startHook) startValue)) = Dynamic joinedEvent startValue
--   where
--     joinedEvent :: Event a
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

-- -- | Grab all unseen events as a list
-- sampleEventAsList :: Event a -> Signal () [a]
-- sampleEventAsList = fmap ($ []) . sampleEvent (\f a -> f . (a :)) id

-- -- | Map a signal function over an event.
-- mapEvent :: Signal a b -> Event a -> Event b
-- mapEvent signal2 (Event signal1 hook) = Event (signal1 >>> signal2) hook

-- -- | Sample a `Dynamic`.
-- sampleDynamic :: Dynamic a -> Signal () a
-- sampleDynamic (Dynamic event initialValue) = Signal $ \r -> unSignal (sampleEvent (\_ x -> x) initialValue event) r

-- -- | Sample a `Behavior`.
-- sampleBehavior :: Behavior a -> Signal () a
-- sampleBehavior (Behavior signal) = signal

-- -- | Map a signal function over a `Behavior`.
-- mapBehavior :: Signal a b -> Behavior a -> Behavior b
-- mapBehavior signal2 (Behavior signal1) = Behavior $ signal1 >>> signal2

-- -- | Hold the value of an `Event` to a `Dynamic` with initial value @a@.
-- holdEvent :: a -> Event a -> Dynamic a
-- holdEvent a event = Dynamic event a

-- -- | Extracts the `Event` from a `Dynamic`
-- dynamicToEvent :: Dynamic a -> Event a
-- dynamicToEvent (Dynamic event _) = event

-- -- | Get a `Behavior` from a `Dynamic`
-- dynamicToBehavior :: Dynamic a -> Behavior a
-- dynamicToBehavior (Dynamic (Event signal hook) initialValue) = makeBehavior $ Signal $ \fin -> do
--   f <- unSignal signal fin

--   ref <- newIORef initialValue

--   hook fin (f >=> writeIORef ref)

--   pure $ \_ ->
--     readIORef ref

-- -- | Make a `Behavior`
-- makeBehavior :: Signal () a -> Behavior a
-- makeBehavior = Behavior
