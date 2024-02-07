{-# LANGUAGE RecursiveDo #-}

module Reactimate.Event where

import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (forever, (>=>))
import Data.IORef
import Reactimate.Basic (identity)
import Reactimate.Signal

-- | Events are like @Signal () a@, they produce values of @a@ and require no input.
-- Events occure at some unknown time, so they cannot simply be sampled with run functions like 'reactimate'.
-- Instead, you can think of @Event r a@ as a @Signal r () a@ which is whenever the event happens, completely independent from the main loop.
data Event a where
  Event ::
    { signal :: !(Signal x a),
      hook :: Finalizer -> (x -> IO ()) -> IO ()
    } ->
    Event a

-- | Dynamics change their value over time. They always have a value, so they can be sampled whenever you want.
data Dynamic a = Dynamic
  { event :: !(Event a),
    initialValue :: !a
  }

newtype Behavior a = Behavior (Signal () a) deriving (Functor, Applicative)

instance Functor Event where
  fmap f (Event signal hook) = Event (fmap f signal) hook
  {-# INLINE fmap #-}

instance Semigroup (Event a) where
  (Event signal1 hook1) <> (Event signal2 hook2) = Event identity $ \fin push -> do
    f1 <- unSignal signal1 fin
    f2 <- unSignal signal2 fin
    hook1 fin (f1 >=> push)
    hook2 fin (f2 >=> push)

instance Monoid (Event a) where
  mempty = Event identity $ \_ _ -> pure ()

instance Functor Dynamic where
  fmap f (Dynamic event a) = Dynamic (fmap f event) (f a)

instance Applicative Dynamic where
  pure = Dynamic mempty
  liftA2
    f
    (Dynamic (Event signalA hookA) initialValueA)
    (Dynamic (Event signalB hookB) initialValueB) = Dynamic event (f initialValueA initialValueB)
      where
        event = Event identity $ \fin trigger -> do
          refA <- newIORef initialValueA
          refB <- newIORef initialValueB

          fA <- unSignal signalA fin
          fB <- unSignal signalB fin

          hookA fin $ \x -> do
            a <- fA x
            writeIORef refA a
            b <- readIORef refB
            trigger $ f a b
          hookB fin $ \x -> do
            b <- fB x
            writeIORef refB b
            a <- readIORef refA
            trigger $ f a b

-- | Emits an event and then waits @frameTime@ seconds.
-- If producing events is little work, this should approximate a frequency @1/frameTime@.
pulse :: Double -> a -> Event a
pulse frameTime a = Event identity $ \fin push -> do
  asyncRef <- async $ forever $ do
    push a
    threadDelay $ round (frameTime * 1000000)
  addFinalizer fin $ cancel asyncRef

-- | Create an event from a callback. The first argument should take a @a -> IO ()@ function and use it to trigger events.
--
-- You can also use the environment, though you need to keep thread-safety in mind. If you need to run some clean-up code,
-- add it to the `Finalizer`.
--
-- @
-- callback $ \\triggerEvent -> do
--   cleanUp <- someFunctionTakingCallback triggerEvent
--   pure cleanUp
-- @
callback :: (Finalizer -> (a -> IO ()) -> IO ()) -> Event a
callback = Event identity

-- | Fold an event over time and return the latest value.
--
-- __The accumulator carries over to the next sampling step.__
accumulateEvent :: (b -> a -> b) -> b -> Event a -> Signal () b
accumulateEvent accumulate initial (Event signal hook) = Signal $ \fin -> mdo
  ref <- newIORef initial
  f <- unSignal signal fin
  hook fin $ \x -> do
    a <- f x
    modifyIORef' ref (`accumulate` a)
  pure $ \_ -> readIORef ref

-- | Fold all events which happened since the last sample.
--
-- __The accumulator will reset to the initial value at each sampling.__
sampleEvent :: (b -> a -> b) -> b -> Event a -> Signal () b
sampleEvent accumulate initial (Event signal hook) = Signal $ \fin -> mdo
  ref <- newIORef initial
  f <- unSignal signal fin
  hook fin $ \x -> do
    a <- f x
    modifyIORef' ref (`accumulate` a)
  pure $ \_ -> atomicModifyIORef' ref (initial,)

-- | Grab all unseen events as a list
sampleEventAsList :: Event a -> Signal () [a]
sampleEventAsList = fmap ($ []) . sampleEvent (\f a -> f . (a :)) id

-- | Map a signal function over an event.
mapEvent :: Signal a b -> Event a -> Event b
mapEvent signal2 (Event signal1 hook) = Event (signal1 >>> signal2) hook

-- | Sample a `Dynamic`.
sampleDynamic :: Dynamic a -> Signal () a
sampleDynamic (Dynamic event initialValue) = Signal $ \r -> unSignal (sampleEvent (\_ x -> x) initialValue event) r

-- | Sample a `Behavior`.
sampleBehavior :: Behavior a -> Signal () a
sampleBehavior (Behavior signal) = signal

-- | Map a signal function over a `Behavior`.
mapBehavior :: Signal a b -> Behavior a -> Behavior b
mapBehavior signal2 (Behavior signal1) = Behavior $ signal1 >>> signal2

-- | Hold the value of an `Event` to a `Dynamic` with initial value @a@.
holdEvent :: a -> Event a -> Dynamic a
holdEvent a event = Dynamic event a

makeBehavior :: Signal () a -> Behavior a
makeBehavior = Behavior
