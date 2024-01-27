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
-- Instead, @Event r a@ is a @Signal r () a@ which is sampled independently from the main loop when the event happens.
data Event r a where
  Event ::
    { signal :: !(Signal r x a),
      hook :: Finalizer -> r -> (x -> IO ()) -> IO ()
    } ->
    Event r a

-- | Behaviors change their value over time. They always have a value, so they can be sampled whenever you want.
data Behavior r a = Behavior
  { event :: !(Event r a),
    initialValue :: !a
  }

instance Functor (Event r) where
  fmap f (Event signal hook) = Event (fmap f signal) hook
  {-# INLINE fmap #-}

instance Semigroup (Event r a) where
  (Event signal1 hook1) <> (Event signal2 hook2) = Event identity $ \fin r push -> do
    f1 <- unSignal signal1 fin r
    f2 <- unSignal signal2 fin r
    hook1 fin r (f1 >=> push)
    hook2 fin r (f2 >=> push)

instance Monoid (Event r a) where
  mempty = Event identity $ \_ _ _ -> pure ()

instance Functor (Behavior r) where
  fmap f (Behavior event a) = Behavior (fmap f event) (f a)

instance Applicative (Behavior r) where
  pure = Behavior mempty
  liftA2
    f
    (Behavior (Event signalA hookA) initialValueA)
    (Behavior (Event signalB hookB) initialValueB) = Behavior event (f initialValueA initialValueB)
      where
        event = Event identity $ \fin r trigger -> do
          refA <- newIORef initialValueA
          refB <- newIORef initialValueB

          fA <- unSignal signalA fin r
          fB <- unSignal signalB fin r

          hookA fin r $ \x -> do
            a <- fA x
            writeIORef refA a
            b <- readIORef refB
            trigger $ f a b
          hookB fin r $ \x -> do
            b <- fB x
            writeIORef refB b
            a <- readIORef refA
            trigger $ f a b

-- | Emits an event and then waits @frameTime@ seconds.
-- If producing events is little work, this should approximate a frequency @1/frameTime@.
pulse :: Double -> a -> Event r a
pulse frameTime a = Event identity $ \fin _ push -> do
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
callback :: (Finalizer -> r -> (a -> IO ()) -> IO ()) -> Event r a
callback = Event identity

-- | Fold an event over time and return the latest value.
--
-- __The accumulator carries over to the next sampling step.__
accumulateEvent :: (b -> a -> b) -> b -> Event r a -> Signal r () b
accumulateEvent accumulate initial (Event signal hook) = Signal $ \fin r -> mdo
  ref <- newIORef initial
  f <- unSignal signal fin r
  hook fin r $ \x -> do
    a <- f x
    modifyIORef' ref (`accumulate` a)
  pure $ \_ -> readIORef ref

-- | Fold all events which happened since the last sample.
--
-- __The accumulator will reset to the initial value at each sampling.__
sampleEvent :: (b -> a -> b) -> b -> Event r a -> Signal r () b
sampleEvent accumulate initial (Event signal hook) = Signal $ \fin r -> mdo
  ref <- newIORef initial
  f <- unSignal signal fin r
  hook fin r $ \x -> do
    a <- f x
    modifyIORef' ref (`accumulate` a)
  pure $ \_ -> atomicModifyIORef' ref (initial,)

-- | Grab all unseen events as a list
sampleEventAsList :: Event r a -> Signal r () [a]
sampleEventAsList = fmap ($ []) . sampleEvent (\f a -> f . (a :)) id

-- | Map a signal function over an event.
eventMap :: Signal r a b -> Event r a -> Event r b
eventMap signal2 (Event signal1 hook) = Event (signal1 >>> signal2) hook

-- | Sample a `Behavior`.
sampleBehavior :: Behavior r a -> Signal r () a
sampleBehavior (Behavior event initialValue) = Signal $ \r -> do
  unSignal (sampleEvent (\_ x -> x) initialValue event) r

-- | Hold the value of an `Event` to a `Behavior` with initial value @a@.
holdEvent :: a -> Event r a -> Behavior r a
holdEvent a event = Behavior event a
