{-# LANGUAGE RecursiveDo #-}

module Reactimate.Event (Event (..), getEvents, isEvent, isNoEvent, filterE) where

import Control.Arrow (Arrow (..))
import Reactimate.Signal

newtype Event a = Event [a] deriving (Functor)

instance Semigroup (Event a) where
  (Event ev1) <> (Event ev2) = Event (ev1 <> ev2)

instance Monoid (Event a) where
  mempty = Event []

instance Foldable Event where
  foldMap f = foldMap f . getEvents

instance Traversable Event where
  traverse f = fmap Event . traverse f . getEvents

getEvents :: Event a -> [a]
getEvents (Event events) = events

isEvent :: Event a -> Bool
isEvent (Event []) = False
isEvent _ = True

isNoEvent :: Event a -> Bool
isNoEvent = not . isEvent

filterE :: (a -> Bool) -> Event a -> Event a
filterE f (Event events) = Event $ filter f events
