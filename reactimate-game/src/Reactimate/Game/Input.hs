module Reactimate.Game.Input where

import Reactimate
import Reactimate.Signal (addFinalizer)
import SDL qualified

inputEvent :: Event SDL.Event
inputEvent = callback $ \fin fire -> do
  eventWatch <- SDL.addEventWatch fire
  addFinalizer fin $ SDL.delEventWatch eventWatch
