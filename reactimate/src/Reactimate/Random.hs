module Reactimate.Random where

import Control.Arrow (Arrow (..))
import Reactimate.Basic
import Reactimate.Signal
import Reactimate.Stateful
import Reactimate.Union
import System.Random

-- | Generates a pseudo-random signal with the global RNG
generateRandom :: (IOE :> es, Random a) => Signal es x a
generateRandom = arrIO $ const randomIO

-- | Generates a pseudo-random signal in the given range
generateRandomRange :: (IOE :> es, Random a) => (a, a) -> Signal es x a
generateRandomRange range = arrIO $ const $ randomRIO range

-- | Generates a pseudo-random signal with the given generator
generateRandomWithRNG :: (RandomGen g, Random a) => g -> Signal es x a
generateRandomWithRNG g = feedbackState g $ arr (random . snd)

-- | Generates a pseudo-random signal in the given range with the given generator
generateRandomRangeWithRNG :: (Random a, RandomGen g) => g -> (a, a) -> Signal es x a
generateRandomRangeWithRNG g range = feedbackState g $ arr (randomR range . snd)
