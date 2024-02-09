module Reactimate.Random where

import Control.Arrow (Arrow (..))
import Reactimate.Basic
import Reactimate.Signal
import Reactimate.Stateful
import System.Random


-- | Generates a pseudo-random signal with the global RNG
generateRandom :: (Random a) => Signal x a
generateRandom = arrIO $ const randomIO

-- | Generates a pseudo-random signal in the given range
generateRandomRange :: (Random a) => (a, a) -> Signal x a
generateRandomRange range = arrIO $ const $ randomRIO range

-- | Generates a pseudo-random signal with the given generator
generateRandomWithRNG :: (RandomGen g, Random a) => g -> Signal x a
generateRandomWithRNG g = feedbackState g $ arr (random . snd)

-- | Generates a pseudo-random signal in the given range with the given generator
generateRandomRangeWithRNG :: (Random a, RandomGen g) => g -> (a, a) -> Signal x a
generateRandomRangeWithRNG g range = feedbackState g $ arr (randomR range . snd)
