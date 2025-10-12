module Reactimate.Random
  ( RNG,
    generateRandom,
    generateRandomRange,
    runRNGWithIO,
    runRNGWithGenerator,
    generateRandomWithGenerator,
    generateRandomRangeWithGenerator,
  )
where

import Control.Arrow (Arrow (..))
import Data.Tuple (swap)
import Reactimate.Basic
import Reactimate.Handles
import Reactimate.Setup
import Reactimate.Signal
import Reactimate.Stateful
import System.Random

-- | An effect for random numbers, utilizing the `random` package
newtype RNG s = RNG {randomGen :: Ref s StdGen}

-- | Generates a pseudo-random signal via the RNG effect. Impacts other users of RNG.
generateRandom :: (RNG :> es, Random a) => Signal es x a
generateRandom = arrStep $ \(Handle (RNG rngRef)) _ -> atomicModifyRef' rngRef (swap . random)

-- | Generates a pseudo-random signal in the given range via the RNG effect. Impacts other users of RNG.
generateRandomRange :: (RNG :> es, Random a) => (a, a) -> Signal es x a
generateRandomRange range = arrStep $ \(Handle (RNG rngRef)) _ -> atomicModifyRef' rngRef (swap . randomR range)

-- | Run the RNG effect with the standard generator from IO
runRNGWithIO :: (IOE :> es) => Signal (RNG : es) a b -> Signal es a b
runRNGWithIO signal =
  withSetup
    initStdGen
    (flip runRNGWithGenerator signal)

-- | Run the RNG effect with an initial random generator
runRNGWithGenerator :: (RandomGen g) => g -> Signal (RNG : es) a b -> Signal es a b
runRNGWithGenerator rng = mapEffects $ \setup -> do
  rngRef <- newRef stdGen
  runHandle (RNG rngRef) setup
  where
    stdGen = mkStdGen $ fst (random rng)

-- | Generates a local pseudo-random signal with the given generator. This sequence is not affected by other random generators.
generateRandomWithGenerator :: (RandomGen g, Random a) => g -> Signal es x a
generateRandomWithGenerator g = feedbackState g $ arr (random . snd)

-- | Generates a local pseudo-random signal in the given range with the given generator. This sequence is not affected by other random generators.
generateRandomRangeWithGenerator :: (Random a, RandomGen g) => g -> (a, a) -> Signal es x a
generateRandomRangeWithGenerator g range = feedbackState g $ arr (randomR range . snd)
