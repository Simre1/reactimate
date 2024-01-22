{-# LANGUAGE TemplateHaskell #-}

import Control.Arrow (Arrow (..))
import Control.Category ((>>>))
import Data.IORef
import Data.MonadicStreamFunction qualified as MSF
import Data.MonadicStreamFunction.InternalCore qualified as MSF
import Data.SF.Combinators qualified as SF
import Data.SF.Core qualified as SF
import Data.SF.Hyperdrive qualified as Hyper
import Data.SF.Hyperdrive2 qualified as Hyper2
import Data.SF.Run qualified as SF
import Data.SF.Time qualified as SF
import Debug.Trace (traceShowId)
import FRP.Yampa qualified as Y
import GHC.Clock (getMonotonicTime)
import Test.BenchPress (bench, benchMany)
import Data.Foldable (Foldable(..))

countYampa :: Y.SF () Int
countYampa = Y.loopPre count (arr (\((), !x) -> (x - 1, x - 1)))

countSF :: SF.SF () () Int
countSF = SF.feedback count (arr (\((), !x) -> (x - 1, x - 1)))

countMSF :: MSF.MSF IO () Int
countMSF = MSF.feedback count (arr (\((), !x) -> (x - 1, x - 1)))

countHyper2 :: Hyper2.SF () () Int
countHyper2 = Hyper2.feedback count $ arr (\((), !x) -> (x - 1, x - 1))

count :: Int
count = 100000

-- this is not super realistic since Yampa does some time calculation as well
yampaCountBench :: IO ()
yampaCountBench = do
  Y.reactimate
    (pure ())
    (\_ -> pure (0, Just ()))
    (\_ !b -> if b == 0 then pure True else pure False)
    countYampa

sfCountBench :: IO ()
sfCountBench = do
  !x <- SF.reactimate (countSF >>> arr (\x -> if x == 0 then Just x else Nothing)) ()
  pure ()

hyper2CountBench :: IO ()
hyper2CountBench = do
  f <- Hyper2.compile (countHyper2 `Hyper2.next` arr (\x -> if x == 0 then Just x else Nothing)) ()
  !x <- Hyper2.reactimate (f ())
  pure ()

msfCountBench :: IO ()
msfCountBench = do
  !x <- reactimate $ countMSF >>> arr (\x -> if x == 0 then Just x else Nothing)
  pure ()
  where
    reactimate :: MSF.MSF IO () (Maybe a) -> IO a
    reactimate msf = do
      (b, next) <- MSF.unMSF msf ()
      case b of
        Nothing -> reactimate next
        Just x -> pure x

hyperCountBench :: IO ()
hyperCountBench = do
  !x <- Hyper.reactimate f
  pure undefined
  where
    f = $$(Hyper.compile Hyper.testFunction ())

integrateSamples :: Int
integrateSamples = 10000000

yampaIntegrateBench :: IO ()
yampaIntegrateBench = do
  !x <- pure $ last (Y.embed (pure (1 :: Double) >>> Y.integral) (Y.deltaEncode 0.1 [1 .. integrateSamples]))
  pure ()

newtype Env = Env
  { time :: SF.Time
  }

sfIntegrateBench :: IO ()
sfIntegrateBench = do
  !x <-
    SF.fold
      (\_ x -> x)
      0
      (SF.withFixedTime 0.1 (\_ t -> Env t) $ pure 1 >>> SF.integrate (*))
      ()
      [1 .. integrateSamples]
  pure ()

yampaChainBench :: IO ()
yampaChainBench = do
  let !x = head $ Y.embed chainTest (0, [])
  pure ()

sfChainBench :: IO ()
sfChainBench = do
  !x <- head <$> SF.sample chainTest () [0]
  pure ()

hyper2ChainBench :: IO ()
hyper2ChainBench = do
  f <- Hyper2.compile chainTest ()
  !x <- f 0
  pure ()

main :: IO ()
main = do
  putStrLn "Countdown benchmark"
  benchMany 1 [("Yampa", yampaCountBench), ("dunai", msfCountBench), ("fast-signals", sfCountBench), ("hyperdrive", hyperCountBench), ("hyperdrive2", hyper2CountBench)]
  putStrLn ""
  putStrLn "Integrate benchmark"
  benchMany 1 [("Yampa", yampaIntegrateBench), ("fast-signals", sfIntegrateBench)]
  putStrLn ""
  putStrLn "Chain benchmark"
  benchMany 20 [("Yampa", yampaChainBench), ("fast-signals", sfChainBench), ("hyper", hyper2ChainBench)]
  -- hyper2CountBench

chainTest :: Arrow a => a Double Double
chainTest = foldl' (\a _ -> a >>> a) (arr (+1)) [0..16]
-- hyperCountBench
