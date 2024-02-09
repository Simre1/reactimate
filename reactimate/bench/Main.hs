import Control.Arrow (Arrow (..))
import Control.Category ((>>>))
import Data.Foldable (Foldable (..))
import Data.MonadicStreamFunction qualified as MSF
import Data.MonadicStreamFunction.InternalCore qualified as MSF
import FRP.Yampa qualified as Y
import Gauge.Main
import Reactimate.Run qualified as Signal
import Reactimate.Stateful qualified as Signal
import Reactimate.Time qualified as Signal

count :: Int
count = 100000

-- this is not super realistic since Yampa does some time calculation as well
yampaCountBench :: IO ()
yampaCountBench = do
  Y.reactimate
    (pure ())
    (\_ -> pure (0, Just ()))
    (\_ !b -> if b == 0 then pure True else pure False)
    (Y.loopPre count (arr (\((), !x) -> (x - 1, x - 1))))

signalCountBench :: IO ()
signalCountBench = do
  !x <- Signal.reactimate $ Signal.feedbackState count (arr (\((), !x) -> (x - 1, x - 1))) >>> arr (\x -> if x == 0 then Just x else Nothing)
  pure ()

msfCountBench :: IO ()
msfCountBench = do
  !x <- reactimate $ MSF.feedback count (arr (\((), !x) -> (x - 1, x - 1))) >>> arr (\x -> if x == 0 then Just x else Nothing)
  pure ()
  where
    reactimate :: MSF.MSF IO () (Maybe a) -> IO a
    reactimate mSignal = do
      (b, next) <- MSF.unMSF mSignal ()
      case b of
        Nothing -> reactimate next
        Just x -> pure x

integrateSamples :: Int
integrateSamples = 1000000

yampaIntegrateBench :: Double -> Double
yampaIntegrateBench x = last (Y.embed (pure (x :: Double) >>> Y.integral) (Y.deltaEncode 0.1 [1 .. integrateSamples]))

signalIntegrateBench :: IO Double
signalIntegrateBench = 
  Signal.fold
      (\_ x -> x)
      0
      (Signal.withFixedTime 0.1 $ \time -> pure 1 >>> Signal.integrate time (*))
      [1 .. integrateSamples]

chainTest :: (Arrow a) => a Double Double
chainTest = foldl' (\a _ -> a >>> a) (arr (+ 1)) [0 .. 16]

yampaChainBench :: Double -> Double
yampaChainBench x = last $ Y.embed chainTest (x, [])

signalChainBench :: IO Double
signalChainBench = last <$> Signal.sample chainTest [0]

msfChainBench :: IO Double
msfChainBench = last <$> MSF.embed chainTest [0]

main :: IO ()
main = do
  defaultMain
    [ bgroup "Countdown benchmark" [bench "Yampa" $ nfIO yampaCountBench, bench "dunai" $ nfIO msfCountBench, bench "reactimate" $ nfIO signalCountBench],
      bgroup "Integrate benchmark" [bench "Yampa" $ nf yampaIntegrateBench 1, bench "reactimate" $ nfIO signalIntegrateBench],
      bgroup "Chaining (>>>) benchmark" [bench "Yampa" $ nf yampaChainBench 0, bench "dunai" $ nfIO msfChainBench, bench "reactimate" $ nfIO signalChainBench]
    ]
