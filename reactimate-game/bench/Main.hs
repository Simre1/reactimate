{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow
import Data.Vector.Storable qualified as VS
import Gauge (Config (timeLimit))
import Gauge.Main
import Gauge.Main.Options (defaultConfig)
import Reactimate
import Reactimate.Game
import Reactimate.Game.Shapes

renderRectangles :: Signal Window Int ()
renderRectangles = constant (camera, picture) >>> render id
  where
    rectsAmount = 1000
    camera = Camera (V2 100 300) (V2 rectsAmount 100)
    picture = makePicture 0 $ BasicShapes $ VS.generate rectsAmount $ \x -> ColouredShape (packColour black) $ BSRectangle $ Rectangle (V2 x 300) (V2 10 10)

main :: IO ()
main = do
  defaultMainWith
    defaultConfig {timeLimit = Just 10}
    [ bench "Render rectangles" $
        nfIO $
          sample () (setupGame (GameConfig "Bench" (V2 800 600)) (\_ x -> x) renderRectangles) [0 .. 360]
    ]