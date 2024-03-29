{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Vector.Storable qualified as VS
import Gauge (Config (timeLimit))
import Gauge.Main
import Gauge.Main.Options (defaultConfig)
import Reactimate
import Reactimate.Game
import Data.Colour.Names
import Control.Monad (forM_)

renderRectangles :: GameEnv -> Signal Int ()
renderRectangles gameEnv = constant (camera, picture) >>> renderGame gameEnv
  where
    rectsAmount = 1000
    camera = Camera (V2 0 300) (V2 rectsAmount 20)
    picture = makePicture 0 $ forM_ [1..rectsAmount] $ \x -> 
      drawRectangle (packColour black) $ Rectangle (V2 x 300) (V2 10 10)

main :: IO ()
main = do
  defaultMainWith
    defaultConfig {timeLimit = Just 10}
    [ bench "Render rectangles" $
        nfIO $
          sample (setupGame (GameConfig "Bench" defaultWindow maxBound) renderRectangles) [0 .. 360]
    ]
