{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Data.Colour.Names
import Data.Vector.Storable qualified as VS
import Reactimate
import Reactimate.Game
import Test.Tasty.Bench

renderRectangles :: GameEnv -> Signal Int ()
renderRectangles gameEnv = constant (camera, picture) >>> renderGame gameEnv
  where
    rectsAmount = 1000
    camera = Camera (V2 0 300) (V2 rectsAmount 20)
    picture = makePicture 0 $ forM_ [1 .. rectsAmount] $ \x ->
      drawRectangle (packColour black) $ Rectangle (V2 x 300) (V2 10 10)

main :: IO ()
main = do
  Test.Tasty.Bench.defaultMain
    [ bench "Render rectangles" $
        nfIO $
          sample (setupGame (GameConfig "Bench" defaultWindow maxBound) renderRectangles) [0 .. 360]
    ]
