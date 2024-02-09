{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow
import Data.Vector.Storable qualified as VS
import Reactimate
import Reactimate.Game
import Reactimate.Stateful (sumUp)
import Data.Colour.Names

main :: IO ()
main =
  reactimate $
    setupGame (GameConfig "Basic Example" defaultWindow 60) $ \gameEnv ->
        game >>> renderGame gameEnv >>> constant Nothing

game :: Signal () (Camera, Picture)
game = constant 1 >>> sumUp >>> arr (\x -> (camera, shapes x))

shapes :: Int -> Picture
shapes x =
  makePicture 0 $
    BasicShapes $
      VS.fromList
        [ ColouredShape (packColour red) $
            BSRectangle $
              Rectangle (V2 xMod100 xMod360) (V2 100 100),
          ColouredShape (packColour blue) $
            BSEllipse $
              Ellipse (V2 300 400) (V2 xMod100 (xMod360 `quot` 2)),
          ColouredShape (packColour green) $
            BSTriangle $
              Triangle (V2 (500 + xMod360) 100) (V2 700 xMod100) (V2 (700 + xMod100) (400 + xMod360)),
          ColouredShape (packColour yellow) $
            BSCircularArc $
              CircularArc (V2 350 100) 100 0 x
        ]
  where
    xMod100 = abs $ x `mod` 200 - 100
    xMod360 = abs $ x `mod` 720 - 360

camera :: Camera
camera = Camera (V2 0 0) (V2 800 600)
