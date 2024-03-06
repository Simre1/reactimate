{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow
import Data.Bool (bool)
import Data.Colour.Names
import Reactimate
import Reactimate.Game
import Reactimate.Stateful (sumUp)

main :: IO ()
main =
  reactimate $
    setupGame (GameConfig "Basic Example" defaultWindow 60) $ \gameEnv ->
      game >>> renderGame gameEnv >>> bool Nothing (Just ()) <$> sampleBehavior (gameShouldQuit gameEnv)

game :: Signal () (Camera, Picture)
game = constant 0.01 >>> sumUp >>> arr (\x -> (camera, shapes x))

shapes :: Float -> Picture
shapes x =
  translatePicture (V2 400 300) $
    rotatePicture (2 * pi * x) $
      makePicture 0 $ do
        drawRectangle (packColour red) $
          Rectangle (V2 (-50) (-50)) (V2 100 100)
        drawRectangle (packColour blue) $
          Rectangle (V2 200 (-50)) (V2 100 100)

camera :: Camera
camera = Camera (V2 0 0) (V2 800 600)
