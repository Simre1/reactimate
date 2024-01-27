{-# LANGUAGE OverloadedStrings #-}
module Main where

import Reactimate
import Reactimate.Game
import Control.Arrow
import Reactimate.Stateful (sumUp)


main :: IO ()
main = reactimate () $ setupGame (GameConfig "Basic Example" (V2 800 600)) (\_ -> GameEnv) $ 
	limitSampleRate (1/60) $ game >>> render (\(GameEnv window) -> window) >>> constant Nothing

data GameEnv = GameEnv {
	window :: Window
} 

game :: Signal GameEnv () (Camera, Picture)
game = constant 1 >>> sumUp >>> arr (\x -> (camera x, rectangle (V2 0 0)))

rectangle :: V2 Int -> Picture
rectangle position = makePicture 0 $ Rectangle position (V2 100 100) $ withOpacity red 1

camera :: Int -> Camera
camera x = Camera (V2 0 0) (V2 (800 + x') 600)
	where x' = abs $ (x `mod` 300) - 150

