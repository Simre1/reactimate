{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Bool (bool)
import Data.Colour.Names
import Data.Vector.Storable qualified as VS
import Reactimate
import Reactimate.Game

main :: IO ()
main = reactimate $ setupGame (GameConfig "Mouse example" defaultWindow 60) $ \gameEnv ->
  constant camera
    >>> mousePosition gameEnv
    >>> game
    >>> renderGame gameEnv
    >>> bool Nothing (Just ())
    <$> sampleBehavior (gameShouldQuit gameEnv)

camera :: Camera
camera = Camera (V2 0 0) (V2 800 600)

picture :: V2 Int -> Picture
picture pos =
  makePicture 0 $
    drawRectangle (packColour blue) $ Rectangle pos (V2 100 100)

game :: Signal (V2 Int) (Camera, Picture)
game = arr $ \pos -> (camera, picture pos)
