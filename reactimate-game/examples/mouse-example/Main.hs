{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Bool (bool)
import Data.Colour.Names
import Reactimate
import Reactimate.Game

main :: IO ()
main =
  runSetup $
    reactimate $
      runGame "Mouse example" defaultWindow 60 $
        game
          >>> renderGame
          >>> bool Nothing (Just ())
          <$> gameShouldQuit

camera :: Camera
camera = Camera (V2 0 0) (V2 800 600)

picture :: V2 Int -> Picture
picture pos =
  makePicture 0 $
    drawRectangle (packColour blue) $
      Rectangle pos (V2 100 100)

game :: (Input :> es) => Signal es () (Camera, Picture)
game = constant camera >>> mousePosition >>> arr (\pos -> (camera, picture pos))
