{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Vector.Storable qualified as VS
import Reactimate
import Reactimate.Game
import Data.Colour.Names

main :: IO ()
main = reactimate $ setupGame (GameConfig "Mouse example" defaultWindow 60) $ \gameEnv ->
  constant camera >>> mousePosition gameEnv >>> game >>> renderGame gameEnv >>> constant Nothing

camera :: Camera
camera = Camera (V2 0 0) (V2 800 600)

picture :: V2 Int -> Picture
picture pos =
  makePicture 0 $
    BasicShapes $
      VS.singleton (ColouredShape (packColour blue) $ BSRectangle $ Rectangle pos (V2 100 100))

game :: Signal (V2 Int) (Camera, Picture)
game = arr $ \pos -> (camera, picture pos)
