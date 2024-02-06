{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow (Arrow (..))
import Control.Category ((>>>))
import Data.Vector.Storable qualified as VS
import Debug.Trace (traceShowId)
import Reactimate
import Reactimate.Game

main :: IO ()
main = reactimate $ limitSampleRate 60 $ setupGame (GameConfig "Mouse example" defaultWindow) $ \gameEnv ->
  constant camera >>> mousePosition gameEnv >>> game >>> render gameEnv >>> constant Nothing

camera :: Camera
camera = Camera (V2 0 0) (V2 800 600)

game :: Signal (V2 Int) (Camera, Picture)
game = arr $ \pos ->
  let picture =
        makePicture 0 $
          BasicShapes $
            VS.singleton (ColouredShape (packColour blue) $ BSRectangle $ Rectangle pos (V2 100 100))
   in (camera, picture)
