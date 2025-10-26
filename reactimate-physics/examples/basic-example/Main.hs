{-# LANGUAGE OverloadedStrings #-}

import Data.Bool (bool)
import Data.Colour.Names (black, blue, red)
import Data.Vector.Storable qualified as VS
import Reactimate
import Reactimate.Game
import Reactimate.Physics2D

main :: IO ()
main =
  runSetup $
    reactimate $
      runGame "Physics example" defaultWindow 60 $
        mapEffects (runPhysics mempty) $
          game
            >>> render
            >>> renderGame
            >>> bool Nothing (Just ())
            <$> gameShouldQuit

game :: (Physics :> es) => Signal es () (V2 Double, V2 Double)
game = makeSignal $ do
  space <- getSpace
  physics <- getPhysics
  (body1, body2) <- prestep $ do
    writeVar physics.gravity (V2 0 (-200))

    body1 <- addDynamicBody space 1 (1 / 0) -- 1 / 0 -> locks rotation due to infinite inertia
    writeVar body1.position (V2 250 300)
    shape1 <- addBoxShape body1 (V2 100 100) 0
    writeVar shape1.friction (0.3)

    body2 <- addDynamicBody space 1 (1 / 0)
    writeVar body2.position (V2 350 500)
    shape2 <- addBoxShape body2 (V2 100 100) 0
    writeVar shape2.friction (0.3)

    let static = space.staticBody
    writeVar static.position (V2 300 50)
    ground <- addBoxShape static (V2 600 100) 0
    writeVar ground.friction (0.5)

    pure (body1, body2)

  pure $ \_ -> do
    _ <- stepPhysics physics (1 / 60)
    p1 <- readVar body1.position
    p2 <- readVar body2.position
    pure (p1, p2)

render :: Signal es (V2 Double, V2 Double) (Camera, Picture)
render =
  arr $ \(pos1, pos2) ->
    ( Camera (V2 0 (-100)) (V2 800 600),
      makePicture 0 $ do
        drawRectangle (packColour red) $ Rectangle (round <$> pos1 - V2 50 50) (V2 100 100)
        drawRectangle (packColour blue) $ Rectangle (round <$> pos2 - V2 50 50) (V2 100 100)
        drawRectangle (packColour black) $ Rectangle (V2 0 0) (V2 600 100)
    )
