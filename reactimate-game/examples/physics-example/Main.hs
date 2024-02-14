{-# LANGUAGE OverloadedStrings #-}

import Data.Bool (bool)
import Data.Colour.Names (black, blue, red)
import Data.Vector.Storable qualified as VS
import Reactimate
import Reactimate.Game

main :: IO ()
main = reactimate $ setupGame (GameConfig "Physics example" defaultWindow 60) $ \gameEnv ->
  withPhysics $ \space ->
    withSetup
      ( do
          space.gravity $= V2 0 (-200)

          body1 <- addDynamicBody space 1 (1 / 0) -- 1 / 0 -> locks rotation due to infinite inertia
          body1.position $= V2 250 300
          shape1 <- addBoxShape space body1 100 100 0
          shape1.friction $= 0.3

          body2 <- addDynamicBody space 1 (1 / 0)
          body2.position $= V2 350 500
          shape2 <- addBoxShape space body2 100 100 0
          shape2.friction $= 0.3

          static <- get space.staticBody
          static.position $= V2 300 50
          ground <- addBoxShape space static 600 100 0
          ground.friction $= 0.5

          pure (body1, body2)
      )
      $ \(body1, body2) ->
        actionIO (spaceStep space (1 / 60))
          >>> arrIO (\_ -> (,) <$> get body1.position <*> get body2.position)
          >>> render
          >>> renderGame gameEnv
          >>> bool Nothing (Just ())
          <$> sampleBehavior (shouldQuit gameEnv)

render :: Signal (V2 Double, V2 Double) (Camera, Picture)
render =
  arr $ \(pos1, pos2) ->
    ( Camera (V2 0 (-100)) (V2 800 600),
      makePicture 0 $
        BasicShapes $
          VS.fromList
            [ ColouredShape (packColour red) (BSRectangle $ Rectangle (round <$> pos1 - V2 50 50) (V2 100 100)),
              ColouredShape (packColour blue) (BSRectangle $ Rectangle (round <$> pos2 - V2 50 50) (V2 100 100)),
              ColouredShape (packColour black) (BSRectangle $ Rectangle (V2 0 0) (V2 600 100))
            ]
    )
