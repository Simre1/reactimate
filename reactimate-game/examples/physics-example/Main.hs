{-# LANGUAGE OverloadedStrings #-}

import Chiphunk.Low qualified as C
import Data.Colour.Names (black, blue, red)
import Data.Vector.Storable qualified as VS
import Reactimate
import Reactimate.Game

main :: IO ()
main = reactimate $ setupGame (GameConfig "Physics example" defaultWindow 60) $ \gameEnv ->
  withPhysics $ \space -> withBodies
    space
    [ CreateDynamicBody 100 $ momentForBox 100 100 100,
      CreateDynamicBody 100 $
        momentForBox 100 100 100
    ]
    $ \[body1, body2] ->
      withSetup_
        ( do
            space.gravity $= V2 0 (-100)

            body1.position $= V2 250 300
            addBoxShape space body1 100 100 0

            body2.position $= V2 300 500
            addBoxShape space body2 100 100 0

            static <- get space.staticBody
            static.position $= V2 300 50
            addBoxShape space static 600 100 0
        )
        $ actionIO (C.spaceStep space (1 / 60))
          >>> arrIO (\_ -> (,) <$> get body1.position <*> get body2.position)
          >>> render
          >>> renderGame gameEnv
          >>> constant Nothing

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
