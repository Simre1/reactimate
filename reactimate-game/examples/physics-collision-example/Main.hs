{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Data.Bool (bool)
import Data.Colour.Names (black, blue, red)
import Data.Foldable (traverse_)
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
          shape1 <- addBoxShape body1 (V2 100 100) 0
          shape1.friction $= 0.3

          body2 <- addDynamicBody space 1 (1 / 0)
          body2.position $= V2 350 500
          shape2 <- addBoxShape body2 (V2 100 100) 0
          shape2.friction $= 0.3

          static <- get space.staticBody
          static.position $= V2 300 50
          ground <- addBoxShape static (V2 600 100) 0
          ground.friction $= 0.5

          collisionEvent <-
            modifyDefaultCollisionHandler space $
              idCollisionHandler
                { begin = Just $ \collision space -> do
                    position <- collision.pointA 1

                    (bodyA, bodyB) <- collision.bodies
                    _ <- schedulePostStepWork space collision $ \space -> do
                      when (bodyB == static) $
                        bodyApplyImpulseAtLocalPoint bodyA (V2 0 200) (V2 0 0)
                      when (bodyA == static) $
                        bodyApplyImpulseAtLocalPoint bodyB (V2 0 200) (V2 0 0)

                    bodies <- collision.bodies
                    pure (True, Just bodies)
                }

          pure (body1, body2, collisionEvent)
      )
      $ \(body1, body2, collisionEvent) ->
        actionIO (spaceStep space (1 / 60))
          >>> arrIO (\_ -> (,) <$> get body1.position <*> get body2.position)
          >>> render
          >>> renderGame gameEnv
          >>> sampleEventAsList collisionEvent
          >>> arrIO
            ( traverse_
                ( \(bodyA, bodyB) -> do
                    posA <- get bodyA.position
                    posB <- get bodyB.position
                    putStrLn $ "Collision with bodies at: " ++ show posA ++ ", " ++ show posB
                )
            )
          >>> bool Nothing (Just ())
          <$> sampleBehavior (gameShouldQuit gameEnv)

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
