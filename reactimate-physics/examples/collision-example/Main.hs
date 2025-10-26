{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Data.Bool (bool)
import Data.Colour.Names (black, blue, red)
import Data.Foldable (traverse_)
import Data.Vector.Storable qualified as VS
import Data.Word (Word64)
import Foreign (WordPtr)
import Reactimate
import Reactimate.Game
import Reactimate.Physics2D

main :: IO ()
main =
  runSetup $
    reactimate $
      runGame "Physics example" defaultWindow 60 $
        mapEffects (\s -> getHandle >>= \ioe -> runPhysics (collisionHandler ioe) s) $
          arrStep (\(Handle physics) _ -> stepPhysics physics (1 / 60))
            >>> setupBodies
            >>> render
            >>> renderGame
            -- >>> sampleEventAsList collisionEvent
            -- >>> arrIO
            --   ( traverse_
            --       ( \(bodyA, bodyB) -> do
            --           posA <- get bodyA.position
            --           posB <- get bodyB.position
            --           putStrLn $ "Collision with bodies at: " ++ show posA ++ ", " ++ show posB
            --       )
            --   )
            >>> bool Nothing (Just ())
            <$> gameShouldQuit

groundCT, boxCT :: WordPtr
groundCT = 1
boxCT = 2

setupBodies :: (Physics :> es) => Signal es () (V2 Double, V2 Double)
setupBodies = makeSignal $ do
  physics <- getPhysics
  space <- getSpace

  prestep $ writeVar physics.gravity (V2 0 (-200))

  body1 <- prestep $ addDynamicBody space 1 (1 / 0) -- 1 / 0 -> locks rotation due to infinite inertia
  prestep $ writeVar body1.position (V2 250 300)
  shape1 <- prestep $ addBoxShape body1 (V2 100 100) 0
  prestep $ writeVar shape1.friction 0.3

  body2 <- prestep $ addDynamicBody space 1 (1 / 0)
  prestep $ writeVar body2.position (V2 350 500)
  shape2 <- prestep $ addBoxShape body2 (V2 100 100) 0
  prestep $ writeVar shape2.friction (0.3)

  prestep $ writeVar space.staticBody.position (V2 300 50)

  ground <- prestep $ addBoxShape space.staticBody (V2 600 100) 0
  prestep $ writeVar ground.collisionType groundCT
  prestep $ writeVar ground.friction 0.5

  pure $ \_ -> do
    p1 <- readVar body1.position
    p2 <- readVar body2.position
    pure (p1, p2)

collisionHandler :: IOE s -> GlobalCollisionHandler s
collisionHandler (IOE lift) = handleCollisionGlobally DefaultScope $ beginCollision $ do
  (bodyA, bodyB) <- getCollisionBodies
  (shapeA, shapeB) <- getCollisionShapes
  addCallback $ do
    ctA <- readVar shapeA.collisionType
    ctB <- readVar shapeB.collisionType

    when (ctB == groundCT) $
      bodyApplyImpulseAtLocalPoint bodyA (V2 0 200) (V2 0 0)

    when (ctA == groundCT) $
      bodyApplyImpulseAtLocalPoint bodyB (V2 0 200) (V2 0 0)

    lift $ putStrLn "Collision happened"

  pure processCollision

-- collisionEvent <-
--   modifyDefaultCollisionHandler space $
--     idCollisionHandler
--       { begin = Just $ \collision space -> do
--           position <- collision.pointA 1

--           (bodyA, bodyB) <- collision.bodies
--           _ <- schedulePostStepWork space collision $ \space -> do
--             when (bodyB == static) $
--               bodyApplyImpulseAtLocalPoint bodyA (V2 0 200) (V2 0 0)
--             when (bodyA == static) $
--               bodyApplyImpulseAtLocalPoint bodyB (V2 0 200) (V2 0 0)

--           bodies <- collision.bodies
--           pure (True, Just bodies)
--       }

-- pure (body1, body2, collisionEvent)
-- )

-- $ \(body1, body2, collisionEvent) ->
-- actionIO (spaceStep space (1 / 60))
--   >>> arrIO (\_ -> (,) <$> get body1.position <*> get body2.position)

render :: Signal es (V2 Double, V2 Double) (Camera, Picture)
render =
  arr $ \(pos1, pos2) ->
    ( Camera (V2 0 (-100)) (V2 800 600),
      makePicture 0 $ do
        drawRectangle (packColour red) $ Rectangle (round <$> pos1 - V2 50 50) (V2 100 100)
        drawRectangle (packColour blue) $ Rectangle (round <$> pos2 - V2 50 50) (V2 100 100)
        drawRectangle (packColour black) $ Rectangle (V2 0 0) (V2 600 100)
    )
