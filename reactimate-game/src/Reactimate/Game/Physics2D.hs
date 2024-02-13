{-# LANGUAGE DataKinds #-}

module Reactimate.Game.Physics2D
  ( withPhysics,

    -- * Space
    Space,
    C.spaceStep,
    spaceGravity,
    C.spaceDamping,
    C.spaceIdleSpeedThreshold,
    C.spaceSleepTimeThreshold,
    C.spaceCollisionSlop,
    C.spaceCollisionBias,
    C.spaceCollisionPersistence,
    C.spaceCurrentTimeStep,
    C.spaceStaticBody,

    -- * Body
    withBodies,
    CreateBody (..),
    C.bodyType,
    C.BodyType (..),
    C.bodyMass,
    bodyPosition,
    bodyCenterOfGravity,
    bodyVelocity,
    bodyForce,
    C.bodyAngle,
    C.bodyAngularVelocity,
    C.bodyTorque,

    -- ** Moment calculation
    C.momentForCircle,
    momentForSegment,
    momentForPoly,
    C.momentForBox,

    -- * Shapes
    addCircleShape,
    addBoxShape,
    addPolyShape,
    addSegmentShape,
    segmentShapeNeighbors,
    C.shapeSensor,
    C.shapeElasticity,
    C.shapeFriction,
    shapeSurfaceVelocity,
    C.shapeCollisionType,
    C.CollisionType,
    C.shapeMass,
    C.shapeDensity,
    C.shapeFilter,
    C.ShapeFilter (..),

    -- * StateVar
    StateVar,
    GettableStateVar,
    SettableStateVar,
    HasGetter (..),
    HasSetter (..),
  )
where

import Chiphunk.Low (Body, Shape, Space, Vect (..))
import Chiphunk.Low qualified as C
import Control.Monad (forM_)
import Data.StateVar
import Data.Traversable (for)
import GHC.Records (HasField (..))
import Linear.V2
import Reactimate
import Data.Word (Word32)

-- SPACE

-- | Allocates a `Space`.
--
-- Do not use `Space` outside the signal. The `Space` will be freed when this signal function does not run anymore.
withPhysics :: (Space -> Signal a b) -> Signal a b
withPhysics =
  allocateResource
    ( \fin -> do
        space <- C.spaceNew
        addFinalizer fin $ C.spaceFree space
        pure space
    )

spaceGravity :: Space -> StateVar (V2 Double)
spaceGravity space = mapStateVar v2ToVect vectToV2 (C.spaceGravity space)

instance HasField "gravity" Space (StateVar (V2 Double)) where
  getField = spaceGravity

instance HasField "damping" Space (StateVar Double) where
  getField = C.spaceDamping

instance HasField "idleSpeedThreshold" Space (StateVar Double) where
  getField = C.spaceIdleSpeedThreshold

instance HasField "sleepTimeThreshold" Space (StateVar Double) where
  getField = C.spaceSleepTimeThreshold

instance HasField "collisionSlop" Space (StateVar Double) where
  getField = C.spaceCollisionSlop

instance HasField "collisionBias" Space (StateVar Double) where
  getField = C.spaceCollisionBias

instance HasField "collisionPersistence" Space (StateVar Word32) where
  getField = C.spaceCollisionPersistence

instance HasField "currentTimeStep" Space (GettableStateVar Double) where
  getField = C.spaceCurrentTimeStep

instance HasField "staticBody" Space (GettableStateVar Body) where
  getField = C.spaceStaticBody

-- BODIES

data CreateBody = CreateKinematicBody | CreateStaticBody | CreateDynamicBody {mass :: !Double, inertia :: !Double}

withBodies :: (Traversable f) => Space -> f CreateBody -> (f Body -> Signal a b) -> Signal a b
withBodies space inits =
  allocateResource
    ( \fin -> do
        bodies <- for inits $ \case
          CreateKinematicBody -> C.bodyNewKinematic
          CreateStaticBody -> C.bodyNewStatic
          CreateDynamicBody mass inertia -> C.bodyNew mass inertia
        forM_ bodies $ C.spaceAddBody space

        addFinalizer fin $ forM_ bodies $ \body -> do
          C.bodyEachShape body (\_ shape _ -> C.spaceRemoveShape space shape >> C.shapeFree shape) C.nullPtr
          C.spaceRemoveBody space body
          C.bodyFree body

        pure bodies
    )

bodyPosition :: Body -> StateVar (V2 Double)
bodyPosition body = mapStateVar v2ToVect vectToV2 (C.bodyPosition body)

bodyCenterOfGravity :: Body -> StateVar (V2 Double)
bodyCenterOfGravity body = mapStateVar v2ToVect vectToV2 (C.bodyCenterOfGravity body)

bodyVelocity :: Body -> StateVar (V2 Double)
bodyVelocity body = mapStateVar v2ToVect vectToV2 (C.bodyVelocity body)

bodyForce :: Body -> StateVar (V2 Double)
bodyForce body = mapStateVar v2ToVect vectToV2 (C.bodyForce body)

momentForSegment :: Double -> V2 Double -> V2 Double -> Double -> Double
momentForSegment mass start end = C.momentForSegment mass (v2ToVect start) (v2ToVect end)

momentForPoly :: Double -> [V2 Double] -> V2 Double -> Double -> Double
momentForPoly mass vertices offset = C.momentForPoly mass (fmap v2ToVect vertices) (v2ToVect offset)

instance HasField "position" Body (StateVar (V2 Double)) where
  getField = bodyPosition

instance HasField "centerOfGravity" Body (StateVar (V2 Double)) where
  getField = bodyCenterOfGravity

instance HasField "velocity" Body (StateVar (V2 Double)) where
  getField = bodyVelocity

instance HasField "force" Body (StateVar (V2 Double)) where
  getField = bodyForce

instance HasField "angle" Body (StateVar Double) where
  getField = C.bodyAngle

instance HasField "angularVelocity" Body (StateVar Double) where
  getField = C.bodyAngularVelocity

instance HasField "torque" Body (StateVar Double) where
  getField = C.bodyTorque

-- SHAPES

addCircleShape :: Space -> Body -> Double -> V2 Double -> IO Shape
addCircleShape space body radius centerOfGravity = do
  shape <- C.circleShapeNew body radius (v2ToVect centerOfGravity)
  C.spaceAddShape space shape
  pure shape

addBoxShape :: Space -> Body -> Double -> Double -> Double -> IO Shape
addBoxShape space body height width boxRadius = do
  shape <- C.boxShapeNew body height width boxRadius
  C.spaceAddShape space shape
  pure shape

addSegmentShape :: Space -> Body -> V2 Double -> V2 Double -> Double -> IO Shape
addSegmentShape space body start end thickness = do
  shape <- C.segmentShapeNew body (v2ToVect start) (v2ToVect end) thickness
  C.spaceAddShape space shape
  pure shape

addPolyShape :: Space -> Body -> [V2 Double] -> IO Shape
addPolyShape space body vects = do
  shape <- C.polyShapeNew body (fmap v2ToVect vects) (C.Transform 0 0 0 0 0 0) 0
  C.spaceAddShape space shape
  pure shape

segmentShapeNeighbors :: Shape -> SettableStateVar (V2 Double, V2 Double)
segmentShapeNeighbors shape = makeSettableStateVar $ \(v1, v2) -> C.segmentShapeNeighbors shape C.$= (v2ToVect v1, v2ToVect v2)

shapeSurfaceVelocity :: Shape -> StateVar (V2 Double)
shapeSurfaceVelocity shape = mapStateVar v2ToVect vectToV2 (C.shapeSurfaceVelocity shape)

instance HasField "shapeSensor" Shape (StateVar Bool) where
  getField = C.shapeSensor

instance HasField "shapeElasticity" Shape (StateVar Double) where
  getField = C.shapeElasticity

instance HasField "shapeFriction" Shape (StateVar Double) where
  getField = C.shapeFriction

instance HasField "shapeSurfaceVelocity" Shape (StateVar (V2 Double)) where
  getField = shapeSurfaceVelocity

instance HasField "shapeCollisionType" Shape (StateVar C.CollisionType) where
  getField = C.shapeCollisionType

instance HasField "shapeMass" Shape (StateVar Double) where
  getField = C.shapeMass

instance HasField "shapeDensity" Shape (StateVar Double) where
  getField = C.shapeDensity

instance HasField "shapeFilter" Shape (StateVar C.ShapeFilter) where
  getField = C.shapeFilter

v2ToVect :: V2 Double -> Vect
v2ToVect (V2 x y) = Vect x y

vectToV2 :: Vect -> V2 Double
vectToV2 (Vect x y) = V2 x y
