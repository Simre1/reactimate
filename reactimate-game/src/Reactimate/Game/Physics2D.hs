{-# LANGUAGE DataKinds #-}

module Reactimate.Game.Physics2D
  ( withPhysics,

    -- * Space
    Space,
    withSubspace,
    Subspace,
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
    Body,
    addDynamicBody,
    addKinematicBody,
    addStaticBody,
    removeBody,
    C.bodyType,
    C.BodyType (..),
    C.bodyMass,
    bodyPosition,
    bodyCenterOfGravity,
    bodyVelocity,
    bodyForce,
    bodyAngle,
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
    removeShape,
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
import Control.Monad (forM_, when)
import Data.Hashable (Hashable (..))
import Data.IORef
import Data.Set qualified as S
import Data.StateVar
import Data.Word (Word32)
import Foreign.StablePtr
import GHC.Records (HasField (..))
import Linear.V2
import Reactimate

-- SPACE

-- | Allocates a `Space`, which is used for physics simulation.
--
-- Do not use `Space` outside the signal. The `Space` will be freed when this signal function does not run anymore.
withPhysics :: (Space -> Signal a b) -> Signal a b
withPhysics =
  allocateResource
    ( \fin -> do
        space <- C.spaceNew
        addFinalizer fin $ do
          shapesRef <- newIORef []
          bodiesRef <- newIORef []

          C.spaceEachShape space (\shape _ -> modifyIORef' shapesRef (shape :)) C.nullPtr

          C.spaceEachBody space (\body _ -> modifyIORef' bodiesRef (body :)) C.nullPtr

          C.spaceFree space

          shapes <- readIORef shapesRef
          forM_ shapes C.shapeFree

          bodies <- readIORef bodiesRef
          forM_ bodies C.bodyFree
        pure space
    )

-- | Global gravity applied to the space. Defaults to (V2 0 0).
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

-- | A `Subspace` is a part of a `Space` and can be used to manage groups of bodies.
data Subspace = Subspace
  { space :: Space,
    bodies :: StablePtr (IORef (S.Set Body))
  }

class HasSpace space where
  getSpace :: space -> Space
  addBody :: space -> Body -> IO ()

removeAndFreeBodyFromSpace :: Space -> Body -> IO ()
removeAndFreeBodyFromSpace space body = do
  shapesRef <- newIORef []

  C.bodyEachShape
    body
    ( \_ shape _ -> modifyIORef' shapesRef (shape :)
    )
    C.nullPtr

  shapes <- readIORef shapesRef
  forM_ shapes $ \shape -> do
    C.spaceRemoveShape space shape
    C.shapeFree shape

  C.spaceRemoveBody space body
  C.bodyFree body

-- | Removes a `Body` and it's shapes from the `Space`. You may not use the `Body` afterwards.
removeBody :: Body -> IO ()
removeBody body = do
  space <- get (C.bodySpace body)
  bodiesPtr <- get (C.bodyUserData body)
  when (bodiesPtr /= C.nullPtr) $ do
    bodiesRef <- deRefStablePtr (castPtrToStablePtr bodiesPtr)
    modifyIORef' bodiesRef (S.delete body)
  removeAndFreeBodyFromSpace space body

instance HasSpace Space where
  getSpace = id
  addBody space body = do
    C.spaceAddBody space body
    C.bodyUserData body $= C.nullPtr

instance HasSpace Subspace where
  getSpace = (.space)
  addBody (Subspace space bodiesPtr) body = do
    C.spaceAddBody space body
    C.bodyUserData body $= castStablePtrToPtr bodiesPtr
    bodies <- deRefStablePtr bodiesPtr
    modifyIORef' bodies $ S.insert body

-- | Creates a `Subspace` which keeps tracks of its contained bodies. All bodies of
-- the `Subspace` get deleted after the `withSubspace` gets switched out.
withSubspace :: (HasSpace space) => space -> (Subspace -> Signal a b) -> Signal a b
withSubspace space = allocateResource $ \fin -> do
  bodiesRef <- newIORef S.empty
  addFinalizer fin $ do
    bodies <- readIORef bodiesRef
    forM_ bodies $ \body ->
      removeAndFreeBodyFromSpace (getSpace space) body
  bodiesPtr <- newStablePtr bodiesRef
  pure $ Subspace (getSpace space) bodiesPtr

-- BODIES

-- | Add a dynamic body with mass and inertia.
addDynamicBody ::
  (HasSpace space) =>
  space ->
  -- | Mass of the body
  Double ->
  -- | Moment of the body. It's best to use the `moment*` functions for this.
  Double ->
  IO Body
addDynamicBody space mass inertia = do
  body <- C.bodyNew mass inertia
  addBody space body
  pure body

-- | Add a kinematic body which is not affected by forces. Control it by setting it's velocity
addKinematicBody :: (HasSpace space) => space -> IO Body
addKinematicBody space = do
  body <- C.bodyNewKinematic
  addBody space body
  pure body

-- | Add a static body. Static bodies have infinite mass and you should not move them.
addStaticBody :: (HasSpace space) => space -> IO Body
addStaticBody space = do
  body <- C.bodyNewStatic
  addBody space body
  pure body

-- | World position of the body
bodyPosition :: Body -> StateVar (V2 Double)
bodyPosition body =
  let bodyPos = C.bodyPosition body
   in makeStateVar
        (vectToV2 <$> get bodyPos)
        ( \pos -> do
            bodyPos $= v2ToVect pos
            space <- get (C.bodySpace body)
            C.spaceReindexShapesForBody space body
        )

-- | Rotation of the angle
bodyAngle :: Body -> StateVar Double
bodyAngle body =
  let bodyAngle' = C.bodyAngle body
   in makeStateVar
        (get bodyAngle')
        ( \angle -> do
            bodyAngle' $= angle
            space <- get (C.bodySpace body)
            C.spaceReindexShapesForBody space body
        )

-- | Location of the center of gravity in body local coordinates.
-- The default value is (0, 0), meaning the center of gravity is the same as the position of the body.
bodyCenterOfGravity :: Body -> StateVar (V2 Double)
bodyCenterOfGravity body = mapStateVar v2ToVect vectToV2 (C.bodyCenterOfGravity body)

-- | Linear velocity of the center of gravity of the body
bodyVelocity :: Body -> StateVar (V2 Double)
bodyVelocity body = mapStateVar v2ToVect vectToV2 (C.bodyVelocity body)

-- | Force applied to the center of gravity of the body. This value is reset for every time step.
bodyForce :: Body -> StateVar (V2 Double)
bodyForce body = mapStateVar v2ToVect vectToV2 (C.bodyForce body)

-- | Calculate the moment of inertia for a line segment. The endpoints a and b are relative to the body.
momentForSegment ::
  -- | Mass
  Double ->
  -- | Start
  V2 Double ->
  -- | End
  V2 Double ->
  -- | Thickness
  Double ->
  Double
momentForSegment mass start end = C.momentForSegment mass (v2ToVect start) (v2ToVect end)

-- Calculate the moment of inertia for a solid polygon shape assuming its center of gravity is at its centroid. The offset is added to each vertex.
momentForPoly ::
  -- | Mass
  Double ->
  -- | Vertices
  [V2 Double] ->
  -- | Offset
  V2 Double ->
  -- | Thickness
  Double ->
  Double
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

addCircleShape ::
  Body ->
  -- | Radius
  Double ->
  -- | Offset from the body coordinates
  V2 Double ->
  IO Shape
addCircleShape body radius centerOfGravity = do
  shape <- C.circleShapeNew body radius (v2ToVect centerOfGravity)
  space <- get $ C.bodySpace body
  C.spaceAddShape space shape
  pure shape

-- | Add a box shape centered around the body center
addBoxShape ::
  Body ->
  -- | Box size
  V2 Double ->
  -- | Radius for smooth edges
  Double ->
  IO Shape
addBoxShape body (V2 height width) boxRadius = do
  shape <- C.boxShapeNew body height width boxRadius
  space <- get $ C.bodySpace body
  C.spaceAddShape space shape
  pure shape

addSegmentShape ::
  Body ->
  -- | Start point
  V2 Double ->
  -- | Etart point
  V2 Double ->
  -- | Thickness
  Double ->
  IO Shape
addSegmentShape body start end thickness = do
  shape <- C.segmentShapeNew body (v2ToVect start) (v2ToVect end) thickness
  space <- get $ C.bodySpace body
  C.spaceAddShape space shape
  pure shape

addPolyShape ::
  Body ->
  -- | Vertices
  [V2 Double] ->
  IO Shape
addPolyShape body vects = do
  shape <- C.polyShapeNew body (fmap v2ToVect vects) (C.Transform 0 0 0 0 0 0) 0
  space <- get $ C.bodySpace body
  C.spaceAddShape space shape
  pure shape

removeShape :: Shape -> IO ()
removeShape shape = do
  space <- get (C.shapeSpace shape)
  C.spaceRemoveShape space shape
  C.shapeFree shape

-- | When you have a number of segment shapes that are all joined together, things can still collide with the “cracks” between the segments.
-- By setting the neighbor segment endpoints you can tell Chipmunk to avoid colliding with the inner parts of the crack.
segmentShapeNeighbors :: Shape -> SettableStateVar (V2 Double, V2 Double)
segmentShapeNeighbors shape = makeSettableStateVar $ \(v1, v2) -> C.segmentShapeNeighbors shape C.$= (v2ToVect v1, v2ToVect v2)

-- | The surface velocity of the object. Useful for creating conveyor belts or players that move around.
-- This value is only used when calculating friction, not resolving the collision.
shapeSurfaceVelocity :: Shape -> StateVar (V2 Double)
shapeSurfaceVelocity shape = mapStateVar v2ToVect vectToV2 (C.shapeSurfaceVelocity shape)

instance HasField "sensor" Shape (StateVar Bool) where
  getField = C.shapeSensor

instance HasField "elasticity" Shape (StateVar Double) where
  getField = C.shapeElasticity

instance HasField "friction" Shape (StateVar Double) where
  getField = C.shapeFriction

instance HasField "surfaceVelocity" Shape (StateVar (V2 Double)) where
  getField = shapeSurfaceVelocity

instance HasField "collisionType" Shape (StateVar C.CollisionType) where
  getField = C.shapeCollisionType

instance HasField "mass" Shape (StateVar Double) where
  getField = C.shapeMass

instance HasField "density" Shape (StateVar Double) where
  getField = C.shapeDensity

instance HasField "filter" Shape (StateVar C.ShapeFilter) where
  getField = C.shapeFilter

v2ToVect :: V2 Double -> Vect
v2ToVect (V2 x y) = Vect x y

vectToV2 :: Vect -> V2 Double
vectToV2 (Vect x y) = V2 x y
