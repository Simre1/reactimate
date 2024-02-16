{-# LANGUAGE DataKinds #-}

module Reactimate.Game.Physics2D
  ( withPhysics,

    -- * Space
    Space,
    spaceStep,
    withSubspace,
    Subspace,
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

    -- ** Coordinate Conversion
    bodyLocalToWorld,
    bodyWorldToLocal,
    bodyVelocityAtWorldPoint,

    -- ** Apply Force
    bodyApplyForceAtWorldPoint,
    bodyApplyForceAtLocalPoint,
    bodyApplyImpulseAtWorldPoint,
    bodyApplyImpulseAtLocalPoint,

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

    -- * Constraints
    C.Constraint,
    IsConstraint (..),
    constraintBodyA,
    constraintBodyB,
    constraintMaxForce,
    constraintErrorBias,
    constraintMaxBias,
    constraintCollideBodies,
    constraintImpulse,

    -- ** Pin Joint
    PinJoint,
    addPinJoint,
    pinJointAnchorA,
    pinJointAnchorB,
    pinJointDistance,

    -- ** Slide Joint
    SlideJoint,
    addSlideJoint,
    slideJointAnchorA,
    slideJointAnchorB,
    slideJointMin,
    slideJointMax,

    -- ** Pivot Joint
    PivotJoint,
    addPivotJoint,
    pivotJointAnchorA,
    pivotJointAnchorB,
    addGrooveJoint,

    -- ** Groove Joint
    GrooveJoint,
    grooveJointGrooveA,
    grooveJointGrooveB,
    grooveJointAnchorB,

    -- ** Damped Spring
    DampedSpring,
    addDampedSpring,
    dampedSpringAnchorA,
    dampedSpringAnchorB,
    dampedSpringDistance,
    dampedSpringStiffness,
    dampedSpringDamping,

    -- ** Damped Rotary Spring,
    DampedRotarySpring,
    addDampedRotarySpring,
    dampedRotarySpringAngle,
    dampedRotarySpringStiffness,
    dampedRotarySpringDamping,

    -- ** Rotary Limit Joint
    RotaryLimitJoint,
    addRotaryLimitJoint,
    rotaryLimitJointMin,
    rotaryLimitJointMax,

    -- ** Ratchet Joint
    RatchetJoint,
    addRatchetJoint,
    ratchetJointAngle,
    ratchetJointPhase,
    ratchetJointRatchet,

    -- ** Gear Joint
    GearJoint,
    addGearJoint,
    gearJointPhase,
    gearJointRatio,

    -- ** Simple Motor
    SimpleMotor,
    addSimpleMotor,
    simpleMotorRate,

    -- * Collisions
    Collision,
    ModifiableCollision,

    -- ** Collision properties
    collisionRestitution,
    modifiableCollisionRestitution,
    collisionFriction,
    modifiableCollisionFriction,
    collisionSurfaceVelocity,
    modifiableCollisionSurfaceVelocity,
    collisionCount,
    modifiableCollisionCount,
    collisionNormal,
    modifiableCollisionNormal,
    collisionPointA,
    modifiableCollisionPointA,
    collisionPointB,
    modifiableCollisionPointB,
    collisionDepth,
    modifiableCollisionDepth,
    collisionIsFirstContact,
    modifiableCollisionIsFirstContact,
    collisionIsRemoval,
    modifiableCollisionIsRemoval,
    collisionShapes,
    modifiableCollisionShapes,
    collisionBodies,
    modifiableCollisionBodies,

    -- ** Modify Collision Handlers
    ModifyCollisionHandler (..),
    CollisionSpace,
    idCollisionHandler,
    modifyCollisionHandler,
    modifyWildcardCollisionHandler,
    modifyDefaultCollisionHandler,

    -- *** Safely change objects
    schedulePostStepWork,

    -- *** Invoke more general handlers
    handleWildcardBeginA,
    handleWildcardBeginB,
    handleWildcardPreSolveA,
    handleWildcardPreSolveB,
    handleWildcardPostSolveA,
    handleWildcardPostSolveB,
    handleWildcardSeparateA,
    handleWildcardSeparateB,

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
import Data.Foldable (traverse_)
import Data.Hashable (Hashable (hash))
import Data.IORef
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as M
import Data.Maybe (isJust)
import Data.Set qualified as S
import Data.StateVar
import Data.Unique (newUnique)
import Data.Word (Word32)
import Foreign (Storable (..))
import Foreign.Marshal (malloc)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (castPtr)
import Foreign.StablePtr
import GHC.Records (HasField (..))
import Linear.V2
import Reactimate
import Reactimate.Signal (unSignal, withFinalizer)
import Unsafe.Coerce (unsafeCoerce)

-- SPACE

-- | Allocates a `Space`, which is used for physics simulation.
--
-- Do not use `Space` outside the signal. The `Space` will be freed when this signal function does not run anymore.
withPhysics :: (Space -> Signal a b) -> Signal a b
withPhysics =
  allocateResource
    ( \fin -> do
        space <- C.spaceNew

        postStepCallbacks <- newIORef emptyPostStepCallbacks
        stablePostStepCallbacks <- newStablePtr postStepCallbacks

        C.spaceUserData space $= castStablePtrToPtr stablePostStepCallbacks

        addFinalizer fin $ do
          shapesRef <- newIORef []
          bodiesRef <- newIORef []
          constraintsRef <- newIORef []

          C.spaceEachShape space (\shape _ -> modifyIORef' shapesRef (shape :)) C.nullPtr

          C.spaceEachBody space (\body _ -> modifyIORef' bodiesRef (body :)) C.nullPtr

          C.spaceEachConstraint space (\body _ -> modifyIORef' constraintsRef (body :)) C.nullPtr

          C.spaceFree space

          shapes <- readIORef shapesRef
          forM_ shapes C.shapeFree

          constraints <- readIORef constraintsRef
          forM_ constraints C.constraintFree

          bodies <- readIORef bodiesRef
          forM_ bodies C.bodyFree
        pure space
    )

spaceStep :: Space -> Double -> IO ()
spaceStep space time = do
  C.spaceStep space time
  ptr <- get (C.spaceUserData space)
  postStepCallbacks <- deRefStablePtr (castPtrToStablePtr ptr)
  PostStepCallbacks unkeyed keyed <- readIORef postStepCallbacks
  writeIORef postStepCallbacks emptyPostStepCallbacks
  unkeyed
  forM_ keyed id

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

class IsSpace space where
  getSpace :: space -> Space
  addBody :: space -> Body -> IO ()

removeAndFreeBodyFromSpace :: Space -> Body -> IO ()
removeAndFreeBodyFromSpace space body = do
  shapesRef <- newIORef []
  constraintsRef <- newIORef []

  C.bodyEachShape
    body
    ( \_ shape _ -> modifyIORef' shapesRef (shape :)
    )
    C.nullPtr

  C.bodyEachConstraint
    body
    ( \_ constraint _ -> modifyIORef' constraintsRef (constraint :)
    )
    C.nullPtr

  constraints <- readIORef constraintsRef
  forM_ constraints $ \constraint -> do
    C.spaceRemoveConstraint space constraint
    C.constraintFree constraint

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

instance IsSpace Space where
  getSpace = id
  addBody space body = do
    C.spaceAddBody space body
    C.bodyUserData body $= C.nullPtr

instance IsSpace Subspace where
  getSpace = (.space)
  addBody (Subspace space bodiesPtr) body = do
    C.spaceAddBody space body
    C.bodyUserData body $= castStablePtrToPtr bodiesPtr
    bodies <- deRefStablePtr bodiesPtr
    modifyIORef' bodies $ S.insert body

-- | Creates a `Subspace` which keeps tracks of its contained bodies. All bodies of
-- the `Subspace` get deleted after the `withSubspace` gets switched out.
withSubspace :: (IsSpace space) => space -> (Subspace -> Signal a b) -> Signal a b
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
  (IsSpace space) =>
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
addKinematicBody :: (IsSpace space) => space -> IO Body
addKinematicBody space = do
  body <- C.bodyNewKinematic
  addBody space body
  pure body

-- | Add a static body. Static bodies have infinite mass and you should not move them.
addStaticBody :: (IsSpace space) => space -> IO Body
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

bodyLocalToWorld :: Body -> V2 Double -> IO (V2 Double)
bodyLocalToWorld body local = vectToV2 <$> C.bodyLocalToWorld body (v2ToVect local)

bodyWorldToLocal :: Body -> V2 Double -> IO (V2 Double)
bodyWorldToLocal body world = vectToV2 <$> C.bodyWorldToLocal body (v2ToVect world)

bodyVelocityAtWorldPoint :: Body -> V2 Double -> IO (V2 Double)
bodyVelocityAtWorldPoint body world = vectToV2 <$> C.bodyVelocityAtWorldPoint body (v2ToVect world)

bodyApplyForceAtWorldPoint :: Body -> V2 Double -> V2 Double -> IO ()
bodyApplyForceAtWorldPoint body force local = C.bodyApplyForceAtWorldPoint body (v2ToVect force) (v2ToVect local)

bodyApplyForceAtLocalPoint :: Body -> V2 Double -> V2 Double -> IO ()
bodyApplyForceAtLocalPoint body force world = C.bodyApplyForceAtLocalPoint body (v2ToVect force) (v2ToVect world)

bodyApplyImpulseAtWorldPoint :: Body -> V2 Double -> V2 Double -> IO ()
bodyApplyImpulseAtWorldPoint body impulse local = C.bodyApplyImpulseAtWorldPoint body (v2ToVect impulse) (v2ToVect local)

bodyApplyImpulseAtLocalPoint :: Body -> V2 Double -> V2 Double -> IO ()
bodyApplyImpulseAtLocalPoint body impulse world = C.bodyApplyImpulseAtLocalPoint body (v2ToVect impulse) (v2ToVect world)

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

-- CONSTRAINTS

class IsConstraint c where
  getConstraint :: c -> C.Constraint

instance IsConstraint C.Constraint where
  getConstraint = id

constraintBodyA :: (IsConstraint c) => c -> GettableStateVar Body
constraintBodyA = C.constraintBodyA . getConstraint

constraintBodyB :: (IsConstraint c) => c -> GettableStateVar Body
constraintBodyB = C.constraintBodyB . getConstraint

constraintMaxForce :: (IsConstraint c) => c -> StateVar Double
constraintMaxForce = C.constraintMaxForce . getConstraint

constraintErrorBias :: (IsConstraint c) => c -> StateVar Double
constraintErrorBias = C.constraintErrorBias . getConstraint

constraintMaxBias :: (IsConstraint c) => c -> StateVar Double
constraintMaxBias = C.constraintMaxBias . getConstraint

constraintCollideBodies :: (IsConstraint c) => c -> StateVar Bool
constraintCollideBodies = C.constraintCollideBodies . getConstraint

constraintImpulse :: (IsConstraint c) => c -> GettableStateVar Double
constraintImpulse = C.constraintImpulse . getConstraint

newtype PinJoint = PinJoint C.Constraint

instance IsConstraint PinJoint where
  getConstraint (PinJoint c) = c

instance HasField "bodyA" PinJoint (GettableStateVar Body) where
  getField = constraintBodyA

instance HasField "bodyB" PinJoint (GettableStateVar Body) where
  getField = constraintBodyB

instance HasField "maxForce" PinJoint (StateVar Double) where
  getField = constraintMaxForce

instance HasField "errorBias" PinJoint (StateVar Double) where
  getField = constraintErrorBias

instance HasField "maxBias" PinJoint (StateVar Double) where
  getField = constraintMaxBias

instance HasField "collideBodies" PinJoint (StateVar Bool) where
  getField = constraintCollideBodies

instance HasField "impulse" PinJoint (GettableStateVar Double) where
  getField = constraintImpulse

addPinJoint :: Body -> V2 Double -> Body -> V2 Double -> IO PinJoint
addPinJoint body1 anchor1 body2 anchor2 = do
  pinJoint <- C.pinJointNew body1 body2 (v2ToVect anchor1) (v2ToVect anchor2)
  space <- get (C.bodySpace body1)
  C.spaceAddConstraint space pinJoint
  pure $ PinJoint pinJoint

pinJointAnchorA :: PinJoint -> StateVar (V2 Double)
pinJointAnchorA (PinJoint c) = mapStateVar v2ToVect vectToV2 (C.pinJointAnchorA c)

pinJointAnchorB :: PinJoint -> StateVar (V2 Double)
pinJointAnchorB (PinJoint c) = mapStateVar v2ToVect vectToV2 (C.pinJointAnchorB c)

pinJointDistance :: PinJoint -> StateVar Double
pinJointDistance (PinJoint c) = C.pinJointDist c

instance HasField "anchorA" PinJoint (StateVar (V2 Double)) where
  getField = pinJointAnchorA

instance HasField "anchorB" PinJoint (StateVar (V2 Double)) where
  getField = pinJointAnchorB

instance HasField "distance" PinJoint (StateVar Double) where
  getField = pinJointDistance

newtype SlideJoint = SlideJoint C.Constraint

instance IsConstraint SlideJoint where
  getConstraint (SlideJoint c) = c

instance HasField "bodyA" SlideJoint (GettableStateVar Body) where
  getField = constraintBodyA

instance HasField "bodyB" SlideJoint (GettableStateVar Body) where
  getField = constraintBodyB

instance HasField "maxForce" SlideJoint (StateVar Double) where
  getField = constraintMaxForce

instance HasField "errorBias" SlideJoint (StateVar Double) where
  getField = constraintErrorBias

instance HasField "maxBias" SlideJoint (StateVar Double) where
  getField = constraintMaxBias

instance HasField "collideBodies" SlideJoint (StateVar Bool) where
  getField = constraintCollideBodies

instance HasField "impulse" SlideJoint (GettableStateVar Double) where
  getField = constraintImpulse

addSlideJoint :: Body -> V2 Double -> Body -> V2 Double -> Double -> Double -> IO SlideJoint
addSlideJoint body1 anchor1 body2 anchor2 minDistance maxDistance = do
  slideJoint <- C.slideJointNew body1 body2 (v2ToVect anchor1) (v2ToVect anchor2) minDistance maxDistance
  space <- get (C.bodySpace body1)
  C.spaceAddConstraint space slideJoint
  pure $ SlideJoint slideJoint

slideJointAnchorA :: SlideJoint -> StateVar (V2 Double)
slideJointAnchorA (SlideJoint c) = mapStateVar v2ToVect vectToV2 (C.slideJointAnchorA c)

slideJointAnchorB :: SlideJoint -> StateVar (V2 Double)
slideJointAnchorB (SlideJoint c) = mapStateVar v2ToVect vectToV2 (C.slideJointAnchorB c)

slideJointMin :: SlideJoint -> StateVar Double
slideJointMin (SlideJoint c) = C.slideJointMin c

slideJointMax :: SlideJoint -> StateVar Double
slideJointMax (SlideJoint c) = C.slideJointMax c

instance HasField "anchorA" SlideJoint (StateVar (V2 Double)) where
  getField = slideJointAnchorA

instance HasField "anchorB" SlideJoint (StateVar (V2 Double)) where
  getField = slideJointAnchorB

instance HasField "min" SlideJoint (StateVar Double) where
  getField = slideJointMin

instance HasField "max" SlideJoint (StateVar Double) where
  getField = slideJointMax

newtype PivotJoint = PivotJoint C.Constraint

instance IsConstraint PivotJoint where
  getConstraint (PivotJoint c) = c

instance HasField "bodyA" PivotJoint (GettableStateVar Body) where
  getField = constraintBodyA

instance HasField "bodyB" PivotJoint (GettableStateVar Body) where
  getField = constraintBodyB

instance HasField "maxForce" PivotJoint (StateVar Double) where
  getField = constraintMaxForce

instance HasField "errorBias" PivotJoint (StateVar Double) where
  getField = constraintErrorBias

instance HasField "maxBias" PivotJoint (StateVar Double) where
  getField = constraintMaxBias

instance HasField "collideBodies" PivotJoint (StateVar Bool) where
  getField = constraintCollideBodies

instance HasField "impulse" PivotJoint (GettableStateVar Double) where
  getField = constraintImpulse

addPivotJoint :: Body -> V2 Double -> Body -> V2 Double -> IO PivotJoint
addPivotJoint body1 anchor1 body2 anchor2 = do
  pivotJoint <- C.pivotJointNew2 body1 body2 (v2ToVect anchor1) (v2ToVect anchor2)
  space <- get (C.bodySpace body1)
  C.spaceAddConstraint space pivotJoint
  pure $ PivotJoint pivotJoint

pivotJointAnchorA :: PivotJoint -> StateVar (V2 Double)
pivotJointAnchorA (PivotJoint c) = mapStateVar v2ToVect vectToV2 (C.pivotJointAnchorA c)

pivotJointAnchorB :: PivotJoint -> StateVar (V2 Double)
pivotJointAnchorB (PivotJoint c) = mapStateVar v2ToVect vectToV2 (C.pivotJointAnchorB c)

instance HasField "anchorA" PivotJoint (StateVar (V2 Double)) where
  getField = pivotJointAnchorA

instance HasField "anchorB" PivotJoint (StateVar (V2 Double)) where
  getField = pivotJointAnchorB

newtype GrooveJoint = GrooveJoint C.Constraint

instance IsConstraint GrooveJoint where
  getConstraint (GrooveJoint c) = c

instance HasField "bodyA" GrooveJoint (GettableStateVar Body) where
  getField = constraintBodyA

instance HasField "bodyB" GrooveJoint (GettableStateVar Body) where
  getField = constraintBodyB

instance HasField "maxForce" GrooveJoint (StateVar Double) where
  getField = constraintMaxForce

instance HasField "errorBias" GrooveJoint (StateVar Double) where
  getField = constraintErrorBias

instance HasField "maxBias" GrooveJoint (StateVar Double) where
  getField = constraintMaxBias

instance HasField "collideBodies" GrooveJoint (StateVar Bool) where
  getField = constraintCollideBodies

instance HasField "impulse" GrooveJoint (GettableStateVar Double) where
  getField = constraintImpulse

addGrooveJoint :: Body -> V2 Double -> Body -> V2 Double -> V2 Double -> IO GrooveJoint
addGrooveJoint body1 endpoint1 body2 endpoint2 anchor = do
  grooveJoint <- C.grooveJointNew body1 body2 (v2ToVect endpoint1) (v2ToVect endpoint2) (v2ToVect anchor)
  space <- get (C.bodySpace body1)
  C.spaceAddConstraint space grooveJoint
  pure $ GrooveJoint grooveJoint

grooveJointGrooveA :: GrooveJoint -> StateVar (V2 Double)
grooveJointGrooveA (GrooveJoint c) = mapStateVar v2ToVect vectToV2 (C.grooveJointGrooveA c)

grooveJointGrooveB :: GrooveJoint -> StateVar (V2 Double)
grooveJointGrooveB (GrooveJoint c) = mapStateVar v2ToVect vectToV2 (C.grooveJointGrooveB c)

grooveJointAnchorB :: GrooveJoint -> StateVar (V2 Double)
grooveJointAnchorB (GrooveJoint c) = mapStateVar v2ToVect vectToV2 (C.grooveJointAnchorB c)

instance HasField "grooveA" GrooveJoint (StateVar (V2 Double)) where
  getField = grooveJointGrooveA

instance HasField "grooveB" GrooveJoint (StateVar (V2 Double)) where
  getField = grooveJointGrooveB

instance HasField "anchorB" GrooveJoint (StateVar (V2 Double)) where
  getField = grooveJointAnchorB

newtype DampedSpring = DampedSpring C.Constraint

instance IsConstraint DampedSpring where
  getConstraint (DampedSpring c) = c

instance HasField "bodyA" DampedSpring (GettableStateVar Body) where
  getField = constraintBodyA

instance HasField "bodyB" DampedSpring (GettableStateVar Body) where
  getField = constraintBodyB

instance HasField "maxForce" DampedSpring (StateVar Double) where
  getField = constraintMaxForce

instance HasField "errorBias" DampedSpring (StateVar Double) where
  getField = constraintErrorBias

instance HasField "maxBias" DampedSpring (StateVar Double) where
  getField = constraintMaxBias

instance HasField "collideBodies" DampedSpring (StateVar Bool) where
  getField = constraintCollideBodies

instance HasField "impulse" DampedSpring (GettableStateVar Double) where
  getField = constraintImpulse

addDampedSpring :: Body -> V2 Double -> Body -> V2 Double -> Double -> Double -> Double -> IO DampedSpring
addDampedSpring body1 anchor1 body2 anchor2 distance springConstant damping = do
  dampedSpring <- C.dampedSpringNew body1 body2 (v2ToVect anchor1) (v2ToVect anchor2) distance springConstant damping
  space <- get (C.bodySpace body1)
  C.spaceAddConstraint space dampedSpring
  pure $ DampedSpring dampedSpring

dampedSpringAnchorA :: DampedSpring -> StateVar (V2 Double)
dampedSpringAnchorA (DampedSpring c) = mapStateVar v2ToVect vectToV2 (C.dampedSpringAnchorA c)

dampedSpringAnchorB :: DampedSpring -> StateVar (V2 Double)
dampedSpringAnchorB (DampedSpring c) = mapStateVar v2ToVect vectToV2 (C.dampedSpringAnchorB c)

dampedSpringDistance :: DampedSpring -> StateVar Double
dampedSpringDistance (DampedSpring c) = C.dampedSpringRestLength c

dampedSpringStiffness :: DampedSpring -> StateVar Double
dampedSpringStiffness (DampedSpring c) = C.dampedSpringStiffness c

dampedSpringDamping :: DampedSpring -> StateVar Double
dampedSpringDamping (DampedSpring c) = C.dampedSpringDamping c

instance HasField "anchorA" DampedSpring (StateVar (V2 Double)) where
  getField = dampedSpringAnchorA

instance HasField "anchorB" DampedSpring (StateVar (V2 Double)) where
  getField = dampedSpringAnchorB

instance HasField "distance" DampedSpring (StateVar Double) where
  getField = dampedSpringDistance

instance HasField "stiffness" DampedSpring (StateVar Double) where
  getField = dampedSpringStiffness

instance HasField "damping" DampedSpring (StateVar Double) where
  getField = dampedSpringDamping

newtype DampedRotarySpring = DampedRotarySpring C.Constraint

instance IsConstraint DampedRotarySpring where
  getConstraint (DampedRotarySpring c) = c

instance HasField "bodyA" DampedRotarySpring (GettableStateVar Body) where
  getField = constraintBodyA

instance HasField "bodyB" DampedRotarySpring (GettableStateVar Body) where
  getField = constraintBodyB

instance HasField "maxForce" DampedRotarySpring (StateVar Double) where
  getField = constraintMaxForce

instance HasField "errorBias" DampedRotarySpring (StateVar Double) where
  getField = constraintErrorBias

instance HasField "maxBias" DampedRotarySpring (StateVar Double) where
  getField = constraintMaxBias

instance HasField "collideBodies" DampedRotarySpring (StateVar Bool) where
  getField = constraintCollideBodies

instance HasField "impulse" DampedRotarySpring (GettableStateVar Double) where
  getField = constraintImpulse

addDampedRotarySpring :: Body -> Body -> Double -> Double -> Double -> IO DampedRotarySpring
addDampedRotarySpring body1 body2 distance springConstant damping = do
  dampedRotarySpring <- C.dampedRotarySpringNew body1 body2 distance springConstant damping
  space <- get (C.bodySpace body1)
  C.spaceAddConstraint space dampedRotarySpring
  pure $ DampedRotarySpring dampedRotarySpring

dampedRotarySpringAngle :: DampedRotarySpring -> StateVar Double
dampedRotarySpringAngle (DampedRotarySpring c) = C.dampedRotarySpringRestAngle c

dampedRotarySpringStiffness :: DampedRotarySpring -> StateVar Double
dampedRotarySpringStiffness (DampedRotarySpring c) = C.dampedRotarySpringStiffness c

dampedRotarySpringDamping :: DampedRotarySpring -> StateVar Double
dampedRotarySpringDamping (DampedRotarySpring c) = C.dampedRotarySpringDamping c

instance HasField "angle" DampedRotarySpring (StateVar Double) where
  getField = dampedRotarySpringAngle

instance HasField "stiffness" DampedRotarySpring (StateVar Double) where
  getField = dampedRotarySpringStiffness

instance HasField "damping" DampedRotarySpring (StateVar Double) where
  getField = dampedRotarySpringDamping

newtype RotaryLimitJoint = RotaryLimitJoint C.Constraint

instance IsConstraint RotaryLimitJoint where
  getConstraint (RotaryLimitJoint c) = c

instance HasField "bodyA" RotaryLimitJoint (GettableStateVar Body) where
  getField = constraintBodyA

instance HasField "bodyB" RotaryLimitJoint (GettableStateVar Body) where
  getField = constraintBodyB

instance HasField "maxForce" RotaryLimitJoint (StateVar Double) where
  getField = constraintMaxForce

instance HasField "errorBias" RotaryLimitJoint (StateVar Double) where
  getField = constraintErrorBias

instance HasField "maxBias" RotaryLimitJoint (StateVar Double) where
  getField = constraintMaxBias

instance HasField "collideBodies" RotaryLimitJoint (StateVar Bool) where
  getField = constraintCollideBodies

instance HasField "impulse" RotaryLimitJoint (GettableStateVar Double) where
  getField = constraintImpulse

addRotaryLimitJoint :: Body -> Body -> Double -> Double -> IO RotaryLimitJoint
addRotaryLimitJoint body1 body2 minAngle maxAngle = do
  rotaryLimitJoint <- C.rotaryLimitJointNew body1 body2 minAngle maxAngle
  space <- get (C.bodySpace body1)
  C.spaceAddConstraint space rotaryLimitJoint
  pure $ RotaryLimitJoint rotaryLimitJoint

rotaryLimitJointMin :: RotaryLimitJoint -> StateVar Double
rotaryLimitJointMin (RotaryLimitJoint c) = C.rotaryLimitJointMin c

rotaryLimitJointMax :: RotaryLimitJoint -> StateVar Double
rotaryLimitJointMax (RotaryLimitJoint c) = C.rotaryLimitJointMax c

instance HasField "min" RotaryLimitJoint (StateVar Double) where
  getField = rotaryLimitJointMin

instance HasField "max" RotaryLimitJoint (StateVar Double) where
  getField = rotaryLimitJointMax

newtype RatchetJoint = RatchetJoint C.Constraint

instance IsConstraint RatchetJoint where
  getConstraint (RatchetJoint c) = c

instance HasField "bodyA" RatchetJoint (GettableStateVar Body) where
  getField = constraintBodyA

instance HasField "bodyB" RatchetJoint (GettableStateVar Body) where
  getField = constraintBodyB

instance HasField "maxForce" RatchetJoint (StateVar Double) where
  getField = constraintMaxForce

instance HasField "errorBias" RatchetJoint (StateVar Double) where
  getField = constraintErrorBias

instance HasField "maxBias" RatchetJoint (StateVar Double) where
  getField = constraintMaxBias

instance HasField "collideBodies" RatchetJoint (StateVar Bool) where
  getField = constraintCollideBodies

instance HasField "impulse" RatchetJoint (GettableStateVar Double) where
  getField = constraintImpulse

addRatchetJoint :: Body -> Body -> Double -> Double -> IO RatchetJoint
addRatchetJoint body1 body2 minAngle maxAngle = do
  ratchetJoint <- C.ratchetJointNew body1 body2 minAngle maxAngle
  space <- get (C.bodySpace body1)
  C.spaceAddConstraint space ratchetJoint
  pure $ RatchetJoint ratchetJoint

ratchetJointAngle :: RatchetJoint -> StateVar Double
ratchetJointAngle (RatchetJoint c) = C.ratchetJointAngle c

ratchetJointPhase :: RatchetJoint -> StateVar Double
ratchetJointPhase (RatchetJoint c) = C.ratchetJointPhase c

ratchetJointRatchet :: RatchetJoint -> StateVar Double
ratchetJointRatchet (RatchetJoint c) = C.ratchetJointRatchet c

instance HasField "angle" RatchetJoint (StateVar Double) where
  getField = ratchetJointAngle

instance HasField "phase" RatchetJoint (StateVar Double) where
  getField = ratchetJointPhase

instance HasField "ratchet" RatchetJoint (StateVar Double) where
  getField = ratchetJointRatchet

newtype GearJoint = GearJoint C.Constraint

instance IsConstraint GearJoint where
  getConstraint (GearJoint c) = c

instance HasField "bodyA" GearJoint (GettableStateVar Body) where
  getField = constraintBodyA

instance HasField "bodyB" GearJoint (GettableStateVar Body) where
  getField = constraintBodyB

instance HasField "maxForce" GearJoint (StateVar Double) where
  getField = constraintMaxForce

instance HasField "errorBias" GearJoint (StateVar Double) where
  getField = constraintErrorBias

instance HasField "maxBias" GearJoint (StateVar Double) where
  getField = constraintMaxBias

instance HasField "collideBodies" GearJoint (StateVar Bool) where
  getField = constraintCollideBodies

instance HasField "impulse" GearJoint (GettableStateVar Double) where
  getField = constraintImpulse

addGearJoint :: Body -> Body -> Double -> Double -> IO GearJoint
addGearJoint body1 body2 offset ratio = do
  gearJoint <- C.gearJointNew body1 body2 offset ratio
  space <- get (C.bodySpace body1)
  C.spaceAddConstraint space gearJoint
  pure $ GearJoint gearJoint

gearJointPhase :: GearJoint -> StateVar Double
gearJointPhase (GearJoint c) = C.gearJointPhase c

gearJointRatio :: GearJoint -> StateVar Double
gearJointRatio (GearJoint c) = C.gearJointRatio c

instance HasField "phase" GearJoint (StateVar Double) where
  getField = gearJointPhase

instance HasField "ratio" GearJoint (StateVar Double) where
  getField = gearJointRatio

newtype SimpleMotor = SimpleMotor C.Constraint

instance IsConstraint SimpleMotor where
  getConstraint (SimpleMotor c) = c

instance HasField "bodyA" SimpleMotor (GettableStateVar Body) where
  getField = constraintBodyA

instance HasField "bodyB" SimpleMotor (GettableStateVar Body) where
  getField = constraintBodyB

instance HasField "maxForce" SimpleMotor (StateVar Double) where
  getField = constraintMaxForce

instance HasField "errorBias" SimpleMotor (StateVar Double) where
  getField = constraintErrorBias

instance HasField "maxBias" SimpleMotor (StateVar Double) where
  getField = constraintMaxBias

instance HasField "collideBodies" SimpleMotor (StateVar Bool) where
  getField = constraintCollideBodies

instance HasField "impulse" SimpleMotor (GettableStateVar Double) where
  getField = constraintImpulse

addSimpleMotor :: Body -> Body -> Double -> IO SimpleMotor
addSimpleMotor body1 body2 angularVelocity = do
  simpleMotor <- C.simpleMotorNew body1 body2 angularVelocity
  space <- get (C.bodySpace body1)
  C.spaceAddConstraint space simpleMotor
  pure $ SimpleMotor simpleMotor

simpleMotorRate :: SimpleMotor -> StateVar Double
simpleMotorRate (SimpleMotor c) = C.simpleMotorRate c

instance HasField "rate" SimpleMotor (StateVar Double) where
  getField = simpleMotorRate

-- Collisions

newtype Collision = Collision C.Arbiter

newtype ModifiableCollision = ModifiableCollision C.Arbiter

collisionRestitution :: Collision -> GettableStateVar Double
collisionRestitution (Collision arbiter) = get (C.arbiterRestitution arbiter)

modifiableCollisionRestitution :: ModifiableCollision -> StateVar Double
modifiableCollisionRestitution (ModifiableCollision arbiter) = C.arbiterRestitution arbiter

instance HasField "restitution" Collision (GettableStateVar Double) where
  getField = collisionRestitution

instance HasField "restitution" ModifiableCollision (StateVar Double) where
  getField = modifiableCollisionRestitution

collisionFriction :: Collision -> GettableStateVar Double
collisionFriction (Collision arbiter) = get (C.arbiterFriction arbiter)

modifiableCollisionFriction :: ModifiableCollision -> StateVar Double
modifiableCollisionFriction (ModifiableCollision arbiter) = C.arbiterFriction arbiter

instance HasField "friction" Collision (GettableStateVar Double) where
  getField = collisionFriction

instance HasField "friction" ModifiableCollision (StateVar Double) where
  getField = modifiableCollisionFriction

collisionSurfaceVelocity :: Collision -> GettableStateVar (V2 Double)
collisionSurfaceVelocity (Collision arbiter) = vectToV2 <$> get (C.arbiterSurfaceVelocity arbiter)

modifiableCollisionSurfaceVelocity :: ModifiableCollision -> StateVar (V2 Double)
modifiableCollisionSurfaceVelocity (ModifiableCollision arbiter) = mapStateVar v2ToVect vectToV2 $ C.arbiterSurfaceVelocity arbiter

instance HasField "surfaceVelocity" Collision (GettableStateVar (V2 Double)) where
  getField = collisionSurfaceVelocity

instance HasField "surfaceVelocity" ModifiableCollision (StateVar (V2 Double)) where
  getField = modifiableCollisionSurfaceVelocity

collisionCount :: Collision -> GettableStateVar Int
collisionCount (Collision arbiter) = C.arbiterCount arbiter

modifiableCollisionCount :: ModifiableCollision -> GettableStateVar Int
modifiableCollisionCount (ModifiableCollision arbiter) = C.arbiterCount arbiter

instance HasField "count" Collision (GettableStateVar Int) where
  getField = collisionCount

instance HasField "count" ModifiableCollision (GettableStateVar Int) where
  getField = modifiableCollisionCount

collisionNormal :: Collision -> GettableStateVar (V2 Double)
collisionNormal (Collision arbiter) = vectToV2 <$> C.arbiterNormal arbiter

modifiableCollisionNormal :: ModifiableCollision -> GettableStateVar (V2 Double)
modifiableCollisionNormal (ModifiableCollision arbiter) = vectToV2 <$> C.arbiterNormal arbiter

instance HasField "normal" Collision (GettableStateVar (V2 Double)) where
  getField = collisionNormal

instance HasField "normal" ModifiableCollision (GettableStateVar (V2 Double)) where
  getField = modifiableCollisionNormal

collisionPointA :: Collision -> Int -> GettableStateVar (V2 Double)
collisionPointA (Collision arbiter) i = vectToV2 <$> C.arbiterPointA arbiter i

modifiableCollisionPointA :: ModifiableCollision -> Int -> GettableStateVar (V2 Double)
modifiableCollisionPointA (ModifiableCollision arbiter) i = vectToV2 <$> C.arbiterPointA arbiter i

instance HasField "pointA" Collision (Int -> GettableStateVar (V2 Double)) where
  getField = collisionPointA

instance HasField "pointA" ModifiableCollision (Int -> GettableStateVar (V2 Double)) where
  getField = modifiableCollisionPointA

collisionPointB :: Collision -> Int -> GettableStateVar (V2 Double)
collisionPointB (Collision arbiter) i = vectToV2 <$> C.arbiterPointB arbiter i

modifiableCollisionPointB :: ModifiableCollision -> Int -> GettableStateVar (V2 Double)
modifiableCollisionPointB (ModifiableCollision arbiter) i = vectToV2 <$> C.arbiterPointB arbiter i

instance HasField "pointB" Collision (Int -> GettableStateVar (V2 Double)) where
  getField = collisionPointB

instance HasField "pointB" ModifiableCollision (Int -> GettableStateVar (V2 Double)) where
  getField = modifiableCollisionPointB

collisionDepth :: Collision -> Int -> GettableStateVar Double
collisionDepth (Collision arbiter) = C.arbiterDepth arbiter

modifiableCollisionDepth :: ModifiableCollision -> Int -> GettableStateVar Double
modifiableCollisionDepth (ModifiableCollision arbiter) = C.arbiterDepth arbiter

instance HasField "depth" Collision (Int -> GettableStateVar Double) where
  getField = collisionDepth

instance HasField "depth" ModifiableCollision (Int -> GettableStateVar Double) where
  getField = modifiableCollisionDepth

collisionIsFirstContact :: Collision -> GettableStateVar Bool
collisionIsFirstContact (Collision arbiter) = C.arbiterIsFirstContact arbiter

modifiableCollisionIsFirstContact :: ModifiableCollision -> GettableStateVar Bool
modifiableCollisionIsFirstContact (ModifiableCollision arbiter) = C.arbiterIsFirstContact arbiter

instance HasField "isFirstContact" Collision (GettableStateVar Bool) where
  getField = collisionIsFirstContact

instance HasField "isFirstContact" ModifiableCollision (GettableStateVar Bool) where
  getField = modifiableCollisionIsFirstContact

collisionIsRemoval :: Collision -> GettableStateVar Bool
collisionIsRemoval (Collision arbiter) = C.arbiterIsRemoval arbiter

modifiableCollisionIsRemoval :: ModifiableCollision -> GettableStateVar Bool
modifiableCollisionIsRemoval (ModifiableCollision arbiter) = C.arbiterIsRemoval arbiter

instance HasField "isRemoval" Collision (GettableStateVar Bool) where
  getField = collisionIsRemoval

instance HasField "isRemoval" ModifiableCollision (GettableStateVar Bool) where
  getField = modifiableCollisionIsRemoval

collisionShapes :: Collision -> GettableStateVar (Shape, Shape)
collisionShapes (Collision arbiter) = C.arbiterShapes arbiter

modifiableCollisionShapes :: ModifiableCollision -> GettableStateVar (Shape, Shape)
modifiableCollisionShapes (ModifiableCollision arbiter) = C.arbiterShapes arbiter

instance HasField "shapes" Collision (GettableStateVar (Shape, Shape)) where
  getField = collisionShapes

instance HasField "shapes" ModifiableCollision (GettableStateVar (Shape, Shape)) where
  getField = modifiableCollisionShapes

collisionBodies :: Collision -> GettableStateVar (Body, Body)
collisionBodies (Collision arbiter) = C.arbiterBodies arbiter

modifiableCollisionBodies :: ModifiableCollision -> GettableStateVar (Body, Body)
modifiableCollisionBodies (ModifiableCollision arbiter) = C.arbiterBodies arbiter

instance HasField "bodies" Collision (GettableStateVar (Body, Body)) where
  getField = collisionBodies

instance HasField "bodies" ModifiableCollision (GettableStateVar (Body, Body)) where
  getField = modifiableCollisionBodies

newtype CollisionSpace = CollisionSpace C.Space

instance IsSpace CollisionSpace where
  getSpace (CollisionSpace space) = space
  addBody (CollisionSpace space) = addBody space

class IsCollision collision where
  getArbiter :: collision -> C.Arbiter

instance IsCollision Collision where
  getArbiter (Collision collision) = collision

instance IsCollision ModifiableCollision where
  getArbiter (ModifiableCollision collision) = collision

schedulePostStepWork :: (IsCollision collision) => CollisionSpace -> collision -> (Space -> IO ()) -> IO Bool
schedulePostStepWork (CollisionSpace space) collision action = do
  let arbiter = getArbiter collision
  (shape1, shape2) <- C.arbiterShapes arbiter
  let key = hash shape1 * hash shape2
  addKeyedCallback space key (action space)

data ModifyCollisionHandler a = ModifyCollisionHandler
  { collisionTypes :: !(C.CollisionType -> C.CollisionType -> (C.CollisionType, C.CollisionType)),
    begin :: !(Maybe (Collision -> CollisionSpace -> IO (Bool, Maybe a))),
    preSolve :: !(Maybe (ModifiableCollision -> CollisionSpace -> IO (Bool, Maybe a))),
    postSolve :: !(Maybe (Collision -> CollisionSpace -> IO (Maybe a))),
    separate :: !(Maybe (Collision -> CollisionSpace -> IO (Maybe a)))
  }

idCollisionHandler :: ModifyCollisionHandler a
idCollisionHandler =
  ModifyCollisionHandler
    { collisionTypes = (,),
      begin = Nothing,
      preSolve = Nothing,
      postSolve = Nothing,
      separate = Nothing
    }

modifyCollisionHandler :: Space -> C.CollisionType -> C.CollisionType -> ModifyCollisionHandler a -> IO (Event a)
modifyCollisionHandler space typeA typeB mch = do
  handlerPtr <- C.spaceAddCollisionHandler space typeA typeB
  modifyCollisionHandlerPtr handlerPtr mch

modifyWildcardCollisionHandler :: Space -> C.CollisionType -> ModifyCollisionHandler a -> IO (Event a)
modifyWildcardCollisionHandler space typeA mch = do
  handlerPtr <- C.spaceAddWildcardHandler space typeA
  modifyCollisionHandlerPtr handlerPtr mch

modifyDefaultCollisionHandler :: Space -> ModifyCollisionHandler a -> IO (Event a)
modifyDefaultCollisionHandler space mch = do
  handlerPtr <- C.spaceAddDefaultCollisionHandler space
  modifyCollisionHandlerPtr handlerPtr mch

handleWildcardBeginA :: Collision -> CollisionSpace -> IO Bool
handleWildcardBeginA (Collision arbiter) (CollisionSpace space) = C.arbiterCallWildcardBeginA arbiter space

handleWildcardBeginB :: Collision -> CollisionSpace -> IO Bool
handleWildcardBeginB (Collision arbiter) (CollisionSpace space) = C.arbiterCallWildcardBeginB arbiter space

handleWildcardPreSolveA :: ModifiableCollision -> CollisionSpace -> IO Bool
handleWildcardPreSolveA (ModifiableCollision arbiter) (CollisionSpace space) = C.arbiterCallWildcardPreSolveA arbiter space

handleWildcardPreSolveB :: ModifiableCollision -> CollisionSpace -> IO Bool
handleWildcardPreSolveB (ModifiableCollision arbiter) (CollisionSpace space) = C.arbiterCallWildcardPreSolveB arbiter space

handleWildcardPostSolveA :: Collision -> CollisionSpace -> IO ()
handleWildcardPostSolveA (Collision arbiter) (CollisionSpace space) = C.arbiterCallWildcardPostSolveA arbiter space

handleWildcardPostSolveB :: Collision -> CollisionSpace -> IO ()
handleWildcardPostSolveB (Collision arbiter) (CollisionSpace space) = C.arbiterCallWildcardPostSolveB arbiter space

handleWildcardSeparateA :: Collision -> CollisionSpace -> IO ()
handleWildcardSeparateA (Collision arbiter) (CollisionSpace space) = C.arbiterCallWildcardSeparateA arbiter space

handleWildcardSeparateB :: Collision -> CollisionSpace -> IO ()
handleWildcardSeparateB (Collision arbiter) (CollisionSpace space) = C.arbiterCallWildcardSeparateB arbiter space

modifyCollisionHandlerPtr :: C.CollisionHandlerPtr -> ModifyCollisionHandler a -> IO (Event a)
modifyCollisionHandlerPtr handlerPtr (ModifyCollisionHandler collisionTypes begin preSolve postSolve separate) = do
  eventTriggers <- newIORef M.empty

  let updateCollisionTypes ch =
        let (newCollisionTypeA, newCollisionTypeB) = collisionTypes (C.chTypeA ch) (C.chTypeB ch)
         in ch {C.chTypeA = newCollisionTypeA, C.chTypeB = newCollisionTypeB}
  updateBegin <- makeCallback eventTriggers begin Collision id C.mkCallbackB (\cb h -> h {C.chBeginFunc = cb})
  updatePreSolve <- makeCallback eventTriggers preSolve ModifiableCollision id C.mkCallbackB (\cb h -> h {C.chPreSolveFunc = cb})
  updatePostSolve <- makeCallback eventTriggers postSolve Collision ((),) C.mkCallback (\cb h -> h {C.chPostSolveFunc = cb})
  updateSeparate <- makeCallback eventTriggers separate Collision ((),) C.mkCallback (\cb h -> h {C.chSeparateFunc = cb})

  C.modifyCollisionHandler handlerPtr $ pure . updateBegin . updatePreSolve . updatePostSolve . updateSeparate . updateCollisionTypes

  let event = callback $ \fin trigger -> do
        key <- newUnique
        modifyIORef' eventTriggers $ M.insert key trigger
        addFinalizer fin $ modifyIORef' eventTriggers $ M.delete key

  if or [isJust begin, isJust preSolve, isJust postSolve, isJust separate]
    then pure event
    else pure mempty
  where
    makeCallback eventTriggers maybeCallback makeCollision extractResult makeCallback insertFunction =
      case maybeCallback of
        Nothing -> pure id
        Just f -> do
          let newF arbiter collisionSpace _ = do
                (b, ma) <- extractResult <$> f (makeCollision arbiter) (CollisionSpace collisionSpace)
                case ma of
                  Nothing -> pure b
                  Just a -> readIORef eventTriggers >>= traverse_ ($ a) >> pure b

          cb <- makeCallback newF

          pure (insertFunction cb)

data PostStepCallbacks = PostStepCallbacks
  { unkeyedCallbacks :: !(IO ()),
    keyedCallbacks :: !(IM.IntMap (IO ()))
  }

emptyPostStepCallbacks :: PostStepCallbacks
emptyPostStepCallbacks = PostStepCallbacks mempty mempty

addKeyedCallback :: Space -> Int -> IO () -> IO Bool
addKeyedCallback space key action = do
  callbacks <- get (C.spaceUserData space) >>= deRefStablePtr . castPtrToStablePtr
  atomicModifyIORef' callbacks $ \postStepCallbacks ->
    let exists = IM.member key postStepCallbacks.keyedCallbacks
     in if exists
          then (postStepCallbacks, False)
          else (postStepCallbacks {keyedCallbacks = IM.insert key action postStepCallbacks.keyedCallbacks}, True)

v2ToVect :: V2 Double -> Vect
v2ToVect (V2 x y) = Vect x y

vectToV2 :: Vect -> V2 Double
vectToV2 (Vect x y) = V2 x y
