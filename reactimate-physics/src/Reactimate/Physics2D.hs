{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoFieldSelectors #-}

-- This module wraps functionality from the [chiphunk](https://hackage.haskell.org/package/chiphunk) library which uses the [chipmunk](https://chipmunk-physics.net/) physics library.
module Reactimate.Physics2D where

--   ( withPhysics,

--     -- * Space
--     Space,
--     spaceStep,
--     withSubspace,
--     Subspace,
--     spaceGravity,
--     C.spaceDamping,
--     C.spaceIdleSpeedThreshold,
--     C.spaceSleepTimeThreshold,
--     C.spaceCollisionSlop,
--     C.spaceCollisionBias,
--     C.spaceCollisionPersistence,
--     C.spaceCurrentTimeStep,
--     C.spaceStaticBody,

--     -- * Body
--     Body,
--     addDynamicBody,
--     addKinematicBody,
--     addStaticBody,
--     removeBody,
--     C.bodyType,
--     C.BodyType (..),
--     C.bodyMass,
--     bodyPosition,
--     bodyCenterOfGravity,
--     bodyVelocity,
--     bodyForce,
--     bodyAngle,
--     C.bodyAngularVelocity,
--     C.bodyTorque,

--     -- ** Moment calculation
--     C.momentForCircle,
--     momentForSegment,
--     momentForPoly,
--     C.momentForBox,

--     -- ** Coordinate Conversion
--     bodyLocalToWorld,
--     bodyWorldToLocal,
--     bodyVelocityAtWorldPoint,

--     -- ** Apply Force
--     bodyApplyForceAtWorldPoint,
--     bodyApplyForceAtLocalPoint,
--     bodyApplyImpulseAtWorldPoint,
--     bodyApplyImpulseAtLocalPoint,

--     -- * Shapes
--     addCircleShape,
--     addBoxShape,
--     addPolyShape,
--     addSegmentShape,
--     removeShape,
--     segmentShapeNeighbors,
--     C.shapeSensor,
--     C.shapeElasticity,
--     C.shapeFriction,
--     shapeSurfaceVelocity,
--     C.shapeCollisionType,
--     C.CollisionType,
--     C.shapeMass,
--     C.shapeDensity,
--     C.shapeFilter,
--     C.ShapeFilter (..),

--     -- * Constraints
--     C.Constraint,
--     IsConstraint (..),
--     constraintBodyA,
--     constraintBodyB,
--     constraintMaxForce,
--     constraintErrorBias,
--     constraintMaxBias,
--     constraintCollideBodies,
--     constraintImpulse,
--     removeConstraint,

--     -- ** Pin Joint
--     PinJoint,
--     addPinJoint,
--     pinJointAnchorA,
--     pinJointAnchorB,
--     pinJointDistance,

--     -- ** Slide Joint
--     SlideJoint,
--     addSlideJoint,
--     slideJointAnchorA,
--     slideJointAnchorB,
--     slideJointMin,
--     slideJointMax,

--     -- ** Pivot Joint
--     PivotJoint,
--     addPivotJoint,
--     pivotJointAnchorA,
--     pivotJointAnchorB,
--     addGrooveJoint,

--     -- ** Groove Joint
--     GrooveJoint,
--     grooveJointGrooveA,
--     grooveJointGrooveB,
--     grooveJointAnchorB,

--     -- ** Damped Spring
--     DampedSpring,
--     addDampedSpring,
--     dampedSpringAnchorA,
--     dampedSpringAnchorB,
--     dampedSpringDistance,
--     dampedSpringStiffness,
--     dampedSpringDamping,

--     -- ** Damped Rotary Spring,
--     DampedRotarySpring,
--     addDampedRotarySpring,
--     dampedRotarySpringAngle,
--     dampedRotarySpringStiffness,
--     dampedRotarySpringDamping,

--     -- ** Rotary Limit Joint
--     RotaryLimitJoint,
--     addRotaryLimitJoint,
--     rotaryLimitJointMin,
--     rotaryLimitJointMax,

--     -- ** Ratchet Joint
--     RatchetJoint,
--     addRatchetJoint,
--     ratchetJointAngle,
--     ratchetJointPhase,
--     ratchetJointRatchet,

--     -- ** Gear Joint
--     GearJoint,
--     addGearJoint,
--     gearJointPhase,
--     gearJointRatio,

--     -- ** Simple Motor
--     SimpleMotor,
--     addSimpleMotor,
--     simpleMotorRate,

--     -- * Collisions
--     Collision,
--     ModifiableCollision,

--     -- ** Collision properties
--     collisionRestitution,
--     modifiableCollisionRestitution,
--     collisionFriction,
--     modifiableCollisionFriction,
--     collisionSurfaceVelocity,
--     modifiableCollisionSurfaceVelocity,
--     collisionCount,
--     modifiableCollisionCount,
--     collisionNormal,
--     modifiableCollisionNormal,
--     collisionPointA,
--     modifiableCollisionPointA,
--     collisionPointB,
--     modifiableCollisionPointB,
--     collisionDepth,
--     modifiableCollisionDepth,
--     collisionIsFirstContact,
--     modifiableCollisionIsFirstContact,
--     collisionIsRemoval,
--     modifiableCollisionIsRemoval,
--     collisionShapes,
--     modifiableCollisionShapes,
--     collisionBodies,
--     modifiableCollisionBodies,

--     -- ** Modify Collision Handlers
--     ModifyCollisionHandler (..),
--     CollisionSpace,
--     idCollisionHandler,
--     modifyCollisionHandler,
--     modifyWildcardCollisionHandler,
--     modifyDefaultCollisionHandler,

--     -- *** Safely change objects
--     schedulePostStepWork,
--     alwaysSchedulePostStepWork,

--     -- *** Invoke more general handlers
--     handleWildcardBeginA,
--     handleWildcardBeginB,
--     handleWildcardPreSolveA,
--     handleWildcardPreSolveB,
--     handleWildcardPostSolveA,
--     handleWildcardPostSolveB,
--     handleWildcardSeparateA,
--     handleWildcardSeparateB,

--     -- * StateVar
--     StateVar,
--     GettableStateVar,
--     SettableStateVar,
--     HasGetter (..),
--     HasSetter (..),
--   )
-- where

import Chiphunk.Low qualified as C
import Control.Monad
import Control.Monad (forM_, when, (>=>))
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Coerce (coerce)
import Data.Foldable (for_, traverse_)
import Data.Functor ((<&>))
import Data.Hashable (Hashable (hash))
import Data.IORef
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, isJust)
import Data.Set qualified as S
import Data.StateVar
import Data.Unique (Unique, newUnique)
import Data.Word (Word32)
import Foreign (Storable (..), castFunPtr, freeHaskellFunPtr)
import Foreign.Marshal (malloc)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (castPtr)
import Foreign.StablePtr
import GHC.Records (HasField (..))
import Linear.V2
import Reactimate
import Unsafe.Coerce (unsafeCoerce)

data Physics s = Physics
  { space :: C.Space,
    ioe :: IOE s,
    postStepCallbacks :: IORef (PostStepCallbacks s)
  }

data PostStepCallbacks s = PostStepCallbacks
  { keyed :: M.Map (Unique, C.Shape, C.Shape) (Step s ()),
    unkeyed :: Step s ()
  }

runPhysics ::
  (IOE :> es) =>
  (GlobalCollisionHandler s) ->
  Setup (Physics : es) s a ->
  Setup es s a
runPhysics handlers innerSetup = do
  ioe@(IOE lift) <- getHandle
  space <- prestep $ lift C.spaceNew
  postStepCallbacks <- prestep $ lift $ newIORef emptyPostStepCallbacks
  registerGlobalCollisionHandlers postStepCallbacks space handlers
  finalize $ do
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

  let physics = Physics {space, ioe, postStepCallbacks}
  runHandle physics $ innerSetup

-- SPACE

-- getSpace :: (Physics :> es) => Setup es

-- | Allocates a `Space`, which is used for physics simulation.
--
-- Do not use `Space` outside the signal. The `Space` will be freed when this signal function does not run anymore.
-- withPhysics :: (Space -> Signal a b) -> Signal a b
-- withPhysics =
--   allocateResource
--     ( \fin -> do
--         space <- C.spaceNew

--         postStepCallbacks <- newIORef emptyPostStepCallbacks
--         stablePostStepCallbacks <- newStablePtr postStepCallbacks

--         C.spaceUserData space $= castStablePtrToPtr stablePostStepCallbacks

--         addFinalizer fin $ do
--           shapesRef <- newIORef []
--           bodiesRef <- newIORef []
--           constraintsRef <- newIORef []

--           C.spaceEachShape space (\shape _ -> modifyIORef' shapesRef (shape :)) C.nullPtr

--           C.spaceEachBody space (\body _ -> modifyIORef' bodiesRef (body :)) C.nullPtr

--           C.spaceEachConstraint space (\body _ -> modifyIORef' constraintsRef (body :)) C.nullPtr

--           C.spaceFree space

--           shapes <- readIORef shapesRef
--           forM_ shapes C.shapeFree

--           constraints <- readIORef constraintsRef
--           forM_ constraints C.constraintFree

--           bodies <- readIORef bodiesRef
--           forM_ bodies C.bodyFree
--         pure space
--     )

-- spaceToPhysics :: Space s -> Physics s
-- spaceToPhysics (Space {space, ioe}) = Physics space ioe

getPhysics :: (Physics :> es) => Setup es s (Physics s)
getPhysics = getHandle

getSpace :: (Physics :> es) => Setup es s (Space s)
getSpace = do
  Physics chipmunkSpace ioe@(IOE lift) _ <- getHandle
  bodiesRef <- prestep $ lift $ newIORef S.empty
  bodiesPtr <- prestep $ lift $ newStablePtr bodiesRef
  spaceBody <- prestep $ lift $ do
    cBody <- C.bodyNewStatic
    C.spaceAddBody chipmunkSpace cBody
    pure cBody
  runHandle ioe $ finalize $ do
    freeStablePtr bodiesPtr
    bodies <- readIORef bodiesRef
    removeAndFreeBodyFromSpace chipmunkSpace spaceBody
    for_ bodies $ \cBody -> do
      removeAndFreeBodyFromSpace chipmunkSpace cBody
  let space = Space chipmunkSpace bodiesPtr spaceBody ioe
  pure space

emptyPostStepCallbacks :: PostStepCallbacks s
emptyPostStepCallbacks = PostStepCallbacks M.empty (pure ())

stepPhysics :: Physics s -> Double -> Step s ()
stepPhysics (Physics space (IOE lift) postStepCallbacks) time = do
  lift $ C.spaceStep space time
  PostStepCallbacks keyed unkeyed <- lift $ atomicModifyIORef' postStepCallbacks (emptyPostStepCallbacks,)
  unkeyed
  sequence_ keyed

-- ptr <- get (C.spaceUserData space)
-- postStepCallbacks <- deRefStablePtr (castPtrToStablePtr ptr)
-- PostStepCallbacks unkeyed keyed <- readIORef postStepCallbacks
-- writeIORef postStepCallbacks emptyPostStepCallbacks
-- unkeyed
-- forM_ keyed id

-- | Global gravity applied to the space. Defaults to (V2 0 0).
spaceGravity :: Physics s -> Var s (V2 Double)
spaceGravity Physics {space, ioe} = mapToVar ioe v2ToVect vectToV2 (C.spaceGravity space)

instance HasField "gravity" (Physics s) (Var s (V2 Double)) where
  getField = spaceGravity

instance HasField "damping" (Physics s) (Var s Double) where
  getField physics = toVar physics.ioe (C.spaceDamping physics.space)

instance HasField "idleSpeedThreshold" (Physics s) (Var s Double) where
  getField physics = toVar physics.ioe (C.spaceIdleSpeedThreshold physics.space)

instance HasField "sleepTimeThreshold" (Physics s) (Var s Double) where
  getField physics = toVar physics.ioe (C.spaceSleepTimeThreshold physics.space)

instance HasField "collisionSlop" (Physics s) (Var s Double) where
  getField physics = toVar physics.ioe (C.spaceCollisionSlop physics.space)

instance HasField "collisionBias" (Physics s) (Var s Double) where
  getField physics = toVar physics.ioe (C.spaceCollisionBias physics.space)

instance HasField "collisionPersistence" (Physics s) (Var s Word32) where
  getField physics = toVar physics.ioe (C.spaceCollisionPersistence physics.space)

instance HasField "currentTimeStep" (Physics s) (Step s Double) where
  getField physics = toStep physics.ioe (C.spaceCurrentTimeStep physics.space)

instance HasField "staticBody" (Space s) (Body s) where
  getField Space {ioe, staticBody'} = Body staticBody' ioe

-- -- | A `Subspace` is a part of a `Space` and can be used to manage groups of bodies.
-- -- If a `Subspace` is switched out, all its `Body`s are removed.
-- data Subspace = Subspace
--   { space :: Space,
--     bodies :: StablePtr (IORef (S.Set Body))
--   }

-- class IsSpace space where
--   getSpace :: space -> Space
--   addBody :: space -> Body -> IO ()

data Space s = Space
  { space :: C.Space,
    bodiesPtr :: StablePtr (IORef (S.Set C.Body)),
    staticBody' :: C.Body,
    ioe :: IOE s
  }

data Body s = Body
  { body :: C.Body,
    ioe :: IOE s
  }

data Shape s = Shape
  { shape :: C.Shape,
    ioe :: IOE s
  }

removeAndFreeBodyFromSpace :: C.Space -> C.Body -> IO ()
removeAndFreeBodyFromSpace space body = do
  shapesRef <- newIORef []
  constraintsRef <- newIORef []

  C.bodyEachShape
    body
    (\_ shape _ -> modifyIORef' shapesRef (shape :))
    C.nullPtr

  C.bodyEachConstraint
    body
    (\_ constraint _ -> modifyIORef' constraintsRef (constraint :))
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

addBody :: StablePtr (IORef (S.Set C.Body)) -> C.Space -> C.Body -> IO ()
addBody bodiesPtr space body = do
  C.spaceAddBody space body
  bodiesRef <- deRefStablePtr bodiesPtr
  modifyIORef' bodiesRef (S.insert body)
  C.bodyUserData body $= castStablePtrToPtr bodiesPtr

-- | Removes a `Body` and it's shapes from the `Space`. You may not use the `Body` afterwards.
removeBody :: Body s -> Step s ()
removeBody (Body body (IOE lift)) = lift $ do
  space <- get (C.bodySpace body)
  bodiesPtr <- get (C.bodyUserData body)
  bodiesRef <- deRefStablePtr (castPtrToStablePtr bodiesPtr)
  modifyIORef' bodiesRef (S.delete body)
  removeAndFreeBodyFromSpace space body

-- instance IsSpace Space where
--   getSpace = id
--   addBody space body = do
--     C.spaceAddBody space body
--     C.bodyUserData body $= C.nullPtr

-- instance IsSpace Subspace where
--   getSpace = (.space)
--   addBody (Subspace space bodiesPtr) body = do
--     C.spaceAddBody space body
--     C.bodyUserData body $= castStablePtrToPtr bodiesPtr
--     bodies <- deRefStablePtr bodiesPtr
--     modifyIORef' bodies $ S.insert body

-- -- | Creates a `Subspace` which keeps tracks of its contained bodies. All bodies of
-- -- the `Subspace` get deleted after the `withSubspace` gets switched out.
-- withSubspace :: (IsSpace space) => space -> (Subspace -> Signal a b) -> Signal a b
-- withSubspace space = allocateResource $ \fin -> do
--   bodiesRef <- newIORef S.empty
--   addFinalizer fin $ do
--     bodies <- readIORef bodiesRef
--     forM_ bodies $ \body ->
--       removeAndFreeBodyFromSpace (getSpace space) body
--   bodiesPtr <- newStablePtr bodiesRef
--   pure $ Subspace (getSpace space) bodiesPtr

-- -- BODIES

-- | Add a dynamic body with mass and inertia.
addDynamicBody ::
  Space s ->
  -- | Mass of the body
  Double ->
  -- | Moment of the body. It's best to use the `moment*` functions for this.
  Double ->
  Step s (Body s)
addDynamicBody physics@Space {space, bodiesPtr, ioe = IOE lift} mass inertia = do
  body <- lift $ C.bodyNew mass inertia
  lift $ addBody bodiesPtr space body
  pure $ Body body physics.ioe

-- | Add a kinematic body which is not affected by forces. Control it by setting it's velocity
addKinematicBody :: Space s -> Step s (Body s)
addKinematicBody Space {space, bodiesPtr, ioe = ioe@(IOE lift)} = do
  body <- lift C.bodyNewKinematic
  lift $ addBody bodiesPtr space body
  pure $ Body body ioe

-- | Add a static body. Static bodies have infinite mass and you should not move them.
addStaticBody :: Space s -> Step s (Body s)
addStaticBody Space {space, bodiesPtr, ioe = ioe@(IOE lift)} = do
  body <- lift C.bodyNewStatic
  lift $ addBody bodiesPtr space body
  pure $ Body body ioe

-- | World position of the body
bodyPosition :: Body s -> Var s (V2 Double)
bodyPosition (Body body (IOE lift)) =
  let bodyPos = C.bodyPosition body
   in makeVar
        (lift $ vectToV2 <$> get bodyPos)
        ( \pos -> lift $ do
            bodyPos $= v2ToVect pos
            space <- get (C.bodySpace body)
            C.spaceReindexShapesForBody space body
        )

-- | Rotation of the angle
bodyAngle :: Body s -> Var s Double
bodyAngle (Body body (IOE lift)) =
  let bodyAngle' = C.bodyAngle body
   in makeVar
        (lift $ get bodyAngle')
        ( \angle -> lift $ do
            bodyAngle' $= angle
            space <- get $ C.bodySpace body
            C.spaceReindexShapesForBody space body
        )

-- | Location of the center of gravity in body local coordinates.
-- The default value is (0, 0), meaning the center of gravity is the same as the position of the body.
bodyCenterOfGravity :: Body s -> Var s (V2 Double)
bodyCenterOfGravity (Body body ioe) = mapToVar ioe v2ToVect vectToV2 (C.bodyCenterOfGravity body)

-- | Linear velocity of the center of gravity of the body
bodyVelocity :: Body s -> Var s (V2 Double)
bodyVelocity (Body body ioe) = mapToVar ioe v2ToVect vectToV2 (C.bodyVelocity body)

-- | Force applied to the center of gravity of the body. This value is reset for every time step.
bodyForce :: Body s -> Var s (V2 Double)
bodyForce (Body body ioe) = mapToVar ioe v2ToVect vectToV2 (C.bodyForce body)

bodyTorque :: Body s -> Var s Double
bodyTorque (Body body ioe) = toVar ioe (C.bodyTorque body)

bodyAngularVelocity :: Body s -> Var s Double
bodyAngularVelocity (Body body ioe) = toVar ioe (C.bodyAngularVelocity body)

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

-- | Convert from body local coordinates to world space coordinates
bodyLocalToWorld :: Body s -> V2 Double -> Step s (V2 Double)
bodyLocalToWorld (Body body (IOE lift)) local = lift $ vectToV2 <$> C.bodyLocalToWorld body (v2ToVect local)

-- | Convert from world space coordinates to body local coordinates.
bodyWorldToLocal :: Body s -> V2 Double -> Step s (V2 Double)
bodyWorldToLocal (Body body (IOE lift)) world = lift $ vectToV2 <$> C.bodyWorldToLocal body (v2ToVect world)

-- | Absolute velocity of the rigid body at the given world point.
bodyVelocityAtWorldPoint :: Body s -> V2 Double -> Step s (V2 Double)
bodyVelocityAtWorldPoint (Body body (IOE lift)) world = lift $ vectToV2 <$> C.bodyVelocityAtWorldPoint body (v2ToVect world)

-- | Add the force to body as if applied from the world point.
bodyApplyForceAtWorldPoint ::
  Body s ->
  -- | force
  V2 Double ->
  -- | point
  V2 Double ->
  Step s ()
bodyApplyForceAtWorldPoint (Body body (IOE lift)) force point = lift $ C.bodyApplyForceAtWorldPoint body (v2ToVect force) (v2ToVect point)

-- | Add the local force to body as if applied from the body local point.
bodyApplyForceAtLocalPoint ::
  Body s ->
  -- | force
  V2 Double ->
  -- | point
  V2 Double ->
  Step s ()
bodyApplyForceAtLocalPoint (Body body (IOE lift)) force point = lift $ C.bodyApplyForceAtLocalPoint body (v2ToVect force) (v2ToVect point)

-- | Add the impulse to body as if applied from the world point.
bodyApplyImpulseAtWorldPoint ::
  Body s ->
  -- | impulse
  V2 Double ->
  -- | point
  V2 Double ->
  Step s ()
bodyApplyImpulseAtWorldPoint (Body body (IOE lift)) impulse point = lift $ C.bodyApplyImpulseAtWorldPoint body (v2ToVect impulse) (v2ToVect point)

-- | Add the local impulse to body as if applied from the body local point.
bodyApplyImpulseAtLocalPoint ::
  Body s ->
  -- | impulse
  V2 Double ->
  -- | point
  V2 Double ->
  Step s ()
bodyApplyImpulseAtLocalPoint (Body body (IOE lift)) impulse point = lift $ C.bodyApplyImpulseAtLocalPoint body (v2ToVect impulse) (v2ToVect point)

instance HasField "position" (Body s) (Var s (V2 Double)) where
  getField = bodyPosition

instance HasField "centerOfGravity" (Body s) (Var s (V2 Double)) where
  getField = bodyCenterOfGravity

instance HasField "velocity" (Body s) (Var s (V2 Double)) where
  getField = bodyVelocity

instance HasField "force" (Body s) (Var s (V2 Double)) where
  getField = bodyForce

instance HasField "angle" (Body s) (Var s Double) where
  getField = bodyAngle

instance HasField "angularVelocity" (Body s) (Var s Double) where
  getField = bodyAngularVelocity

instance HasField "torque" (Body s) (Var s Double) where
  getField = bodyTorque

-- SHAPES
addCircleShape ::
  Body s ->
  -- | Radius
  Double ->
  -- | Offset from the body coordinates
  V2 Double ->
  Step s (Shape s)
addCircleShape (Body body ioe@(IOE lift)) radius centerOfGravity = lift $ do
  shape <- C.circleShapeNew body radius (v2ToVect centerOfGravity)
  space <- get $ C.bodySpace body
  C.spaceAddShape space shape
  pure (Shape shape ioe)

-- | Add a box shape centered around the body center
addBoxShape ::
  Body s ->
  -- | Box size
  V2 Double ->
  -- | Radius for smooth edges
  Double ->
  Step s (Shape s)
addBoxShape (Body body ioe@(IOE lift)) (V2 height width) boxRadius = lift $ do
  shape <- C.boxShapeNew body height width boxRadius
  space <- get $ C.bodySpace body
  C.spaceAddShape space shape
  pure (Shape shape ioe)

addSegmentShape ::
  Body s ->
  -- | Start point
  V2 Double ->
  -- | End point
  V2 Double ->
  -- | Thickness
  Double ->
  Step s (Shape s)
addSegmentShape (Body body ioe@(IOE lift)) start end thickness = lift $ do
  shape <- C.segmentShapeNew body (v2ToVect start) (v2ToVect end) thickness
  space <- get $ C.bodySpace body
  C.spaceAddShape space shape
  pure (Shape shape ioe)

addPolyShape ::
  Body s ->
  -- | Vertices local to body
  [V2 Double] ->
  Step s (Shape s)
addPolyShape (Body body ioe@(IOE lift)) vects = lift $ do
  shape <- C.polyShapeNew body (fmap v2ToVect vects) (C.Transform 0 0 0 0 0 0) 0
  space <- get $ C.bodySpace body
  C.spaceAddShape space shape
  pure (Shape shape ioe)

-- | Remove a shape from its 'Body'
removeShape :: Shape s -> Step s ()
removeShape (Shape shape (IOE lift)) = lift $ do
  space <- get (C.shapeSpace shape)
  C.spaceRemoveShape space shape
  C.shapeFree shape

-- | When you have a number of segment shapes that are all joined together, things can still collide with the “cracks” between the segments.
-- By setting the neighbor segment endpoints you can tell Chipmunk to avoid colliding with the inner parts of the crack.
setSegmentShapeNeighbors :: Shape s -> V2 Double -> V2 Double -> Step s ()
setSegmentShapeNeighbors (Shape shape (IOE lift)) v1 v2 = lift $ C.segmentShapeNeighbors shape C.$= (v2ToVect v1, v2ToVect v2)

-- | The surface velocity of the object. Useful for creating conveyor belts or players that move around.
-- This value is only used when calculating friction, not resolving the collision.
shapeSurfaceVelocity :: Shape s -> Var s (V2 Double)
shapeSurfaceVelocity (Shape shape ioe) = mapToVar ioe v2ToVect vectToV2 (C.shapeSurfaceVelocity shape)

shapeSensor :: Shape s -> Var s Bool
shapeSensor (Shape shape ioe) = toVar ioe (C.shapeSensor shape)

shapeElasticity :: Shape s -> Var s Double
shapeElasticity (Shape shape ioe) = toVar ioe (C.shapeElasticity shape)

shapeFriction :: Shape s -> Var s Double
shapeFriction (Shape shape ioe) = toVar ioe (C.shapeFriction shape)

shapeCollisionType :: Shape s -> Var s C.CollisionType
shapeCollisionType (Shape shape ioe) = toVar ioe (C.shapeCollisionType shape)

shapeMass :: Shape s -> Var s Double
shapeMass (Shape shape ioe) = toVar ioe (C.shapeMass shape)

shapeDensity :: Shape s -> Var s Double
shapeDensity (Shape shape ioe) = toVar ioe (C.shapeDensity shape)

shapeFilter :: Shape s -> Var s C.ShapeFilter
shapeFilter (Shape shape ioe) = toVar ioe (C.shapeFilter shape)

instance HasField "sensor" (Shape s) (Var s Bool) where
  getField = shapeSensor

instance HasField "elasticity" (Shape s) (Var s Double) where
  getField = shapeElasticity

instance HasField "friction" (Shape s) (Var s Double) where
  getField = shapeFriction

instance HasField "surfaceVelocity" (Shape s) (Var s (V2 Double)) where
  getField = shapeSurfaceVelocity

instance HasField "collisionType" (Shape s) (Var s C.CollisionType) where
  getField = shapeCollisionType

instance HasField "mass" (Shape s) (Var s Double) where
  getField = shapeMass

instance HasField "density" (Shape s) (Var s Double) where
  getField = shapeDensity

instance HasField "filter" (Shape s) (Var s C.ShapeFilter) where
  getField = shapeFilter

-- -- CONSTRAINTS

data Constraint s = Constraint
  { constraint :: C.Constraint,
    ioe :: IOE s
  }

class IsConstraint c where
  getConstraint :: c s -> Constraint s

instance IsConstraint Constraint where
  getConstraint = id

-- | The first body constraint is attached to
getConstraintBodyA :: (IsConstraint c) => c s -> Step s (Body s)
getConstraintBodyA c = lift $ flip Body ioe <$> get (C.constraintBodyA constraint)
  where
    Constraint constraint ioe@(IOE lift) = getConstraint c

-- | The second body constraint is attached to
getConstraintBodyB :: (IsConstraint c) => c s -> Step s (Body s)
getConstraintBodyB c = lift $ flip Body ioe <$> get (C.constraintBodyB constraint)
  where
    Constraint constraint ioe@(IOE lift) = getConstraint c

-- | The maximum force that the constraint can use to act on the two bodies. Defaults to INFINITY.
constraintMaxForce :: (IsConstraint c) => c s -> Var s Double
constraintMaxForce c = toVar ioe $ C.constraintMaxForce constraint
  where
    Constraint constraint ioe = getConstraint c

-- | The percentage of joint error that remains unfixed after a second. This works exactly the same as the collision bias property of a space, but applies to fixing error (stretching) of joints instead of overlapping collisions.
constraintErrorBias :: (IsConstraint c) => c s -> Var s Double
constraintErrorBias c = toVar ioe $ C.constraintErrorBias constraint
  where
    Constraint constraint ioe = getConstraint c

-- | Get the maximum speed at which the constraint can apply error correction. Defaults to INFINITY.
constraintMaxBias :: (IsConstraint c) => c s -> Var s Double
constraintMaxBias c = toVar ioe $ C.constraintMaxBias constraint
  where
    Constraint constraint ioe = getConstraint c

-- | Constraints can be used for filtering collisions too. When two bodies collide, Chipmunk ignores the collisions if this property is set to False on any constraint that connects the two bodies. Defaults to True.
--
-- This can be used to create a chain that self collides, but adjacent links in the chain do not collide.
constraintCollideBodies :: (IsConstraint c) => c s -> Var s Bool
constraintCollideBodies c = toVar ioe $ C.constraintCollideBodies constraint
  where
    Constraint constraint ioe = getConstraint c

-- | The most recent impulse that constraint applied. To convert this to a force, divide by the timestep passed to spaceStep. You can use this to implement breakable joints to check if the force they attempted to apply exceeded a certain threshold.
getConstraintImpulse :: (IsConstraint c) => c s -> Step s Double
getConstraintImpulse c = lift $ get $ C.constraintImpulse constraint
  where
    Constraint constraint (IOE lift) = getConstraint c

-- | Remove a constraint from its bodies
removeConstraint :: (IsConstraint c) => c s -> Step s ()
removeConstraint c = lift $ do
  space <- C.constraintSpace constraint
  C.spaceRemoveConstraint space constraint
  C.constraintFree constraint
  where
    Constraint constraint (IOE lift) = getConstraint c

-- | Connect two bodies via anchor points on those bodies. The distance between the two anchor points is measured when the joint is created.
-- If you want to set a specific distance, use the setter function to override it
newtype PinJoint s = PinJoint (Constraint s)

instance IsConstraint PinJoint where
  getConstraint (PinJoint c) = c

instance HasField "bodyA" (PinJoint s) (Step s (Body s)) where
  getField = getConstraintBodyA

instance HasField "bodyB" (PinJoint s) (Step s (Body s)) where
  getField = getConstraintBodyB

instance HasField "maxForce" (PinJoint s) (Var s Double) where
  getField = constraintMaxForce

instance HasField "errorBias" (PinJoint s) (Var s Double) where
  getField = constraintErrorBias

instance HasField "maxBias" (PinJoint s) (Var s Double) where
  getField = constraintMaxBias

instance HasField "collideBodies" (PinJoint s) (Var s Bool) where
  getField = constraintCollideBodies

instance HasField "impulse" (PinJoint s) (Step s Double) where
  getField = getConstraintImpulse

addPinJoint ::
  -- | body 1
  Body s ->
  -- | anchor 1
  V2 Double ->
  -- | body 2
  Body s ->
  -- | anchor 2
  V2 Double ->
  Step s (PinJoint s)
addPinJoint (Body body1 ioe@(IOE lift)) anchor1 (Body body2 _) anchor2 = lift $ do
  pinJoint <- C.pinJointNew body1 body2 (v2ToVect anchor1) (v2ToVect anchor2)
  space <- get (C.bodySpace body1)
  C.spaceAddConstraint space pinJoint
  pure $ PinJoint (Constraint pinJoint ioe)

pinJointAnchorA :: PinJoint s -> Var s (V2 Double)
pinJointAnchorA (PinJoint (Constraint constraint ioe)) = mapToVar ioe v2ToVect vectToV2 (C.pinJointAnchorA constraint)

pinJointAnchorB :: PinJoint s -> Var s (V2 Double)
pinJointAnchorB (PinJoint (Constraint constraint ioe)) = mapToVar ioe v2ToVect vectToV2 (C.pinJointAnchorB constraint)

pinJointDistance :: PinJoint s -> Var s Double
pinJointDistance (PinJoint (Constraint constraint ioe)) = toVar ioe $ C.pinJointDist constraint

instance HasField "anchorA" (PinJoint s) (Var s (V2 Double)) where
  getField = pinJointAnchorA

instance HasField "anchorB" (PinJoint s) (Var s (V2 Double)) where
  getField = pinJointAnchorB

instance HasField "distance" (PinJoint s) (Var s Double) where
  getField = pinJointDistance

-- | Connect two bodies via anchor points forcing distance to remain in range.
newtype SlideJoint s = SlideJoint (Constraint s)

instance IsConstraint SlideJoint where
  getConstraint (SlideJoint c) = c

instance HasField "bodyA" (SlideJoint s) (Step s (Body s)) where
  getField = getConstraintBodyA

instance HasField "bodyB" (SlideJoint s) (Step s (Body s)) where
  getField = getConstraintBodyB

instance HasField "maxForce" (SlideJoint s) (Var s Double) where
  getField = constraintMaxForce

instance HasField "errorBias" (SlideJoint s) (Var s Double) where
  getField = constraintErrorBias

instance HasField "maxBias" (SlideJoint s) (Var s Double) where
  getField = constraintMaxBias

instance HasField "collideBodies" (SlideJoint s) (Var s Bool) where
  getField = constraintCollideBodies

instance HasField "impulse" (SlideJoint s) (Step s Double) where
  getField = getConstraintImpulse

addSlideJoint ::
  -- | body 1
  (Body s) ->
  -- | anchor 1
  V2 Double ->
  -- | body 2
  (Body s) ->
  -- | anchor 2
  V2 Double ->
  -- | minimum distance
  Double ->
  -- | maximum distance
  Double ->
  Step s (SlideJoint s)
addSlideJoint (Body body1 ioe@(IOE lift)) anchor1 (Body body2 _) anchor2 minDistance maxDistance = lift $ do
  slideJoint <- C.slideJointNew body1 body2 (v2ToVect anchor1) (v2ToVect anchor2) minDistance maxDistance
  space <- get (C.bodySpace body1)
  C.spaceAddConstraint space slideJoint
  pure $ SlideJoint (Constraint slideJoint ioe)

slideJointAnchorA :: SlideJoint s -> Var s (V2 Double)
slideJointAnchorA (SlideJoint (Constraint constraint ioe)) = mapToVar ioe v2ToVect vectToV2 (C.slideJointAnchorA constraint)

slideJointAnchorB :: SlideJoint s -> Var s (V2 Double)
slideJointAnchorB (SlideJoint (Constraint constraint ioe)) = mapToVar ioe v2ToVect vectToV2 (C.slideJointAnchorB constraint)

slideJointMin :: SlideJoint s -> Var s Double
slideJointMin (SlideJoint (Constraint constraint ioe)) = toVar ioe $ C.slideJointMin constraint

slideJointMax :: SlideJoint s -> Var s Double
slideJointMax (SlideJoint (Constraint constraint ioe)) = toVar ioe $ C.slideJointMax constraint

instance HasField "anchorA" (SlideJoint s) (Var s (V2 Double)) where
  getField = slideJointAnchorA

instance HasField "anchorB" (SlideJoint s) (Var s (V2 Double)) where
  getField = slideJointAnchorB

instance HasField "min" (SlideJoint s) (Var s Double) where
  getField = slideJointMin

instance HasField "max" (SlideJoint s) (Var s Double) where
  getField = slideJointMax

newtype PivotJoint s = PivotJoint (Constraint s)

instance IsConstraint PivotJoint where
  getConstraint (PivotJoint c) = c

instance HasField "bodyA" (PivotJoint s) (Step s (Body s)) where
  getField = getConstraintBodyA

instance HasField "bodyB" (PivotJoint s) (Step s (Body s)) where
  getField = getConstraintBodyB

instance HasField "maxForce" (PivotJoint s) (Var s Double) where
  getField = constraintMaxForce

instance HasField "errorBias" (PivotJoint s) (Var s Double) where
  getField = constraintErrorBias

instance HasField "maxBias" (PivotJoint s) (Var s Double) where
  getField = constraintMaxBias

instance HasField "collideBodies" (PivotJoint s) (Var s Bool) where
  getField = constraintCollideBodies

instance HasField "impulse" (PivotJoint s) (Step s Double) where
  getField = getConstraintImpulse

addPivotJoint ::
  -- | body 1
  Body s ->
  -- | anchor 1
  V2 Double ->
  -- | body 2
  Body s ->
  -- | anchor 2
  V2 Double ->
  Step s (PivotJoint s)
addPivotJoint (Body body1 ioe@(IOE lift)) anchor1 (Body body2 _) anchor2 = lift $ do
  pivotJoint <- C.pivotJointNew2 body1 body2 (v2ToVect anchor1) (v2ToVect anchor2)
  space <- get (C.bodySpace body1)
  C.spaceAddConstraint space pivotJoint
  pure $ PivotJoint (Constraint pivotJoint ioe)

pivotJointAnchorA :: PivotJoint s -> Var s (V2 Double)
pivotJointAnchorA (PivotJoint (Constraint constraint ioe)) = mapToVar ioe v2ToVect vectToV2 (C.pivotJointAnchorA constraint)

pivotJointAnchorB :: PivotJoint s -> Var s (V2 Double)
pivotJointAnchorB (PivotJoint (Constraint constraint ioe)) = mapToVar ioe v2ToVect vectToV2 (C.pivotJointAnchorB constraint)

instance HasField "anchorA" (PivotJoint s) (Var s (V2 Double)) where
  getField = pivotJointAnchorA

instance HasField "anchorB" (PivotJoint s) (Var s (V2 Double)) where
  getField = pivotJointAnchorB

-- | Pivot is attached to groove on first body and to anchor on the second. All coordinates are body local.
newtype GrooveJoint s = GrooveJoint (Constraint s)

instance IsConstraint GrooveJoint where
  getConstraint (GrooveJoint c) = c

instance HasField "bodyA" (GrooveJoint s) (Step s (Body s)) where
  getField = getConstraintBodyA

instance HasField "bodyB" (GrooveJoint s) (Step s (Body s)) where
  getField = getConstraintBodyB

instance HasField "maxForce" (GrooveJoint s) (Var s Double) where
  getField = constraintMaxForce

instance HasField "errorBias" (GrooveJoint s) (Var s Double) where
  getField = constraintErrorBias

instance HasField "maxBias" (GrooveJoint s) (Var s Double) where
  getField = constraintMaxBias

instance HasField "collideBodies" (GrooveJoint s) (Var s Bool) where
  getField = constraintCollideBodies

instance HasField "impulse" (GrooveJoint s) (Step s Double) where
  getField = getConstraintImpulse

addGrooveJoint ::
  -- | body 1
  Body s ->
  -- | endpoint 1 on body 1
  V2 Double ->
  -- | endpoint 2 on body 1
  V2 Double ->
  -- | body 2
  Body s ->
  -- | anchor on body 2
  V2 Double ->
  Step s (GrooveJoint s)
addGrooveJoint (Body body1 ioe@(IOE lift)) endpoint1 endpoint2 (Body body2 _) anchor = lift $ do
  grooveJoint <- C.grooveJointNew body1 body2 (v2ToVect endpoint1) (v2ToVect endpoint2) (v2ToVect anchor)
  space <- get (C.bodySpace body1)
  C.spaceAddConstraint space grooveJoint
  pure $ GrooveJoint (Constraint grooveJoint ioe)

grooveJointGrooveA :: GrooveJoint s -> Var s (V2 Double)
grooveJointGrooveA (GrooveJoint (Constraint constraint ioe)) = mapToVar ioe v2ToVect vectToV2 (C.grooveJointGrooveA constraint)

grooveJointGrooveB :: GrooveJoint s -> Var s (V2 Double)
grooveJointGrooveB (GrooveJoint (Constraint constraint ioe)) = mapToVar ioe v2ToVect vectToV2 (C.grooveJointGrooveB constraint)

grooveJointAnchorB :: GrooveJoint s -> Var s (V2 Double)
grooveJointAnchorB (GrooveJoint (Constraint constraint ioe)) = mapToVar ioe v2ToVect vectToV2 (C.grooveJointAnchorB constraint)

instance HasField "grooveA" (GrooveJoint s) (Var s (V2 Double)) where
  getField = grooveJointGrooveA

instance HasField "grooveB" (GrooveJoint s) (Var s (V2 Double)) where
  getField = grooveJointGrooveB

instance HasField "anchorB" (GrooveJoint s) (Var s (V2 Double)) where
  getField = grooveJointAnchorB

newtype DampedSpring s = DampedSpring (Constraint s)

instance IsConstraint DampedSpring where
  getConstraint (DampedSpring c) = c

instance HasField "bodyA" (DampedSpring s) (Step s (Body s)) where
  getField = getConstraintBodyA

instance HasField "bodyB" (DampedSpring s) (Step s (Body s)) where
  getField = getConstraintBodyB

instance HasField "maxForce" (DampedSpring s) (Var s Double) where
  getField = constraintMaxForce

instance HasField "errorBias" (DampedSpring s) (Var s Double) where
  getField = constraintErrorBias

instance HasField "maxBias" (DampedSpring s) (Var s Double) where
  getField = constraintMaxBias

instance HasField "collideBodies" (DampedSpring s) (Var s Bool) where
  getField = constraintCollideBodies

instance HasField "impulse" (DampedSpring s) (Step s Double) where
  getField = getConstraintImpulse

addDampedSpring ::
  -- | body 1
  Body s ->
  -- | anchor 1
  V2 Double ->
  -- | body 2
  Body s ->
  -- | anchor 2
  V2 Double ->
  -- | distance of spring
  Double ->
  -- | spring constant
  Double ->
  -- | spring damping
  Double ->
  Step s (DampedSpring s)
addDampedSpring (Body body1 ioe@(IOE lift)) anchor1 (Body body2 _) anchor2 distance springConstant damping = lift $ do
  dampedSpring <- C.dampedSpringNew body1 body2 (v2ToVect anchor1) (v2ToVect anchor2) distance springConstant damping
  space <- get (C.bodySpace body1)
  C.spaceAddConstraint space dampedSpring
  pure $ DampedSpring (Constraint dampedSpring ioe)

dampedSpringAnchorA :: DampedSpring s -> Var s (V2 Double)
dampedSpringAnchorA (DampedSpring (Constraint c ioe)) = mapToVar ioe v2ToVect vectToV2 (C.dampedSpringAnchorA c)

dampedSpringAnchorB :: DampedSpring s -> Var s (V2 Double)
dampedSpringAnchorB (DampedSpring (Constraint c ioe)) = mapToVar ioe v2ToVect vectToV2 (C.dampedSpringAnchorB c)

dampedSpringDistance :: DampedSpring s -> Var s Double
dampedSpringDistance (DampedSpring (Constraint c ioe)) = toVar ioe $ C.dampedSpringRestLength c

dampedSpringStiffness :: DampedSpring s -> Var s Double
dampedSpringStiffness (DampedSpring (Constraint c ioe)) = toVar ioe $ C.dampedSpringStiffness c

dampedSpringDamping :: DampedSpring s -> Var s Double
dampedSpringDamping (DampedSpring (Constraint c ioe)) = toVar ioe $ C.dampedSpringDamping c

instance HasField "anchorA" (DampedSpring s) (Var s (V2 Double)) where
  getField = dampedSpringAnchorA

instance HasField "anchorB" (DampedSpring s) (Var s (V2 Double)) where
  getField = dampedSpringAnchorB

instance HasField "distance" (DampedSpring s) (Var s Double) where
  getField = dampedSpringDistance

instance HasField "stiffness" (DampedSpring s) (Var s Double) where
  getField = dampedSpringStiffness

instance HasField "damping" (DampedSpring s) (Var s Double) where
  getField = dampedSpringDamping

newtype DampedRotarySpring s = DampedRotarySpring (Constraint s)

instance IsConstraint DampedRotarySpring where
  getConstraint (DampedRotarySpring c) = c

instance HasField "bodyA" (DampedRotarySpring s) (Step s (Body s)) where
  getField = getConstraintBodyA

instance HasField "bodyB" (DampedRotarySpring s) (Step s (Body s)) where
  getField = getConstraintBodyB

instance HasField "maxForce" (DampedRotarySpring s) (Var s Double) where
  getField = constraintMaxForce

instance HasField "errorBias" (DampedRotarySpring s) (Var s Double) where
  getField = constraintErrorBias

instance HasField "maxBias" (DampedRotarySpring s) (Var s Double) where
  getField = constraintMaxBias

instance HasField "collideBodies" (DampedRotarySpring s) (Var s Bool) where
  getField = constraintCollideBodies

instance HasField "impulse" (DampedRotarySpring s) (Step s Double) where
  getField = getConstraintImpulse

addDampedRotarySpring ::
  -- | body 1
  Body s ->
  -- | body 2
  Body s ->
  -- | angle of the spring
  Double ->
  -- | spring constant
  Double ->
  -- | damping
  Double ->
  Step s (DampedRotarySpring s)
addDampedRotarySpring (Body body1 ioe@(IOE lift)) (Body body2 _) angle springConstant damping = lift $ do
  dampedRotarySpring <- C.dampedRotarySpringNew body1 body2 angle springConstant damping
  space <- get (C.bodySpace body1)
  C.spaceAddConstraint space dampedRotarySpring
  pure $ DampedRotarySpring (Constraint dampedRotarySpring ioe)

dampedRotarySpringAngle :: DampedRotarySpring s -> Var s Double
dampedRotarySpringAngle (DampedRotarySpring (Constraint c ioe)) = toVar ioe $ C.dampedRotarySpringRestAngle c

dampedRotarySpringStiffness :: DampedRotarySpring s -> Var s Double
dampedRotarySpringStiffness (DampedRotarySpring (Constraint c ioe)) = toVar ioe $ C.dampedRotarySpringStiffness c

dampedRotarySpringDamping :: DampedRotarySpring s -> Var s Double
dampedRotarySpringDamping (DampedRotarySpring (Constraint c ioe)) = toVar ioe $ C.dampedRotarySpringDamping c

instance HasField "angle" (DampedRotarySpring s) (Var s Double) where
  getField = dampedRotarySpringAngle

instance HasField "stiffness" (DampedRotarySpring s) (Var s Double) where
  getField = dampedRotarySpringStiffness

instance HasField "damping" (DampedRotarySpring s) (Var s Double) where
  getField = dampedRotarySpringDamping

-- -- | Constrains the relative rotations of two bodies. It is implemented so that it’s possible to for the range to be greater than a full revolution.
-- newtype RotaryLimitJoint = RotaryLimitJoint C.Constraint

-- instance IsConstraint RotaryLimitJoint where
--   getConstraint (RotaryLimitJoint c) = c

-- instance HasField "bodyA" RotaryLimitJoint (GettableStateVar Body) where
--   getField = constraintBodyA

-- instance HasField "bodyB" RotaryLimitJoint (GettableStateVar Body) where
--   getField = constraintBodyB

-- instance HasField "maxForce" RotaryLimitJoint (StateVar Double) where
--   getField = constraintMaxForce

-- instance HasField "errorBias" RotaryLimitJoint (StateVar Double) where
--   getField = constraintErrorBias

-- instance HasField "maxBias" RotaryLimitJoint (StateVar Double) where
--   getField = constraintMaxBias

-- instance HasField "collideBodies" RotaryLimitJoint (StateVar Bool) where
--   getField = constraintCollideBodies

-- instance HasField "impulse" RotaryLimitJoint (GettableStateVar Double) where
--   getField = constraintImpulse

-- addRotaryLimitJoint ::
--   -- | body 1
--   Body ->
--   -- body 2
--   Body ->
--   -- | minimum angle
--   Double ->
--   -- | maximum angle
--   Double ->
--   IO RotaryLimitJoint
-- addRotaryLimitJoint body1 body2 minAngle maxAngle = do
--   rotaryLimitJoint <- C.rotaryLimitJointNew body1 body2 minAngle maxAngle
--   space <- get (C.bodySpace body1)
--   C.spaceAddConstraint space rotaryLimitJoint
--   pure $ RotaryLimitJoint rotaryLimitJoint

-- rotaryLimitJointMin :: RotaryLimitJoint -> StateVar Double
-- rotaryLimitJointMin (RotaryLimitJoint c) = C.rotaryLimitJointMin c

-- rotaryLimitJointMax :: RotaryLimitJoint -> StateVar Double
-- rotaryLimitJointMax (RotaryLimitJoint c) = C.rotaryLimitJointMax c

-- instance HasField "min" RotaryLimitJoint (StateVar Double) where
--   getField = rotaryLimitJointMin

-- instance HasField "max" RotaryLimitJoint (StateVar Double) where
--   getField = rotaryLimitJointMax

-- -- | Works like a socket wrench.
-- newtype RatchetJoint = RatchetJoint C.Constraint

-- instance IsConstraint RatchetJoint where
--   getConstraint (RatchetJoint c) = c

-- instance HasField "bodyA" RatchetJoint (GettableStateVar Body) where
--   getField = constraintBodyA

-- instance HasField "bodyB" RatchetJoint (GettableStateVar Body) where
--   getField = constraintBodyB

-- instance HasField "maxForce" RatchetJoint (StateVar Double) where
--   getField = constraintMaxForce

-- instance HasField "errorBias" RatchetJoint (StateVar Double) where
--   getField = constraintErrorBias

-- instance HasField "maxBias" RatchetJoint (StateVar Double) where
--   getField = constraintMaxBias

-- instance HasField "collideBodies" RatchetJoint (StateVar Bool) where
--   getField = constraintCollideBodies

-- instance HasField "impulse" RatchetJoint (GettableStateVar Double) where
--   getField = constraintImpulse

-- addRatchetJoint ::
--   -- | body 1
--   Body ->
--   -- | body 2
--   Body ->
--   -- | The initial offset to use when deciding where the ratchet angles are
--   Double ->
--   -- | The distance between “clicks”
--   Double ->
--   IO RatchetJoint
-- addRatchetJoint body1 body2 minAngle maxAngle = do
--   ratchetJoint <- C.ratchetJointNew body1 body2 minAngle maxAngle
--   space <- get (C.bodySpace body1)
--   C.spaceAddConstraint space ratchetJoint
--   pure $ RatchetJoint ratchetJoint

-- ratchetJointAngle :: RatchetJoint -> StateVar Double
-- ratchetJointAngle (RatchetJoint c) = C.ratchetJointAngle c

-- ratchetJointPhase :: RatchetJoint -> StateVar Double
-- ratchetJointPhase (RatchetJoint c) = C.ratchetJointPhase c

-- ratchetJointRatchet :: RatchetJoint -> StateVar Double
-- ratchetJointRatchet (RatchetJoint c) = C.ratchetJointRatchet c

-- instance HasField "angle" RatchetJoint (StateVar Double) where
--   getField = ratchetJointAngle

-- instance HasField "phase" RatchetJoint (StateVar Double) where
--   getField = ratchetJointPhase

-- instance HasField "ratchet" RatchetJoint (StateVar Double) where
--   getField = ratchetJointRatchet

-- -- | Keeps the angular velocity ratio of a pair of bodies constant.
-- newtype GearJoint = GearJoint C.Constraint

-- instance IsConstraint GearJoint where
--   getConstraint (GearJoint c) = c

-- instance HasField "bodyA" GearJoint (GettableStateVar Body) where
--   getField = constraintBodyA

-- instance HasField "bodyB" GearJoint (GettableStateVar Body) where
--   getField = constraintBodyB

-- instance HasField "maxForce" GearJoint (StateVar Double) where
--   getField = constraintMaxForce

-- instance HasField "errorBias" GearJoint (StateVar Double) where
--   getField = constraintErrorBias

-- instance HasField "maxBias" GearJoint (StateVar Double) where
--   getField = constraintMaxBias

-- instance HasField "collideBodies" GearJoint (StateVar Bool) where
--   getField = constraintCollideBodies

-- instance HasField "impulse" GearJoint (GettableStateVar Double) where
--   getField = constraintImpulse

-- addGearJoint ::
--   -- | body 1
--   Body ->
--   -- | body 2
--   Body ->
--   -- | initial angular offset
--   Double ->
--   -- | ratio
--   Double ->
--   IO GearJoint
-- addGearJoint body1 body2 offset ratio = do
--   gearJoint <- C.gearJointNew body1 body2 offset ratio
--   space <- get (C.bodySpace body1)
--   C.spaceAddConstraint space gearJoint
--   pure $ GearJoint gearJoint

-- gearJointPhase :: GearJoint -> StateVar Double
-- gearJointPhase (GearJoint c) = C.gearJointPhase c

-- gearJointRatio :: GearJoint -> StateVar Double
-- gearJointRatio (GearJoint c) = C.gearJointRatio c

-- instance HasField "phase" GearJoint (StateVar Double) where
--   getField = gearJointPhase

-- instance HasField "ratio" GearJoint (StateVar Double) where
--   getField = gearJointRatio

-- -- | Keeps the relative angular velocity of a pair of bodies constant. You will usually want to set an force (torque) maximum for motors as otherwise they will be able to apply a nearly infinite torque to keep the bodies moving.
-- newtype SimpleMotor = SimpleMotor C.Constraint

-- instance IsConstraint SimpleMotor where
--   getConstraint (SimpleMotor c) = c

-- instance HasField "bodyA" SimpleMotor (GettableStateVar Body) where
--   getField = constraintBodyA

-- instance HasField "bodyB" SimpleMotor (GettableStateVar Body) where
--   getField = constraintBodyB

-- instance HasField "maxForce" SimpleMotor (StateVar Double) where
--   getField = constraintMaxForce

-- instance HasField "errorBias" SimpleMotor (StateVar Double) where
--   getField = constraintErrorBias

-- instance HasField "maxBias" SimpleMotor (StateVar Double) where
--   getField = constraintMaxBias

-- instance HasField "collideBodies" SimpleMotor (StateVar Bool) where
--   getField = constraintCollideBodies

-- instance HasField "impulse" SimpleMotor (GettableStateVar Double) where
--   getField = constraintImpulse

-- addSimpleMotor ::
--   -- | body 1
--   Body ->
--   -- | body 2
--   Body ->
--   -- | relative angular velocity
--   Double ->
--   IO SimpleMotor
-- addSimpleMotor body1 body2 angularVelocity = do
--   simpleMotor <- C.simpleMotorNew body1 body2 angularVelocity
--   space <- get (C.bodySpace body1)
--   C.spaceAddConstraint space simpleMotor
--   pure $ SimpleMotor simpleMotor

-- simpleMotorRate :: SimpleMotor -> StateVar Double
-- simpleMotorRate (SimpleMotor c) = C.simpleMotorRate c

-- instance HasField "rate" SimpleMotor (StateVar Double) where
--   getField = simpleMotorRate

-- Collisions

-- | A 'Collision' happens when two shapes touch
newtype Collision = Collision C.Arbiter

-- | For a `PreCollision`, friction, restitution and surface velocity may be changed
newtype PreCollision = PreCollision C.Arbiter

newtype CollisionStatus = CollisionStatus Bool deriving (Eq, Ord, Show)

processCollision :: CollisionStatus
processCollision = CollisionStatus True

ignoreCollision :: CollisionStatus
ignoreCollision = CollisionStatus False

data CollisionPhase = Begin | PreSolve | PostSolve | Separate

newtype CollisionM (phase :: CollisionPhase) s a = CollisionM (IORef (PostStepCallbacks s) -> Unique -> IOE s -> C.Arbiter -> IO a)
  deriving (Functor, Applicative, Monad) via ReaderT (IORef (PostStepCallbacks s)) (ReaderT Unique ((ReaderT (IOE s) (ReaderT C.Arbiter IO))))

runCollisionM :: IORef (PostStepCallbacks s) -> Unique -> IOE s -> C.Arbiter -> CollisionM phase s a -> IO a
runCollisionM postStepCallbacks unique ioe arbiter (CollisionM f) = f postStepCallbacks unique ioe arbiter

makeCollisionM :: (C.Arbiter -> IO a) -> CollisionM phase s a
makeCollisionM f = CollisionM $ \_ _ _ a -> f a

handleCollisionBegin :: CollisionM Begin s Bool -> CollisionHandler s
handleCollisionBegin = undefined

addCallback :: Step s () -> CollisionM phase s ()
addCallback step = CollisionM $ \postStepCallbacks _ _ _ -> do
  modifyIORef' postStepCallbacks $ \psc -> psc {unkeyed = psc.unkeyed >> step}

addSingleCallback :: Step s () -> CollisionM phase s ()
addSingleCallback step = CollisionM $ \postStepCallbacks unique _ arbiter -> do
  (shapeA, shapeB) <- get (C.arbiterShapes arbiter)
  let (shapeA', shapeB') = if shapeA < shapeB then (shapeA, shapeB) else (shapeB, shapeA)
  modifyIORef' postStepCallbacks $ \psc -> psc {keyed = M.insert (unique, shapeA', shapeB') step psc.keyed}

getCollisionRestitution :: CollisionM phase s Double
getCollisionRestitution = makeCollisionM $ \arbiter -> get (C.arbiterRestitution arbiter)

setCollisionRestitution :: Double -> CollisionM PreSolve s ()
setCollisionRestitution v = makeCollisionM $ \arbiter -> C.arbiterRestitution arbiter $= v

getCollisionFriction :: CollisionM phase s Double
getCollisionFriction = makeCollisionM $ \arbiter -> get (C.arbiterFriction arbiter)

setCollisionFriction :: Double -> CollisionM PreSolve s ()
setCollisionFriction v = makeCollisionM $ \arbiter -> C.arbiterFriction arbiter $= v

getCollisionSurfaceVelocity :: CollisionM phase s (V2 Double)
getCollisionSurfaceVelocity = makeCollisionM $ \arbiter -> fmap vectToV2 $ get (C.arbiterSurfaceVelocity arbiter)

setCollisionSurfaceVelocity :: V2 Double -> CollisionM PreSolve s ()
setCollisionSurfaceVelocity v = makeCollisionM $ \arbiter -> C.arbiterSurfaceVelocity arbiter $= v2ToVect v

getCollisionCount :: CollisionM phase s Int
getCollisionCount = makeCollisionM C.arbiterCount

getCollisionNormal :: CollisionM phase s (V2 Double)
getCollisionNormal = makeCollisionM $ \arbiter -> vectToV2 <$> C.arbiterNormal arbiter

getCollisionPointsA :: CollisionM phase s [V2 Double]
getCollisionPointsA = makeCollisionM $ \arbiter -> do
  n <- C.arbiterCount arbiter
  forM [0 .. n - 1] $ \i ->
    vectToV2 <$> C.arbiterPointA arbiter i

getCollisionPointsB :: CollisionM phase s [V2 Double]
getCollisionPointsB = makeCollisionM $ \arbiter -> do
  n <- C.arbiterCount arbiter
  forM [0 .. n - 1] $ \i ->
    vectToV2 <$> C.arbiterPointB arbiter i

getCollisionPointA :: Int -> CollisionM phase s (V2 Double)
getCollisionPointA i = makeCollisionM $ \arbiter -> vectToV2 <$> C.arbiterPointA arbiter i

getCollisionPointB :: Int -> CollisionM phase s (V2 Double)
getCollisionPointB i = makeCollisionM $ \arbiter -> vectToV2 <$> C.arbiterPointB arbiter i

getCollisionDepth :: Int -> CollisionM phase s Double
getCollisionDepth i = makeCollisionM $ \arbiter -> C.arbiterDepth arbiter i

isFirstContact :: CollisionM phase s Bool
isFirstContact = makeCollisionM $ \arbiter -> C.arbiterIsFirstContact arbiter

isRemoval :: CollisionM phase s Bool
isRemoval = makeCollisionM $ \arbiter -> C.arbiterIsRemoval arbiter

getCollisionShapes :: CollisionM phase s (Shape s, Shape s)
getCollisionShapes = CollisionM $ \_ _ ioe arbiter -> do
  (shapeA, shapeB) <- C.arbiterShapes arbiter
  pure $ (Shape shapeA ioe, Shape shapeB ioe)

getCollisionBodies :: CollisionM phase s (Body s, Body s)
getCollisionBodies = CollisionM $ \_ _ ioe arbiter -> do
  (bodyA, bodyB) <- C.arbiterBodies arbiter
  pure $ (Body bodyA ioe, Body bodyB ioe)

class IsCollision collision where
  getArbiter :: collision -> C.Arbiter

instance IsCollision Collision where
  getArbiter (Collision collision) = collision

instance IsCollision PreCollision where
  getArbiter (PreCollision collision) = collision

-- -- | Schedule an action to be done after the physics update is done.
-- -- This is useful since you may not add to/remove from the space during a collision callback
-- --
-- -- `schedulePostStepWork` will only schedule one action for each shape pair and returns `False` if this collision already was given to `schedulePostStepWork`.
-- --  Use `alwaysSchedulePostStepWork` if you do not want this.
-- schedulePostStepWork :: (IsCollision collision) => CollisionSpace -> collision -> (Space -> IO ()) -> IO Bool
-- schedulePostStepWork (CollisionSpace space) collision action = do
--   let arbiter = getArbiter collision
--   (shape1, shape2) <- C.arbiterShapes arbiter
--   let key = hash shape1 * hash shape2
--   addKeyedCallback space key (action space)

-- -- | Schedule an action to be done after the physics update is done.
-- -- This is useful since you may not add to/remove from the space during a collision callback
-- --
-- -- `alwaysSchedulePostStepWork ` might schedule an action twice, once for collision (shapeA, shapeB) and once for collision (shapeB, shapeA).
-- -- Use `schedulePostStepWork` if you want to avoid this.
-- alwaysSchedulePostStepWork :: CollisionSpace -> (Space -> IO ()) -> IO ()
-- alwaysSchedulePostStepWork (CollisionSpace space) action = do
--   addCallback space (action space)

-- | Keeps track of the changes you want to apply to a collision handler
-- data ModifyCollisionHandler a = ModifyCollisionHandler
--   { collisionTypes :: !(C.CollisionType -> C.CollisionType -> (C.CollisionType, C.CollisionType)),
--     begin :: !(Maybe (Collision -> CollisionSpace -> IO (Bool, Maybe a))),
--     preSolve :: !(Maybe (ModifiableCollision -> CollisionSpace -> IO (Bool, Maybe a))),
--     postSolve :: !(Maybe (Collision -> CollisionSpace -> IO (Maybe a))),
--     separate :: !(Maybe (Collision -> CollisionSpace -> IO (Maybe a)))
--   }
data CollisionHandler s = CollisionHandler
  { begin :: [CollisionM Begin s CollisionStatus],
    preSolve :: [CollisionM PreSolve s CollisionStatus],
    postSolve :: [CollisionM PostSolve s ()],
    separate :: [CollisionM Separate s ()]
  }

instance Semigroup (CollisionHandler s) where
  (CollisionHandler b1 pre1 post1 s1) <> (CollisionHandler b2 pre2 post2 s2) =
    CollisionHandler
      (b1 ++ b2)
      (pre1 ++ pre2)
      (post1 ++ post2)
      (s1 ++ s2)
    where
      merge f x y = case x of
        Nothing -> y
        Just x' -> case y of
          Nothing -> Just x'
          Just y' -> Just (\a -> f (x' a) (y' a))

instance Monoid (CollisionHandler s) where
  mempty = CollisionHandler [] [] [] []

data CollisionHandlerScope
  = DefaultScope
  | WildcardScope C.CollisionType
  | PairScope C.CollisionType C.CollisionType
  deriving (Show, Eq, Ord)

newtype GlobalCollisionHandler s = GlobalCollisionHandler (M.Map CollisionHandlerScope (CollisionHandler s))

instance Semigroup (GlobalCollisionHandler s) where
  (GlobalCollisionHandler handlers1) <> (GlobalCollisionHandler handlers2) =
    GlobalCollisionHandler $ M.unionWith (<>) handlers1 handlers2

instance Monoid (GlobalCollisionHandler s) where
  mempty = GlobalCollisionHandler M.empty

registerGlobalCollisionHandlers :: (IOE :> es) => IORef (PostStepCallbacks s) -> C.Space -> GlobalCollisionHandler s -> Setup es s ()
registerGlobalCollisionHandlers postStepCallbacks space (GlobalCollisionHandler handlers) = do
  IOE lift <- getHandle
  forM_ (M.toList handlers) $ \(scope, handler) -> do
    case scope of
      DefaultScope -> do
        defaultHandler <- prestep $ lift $ C.spaceAddDefaultCollisionHandler space
        updateHandler postStepCallbacks defaultHandler handler
      WildcardScope ct -> do
        wildcardHandler <- prestep $ lift $ C.spaceAddWildcardHandler space ct
        updateHandler postStepCallbacks wildcardHandler handler
      PairScope ct1 ct2 -> do
        collisionHandler <- prestep $ lift $ C.spaceAddCollisionHandler space ct1 ct2
        updateHandler postStepCallbacks collisionHandler handler

updateHandler :: (IOE :> es) => IORef (PostStepCallbacks s) -> C.CollisionHandlerPtr -> CollisionHandler s -> Setup es s ()
updateHandler postStepCallbacks ptr (CollisionHandler begin pre post sep) = do
  ioe@(IOE lift) <- getHandle

  begin' <- collectHandlers ioe (&&) (coerce begin)
  pre' <- collectHandlers ioe (&&) (coerce pre)
  post' <- collectHandlers ioe (<>) post
  sep' <- collectHandlers ioe (<>) sep

  beginCB <- prestep $ lift $ traverse (\f -> C.mkCallbackB (\arbiter _ _ -> f arbiter)) begin'
  preSolveCB <- prestep $ lift $ traverse (\f -> C.mkCallbackB (\arbiter _ _ -> f arbiter)) pre'
  postSolveCB <- prestep $ lift $ traverse (\f -> C.mkCallback (\arbiter _ _ -> f arbiter)) post'
  sepCB <- prestep $ lift $ traverse (\f -> C.mkCallback (\arbiter _ _ -> f arbiter)) sep'

  finalize $
    traverse_ freeHaskellFunPtr $
      catMaybes [castFunPtr <$> beginCB, castFunPtr <$> preSolveCB, postSolveCB, sepCB]

  prestep $
    lift $
      C.modifyCollisionHandler ptr $
        maybe pure (\f ch -> C.mkCallbackB (\arbiter _ _ -> f arbiter) <&> \func -> ch {C.chBeginFunc = func}) begin'
          >=> maybe pure (\f ch -> C.mkCallbackB (\arbiter _ _ -> f arbiter) <&> \func -> ch {C.chPreSolveFunc = func}) pre'
          >=> maybe pure (\f ch -> C.mkCallback (\arbiter _ _ -> f arbiter) <&> \func -> ch {C.chPostSolveFunc = func}) post'
          >=> maybe pure (\f ch -> C.mkCallback (\arbiter _ _ -> f arbiter) <&> \func -> ch {C.chSeparateFunc = func}) sep'
  where
    collectHandlers ioe@(IOE lift) m handler =
      fmap (merge m) $ forM handler $ \(CollisionM f) -> do
        unique <- prestep $ lift $ newUnique
        pure $ f postStepCallbacks unique ioe
    merge :: (a -> a -> a) -> [C.Arbiter -> IO a] -> Maybe (C.Arbiter -> IO a)
    merge _ [] = Nothing
    merge f (x : xs) =
      case merge f xs of
        Nothing -> Just x
        Just xs' -> Just $ \a -> f <$> x a <*> xs' a

beginCollision :: CollisionM Begin s CollisionStatus -> CollisionHandler s
beginCollision c = mempty {begin = [c]}

preSolveCollision :: CollisionM PreSolve s CollisionStatus -> CollisionHandler s
preSolveCollision c = mempty {preSolve = [c]}

postSolveCollision :: CollisionM PostSolve s () -> CollisionHandler s
postSolveCollision c = mempty {postSolve = [c]}

separateCollision :: CollisionM Separate s () -> CollisionHandler s
separateCollision c = mempty {separate = [c]}

handleCollisionGlobally :: CollisionHandlerScope -> CollisionHandler s -> GlobalCollisionHandler s
handleCollisionGlobally scope handler = GlobalCollisionHandler (M.singleton scope handler)

-- -- | Change the behavior of a collision handler between two collision types.
-- --
-- -- Also returns an `Event` which triggers whenever the handlers in `ModifyCollisionHandler` trigger one.
-- modifyCollisionHandler :: Space -> C.CollisionType -> C.CollisionType -> ModifyCollisionHandler a -> IO (Event a)
-- modifyCollisionHandler space typeA typeB mch = do
--   handlerPtr <- C.spaceAddCollisionHandler space typeA typeB
--   modifyCollisionHandlerPtr handlerPtr mch

-- -- | Change the behavior of a wildcard collision handler
-- --
-- -- Also returns an `Event` which triggers whenever the handlers in `ModifyCollisionHandler` trigger one.
-- modifyWildcardCollisionHandler :: Space -> C.CollisionType -> ModifyCollisionHandler a -> IO (Event a)
-- modifyWildcardCollisionHandler space typeA mch = do
--   handlerPtr <- C.spaceAddWildcardHandler space typeA
--   modifyCollisionHandlerPtr handlerPtr mch

-- -- | Change the behavior of the default collision handler
-- --
-- -- Also returns an `Event` which triggers whenever the handlers in `ModifyCollisionHandler` trigger one.
-- modifyDefaultCollisionHandler :: Space -> ModifyCollisionHandler a -> IO (Event a)
-- modifyDefaultCollisionHandler space mch = do
--   handlerPtr <- C.spaceAddDefaultCollisionHandler space
--   modifyCollisionHandlerPtr handlerPtr mch

-- -- | Use the wildcard begin wildcard handler for shape of body A
-- handleWildcardBeginA :: Collision -> CollisionSpace -> IO Bool
-- handleWildcardBeginA (Collision arbiter) (CollisionSpace space) = C.arbiterCallWildcardBeginA arbiter space

-- -- | Use the wildcard begin wildcard handler for shape of body B
-- handleWildcardBeginB :: Collision -> CollisionSpace -> IO Bool
-- handleWildcardBeginB (Collision arbiter) (CollisionSpace space) = C.arbiterCallWildcardBeginB arbiter space

-- -- | Use the wildcard presolve handler for shape of body A
-- handleWildcardPreSolveA :: ModifiableCollision -> CollisionSpace -> IO Bool
-- handleWildcardPreSolveA (ModifiableCollision arbiter) (CollisionSpace space) = C.arbiterCallWildcardPreSolveA arbiter space

-- -- | Use the wildcard presolve handler for shape of body B
-- handleWildcardPreSolveB :: ModifiableCollision -> CollisionSpace -> IO Bool
-- handleWildcardPreSolveB (ModifiableCollision arbiter) (CollisionSpace space) = C.arbiterCallWildcardPreSolveB arbiter space

-- -- | Use the wildcard postsolve handler for shape of body A
-- handleWildcardPostSolveA :: Collision -> CollisionSpace -> IO ()
-- handleWildcardPostSolveA (Collision arbiter) (CollisionSpace space) = C.arbiterCallWildcardPostSolveA arbiter space

-- -- | Use the wildcard postsolve handler for shape of body B
-- handleWildcardPostSolveB :: Collision -> CollisionSpace -> IO ()
-- handleWildcardPostSolveB (Collision arbiter) (CollisionSpace space) = C.arbiterCallWildcardPostSolveB arbiter space

-- -- | Use the wildcard separate handler for shape of body A
-- handleWildcardSeparateA :: Collision -> CollisionSpace -> IO ()
-- handleWildcardSeparateA (Collision arbiter) (CollisionSpace space) = C.arbiterCallWildcardSeparateA arbiter space

-- -- | Use the wildcard separate handler for shape of body B
-- handleWildcardSeparateB :: Collision -> CollisionSpace -> IO ()
-- handleWildcardSeparateB (Collision arbiter) (CollisionSpace space) = C.arbiterCallWildcardSeparateB arbiter space

-- modifyCollisionHandlerPtr :: C.CollisionHandlerPtr -> ModifyCollisionHandler a -> IO (Event a)
-- modifyCollisionHandlerPtr handlerPtr (ModifyCollisionHandler collisionTypes begin preSolve postSolve separate) = do
--   eventTriggers <- newIORef M.empty

--   let updateCollisionTypes ch =
--         let (newCollisionTypeA, newCollisionTypeB) = collisionTypes (C.chTypeA ch) (C.chTypeB ch)
--          in ch {C.chTypeA = newCollisionTypeA, C.chTypeB = newCollisionTypeB}
--   updateBegin <- makeCallback eventTriggers begin Collision id C.mkCallbackB (\cb h -> h {C.chBeginFunc = cb})
--   updatePreSolve <- makeCallback eventTriggers preSolve ModifiableCollision id C.mkCallbackB (\cb h -> h {C.chPreSolveFunc = cb})
--   updatePostSolve <- makeCallback eventTriggers postSolve Collision ((),) C.mkCallback (\cb h -> h {C.chPostSolveFunc = cb})
--   updateSeparate <- makeCallback eventTriggers separate Collision ((),) C.mkCallback (\cb h -> h {C.chSeparateFunc = cb})

--   C.modifyCollisionHandler handlerPtr $ pure . updateBegin . updatePreSolve . updatePostSolve . updateSeparate . updateCollisionTypes

--   let event = callback $ \fin trigger -> do
--         key <- newUnique
--         modifyIORef' eventTriggers $ M.insert key trigger
--         addFinalizer fin $ modifyIORef' eventTriggers $ M.delete key

--   if or [isJust begin, isJust preSolve, isJust postSolve, isJust separate]
--     then pure event
--     else pure mempty
--   where
--     makeCallback eventTriggers maybeCallback makeCollision extractResult makeCallback insertFunction =
--       case maybeCallback of
--         Nothing -> pure id
--         Just f -> do
--           let newF arbiter collisionSpace _ = do
--                 (b, ma) <- extractResult <$> f (makeCollision arbiter) (CollisionSpace collisionSpace)
--                 case ma of
--                   Nothing -> pure b
--                   Just a -> readIORef eventTriggers >>= traverse_ ($ a) >> pure b

--           cb <- makeCallback newF

--           pure (insertFunction cb)

-- data PostStepCallbacks = PostStepCallbacks
--   { unkeyedCallbacks :: !(IO ()),
--     keyedCallbacks :: !(IM.IntMap (IO ()))
--   }

-- emptyPostStepCallbacks :: PostStepCallbacks
-- emptyPostStepCallbacks = PostStepCallbacks mempty mempty

-- -- | Add a post step callback with a key. If the key already exists, then the action will not be added and `False` is returned.
-- addKeyedCallback :: Space -> Int -> IO () -> IO Bool
-- addKeyedCallback space key action = do
--   callbacks <- get (C.spaceUserData space) >>= deRefStablePtr . castPtrToStablePtr
--   atomicModifyIORef' callbacks $ \postStepCallbacks ->
--     let exists = IM.member key postStepCallbacks.keyedCallbacks
--      in if exists
--           then (postStepCallbacks, False)
--           else (postStepCallbacks {keyedCallbacks = IM.insert key action postStepCallbacks.keyedCallbacks}, True)

-- | Add a post step callback
-- addCallback :: Step s () -> CollisionM phase s ()
-- addCallback step = makeCollisionM
--   modifyIORef' callbacks $ \postStepCallbacks ->
--     postStepCallbacks {unkeyedCallbacks = postStepCallbacks.unkeyedCallbacks >> action}
mapToVar :: IOE s -> (b -> a) -> (a -> b) -> StateVar a -> Var s b
mapToVar (IOE lift) f g stateVar =
  let sv = mapStateVar f g stateVar
   in makeVar (lift $ get sv) (lift . ($=) sv)

toVar :: IOE s -> StateVar a -> Var s a
toVar ioe = mapToVar ioe id id

toStep :: IOE s -> GettableStateVar a -> Step s a
toStep (IOE lift) sv = lift (get sv)

v2ToVect :: V2 Double -> C.Vect
v2ToVect (V2 x y) = C.Vect x y

vectToV2 :: C.Vect -> V2 Double
vectToV2 (C.Vect x y) = V2 x y
