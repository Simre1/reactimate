module Reactimate.Game.Projection2D where

import Linear (V2)
import Linear.V2 (V2 (..))

data Projection2D a = Projection2D
  { p00 :: {-# UNPACK #-} !a,
    p01 :: {-# UNPACK #-} !a,
    p02 :: {-# UNPACK #-} !a,
    p10 :: {-# UNPACK #-} !a,
    p11 :: {-# UNPACK #-} !a,
    p12 :: {-# UNPACK #-} !a,
    p20 :: {-# UNPACK #-} !a,
    p21 :: {-# UNPACK #-} !a,
    p22 :: {-# UNPACK #-} !a
  }
  deriving (Eq, Show)

combineProjection :: (Num a) => Projection2D a -> Projection2D a -> Projection2D a
combineProjection p1 p2 =
  Projection2D
    { p00 = p1.p00 * p2.p00 + p1.p01 * p2.p10 + p1.p02 * p2.p20,
      p01 = p1.p00 * p2.p01 + p1.p01 * p2.p11 + p1.p02 * p2.p21,
      p02 = p1.p00 * p2.p02 + p1.p01 * p2.p12 + p1.p02 * p2.p22,
      p10 = p1.p10 * p2.p00 + p1.p11 * p2.p10 + p1.p12 * p2.p20,
      p11 = p1.p10 * p2.p01 + p1.p11 * p2.p11 + p1.p12 * p2.p21,
      p12 = p1.p10 * p2.p02 + p1.p11 * p2.p12 + p1.p12 * p2.p22,
      p20 = p1.p20 * p2.p00 + p1.p21 * p2.p10 + p1.p22 * p2.p20,
      p21 = p1.p20 * p2.p01 + p1.p21 * p2.p11 + p1.p22 * p2.p21,
      p22 = p1.p20 * p2.p02 + p1.p21 * p2.p12 + p1.p22 * p2.p22
    }
{-# INLINE combineProjection #-}

(***) :: (Num a) => Projection2D a -> Projection2D a -> Projection2D a
(***) = combineProjection
{-# INLINE (***) #-}

zeroProjection :: (Num a) => Projection2D a
zeroProjection =
  Projection2D
    { p00 = 0,
      p01 = 0,
      p02 = 0,
      p10 = 0,
      p11 = 0,
      p12 = 0,
      p20 = 0,
      p21 = 0,
      p22 = 0
    }
{-# INLINE zeroProjection #-}

idProjection :: (Num a) => Projection2D a
idProjection = zeroProjection {p00 = 1, p11 = 1, p22 = 1}
{-# INLINE idProjection #-}

translateProjection :: (Num a) => V2 a -> Projection2D a -> Projection2D a
translateProjection (V2 x y) projection =
  projection {p10 = projection.p10 + projection.p00 * x, p20 = projection.p20 + projection.p00 * y}
{-# INLINE translateProjection #-}

translation :: (Num a) => V2 a -> Projection2D a
translation (V2 x y) =
  idProjection {p10 = x, p20 = y}
{-# INLINE translation #-}

rotation :: (Floating a) => a -> Projection2D a
rotation r =
  Projection2D
    1
    0
    0
    0
    (cos r)
    (sin r)
    0
    (-sin r)
    (cos r)
{-# INLINE rotation #-}

approximateRotation :: (Integral a, Floating b, RealFrac b) => b -> Projection2D a
approximateRotation r =
  Projection2D
    360
    0
    0
    0
    (round $ 360 * cos r)
    (round $ 360 * sin r)
    0
    (round $ 360 * (-sin r))
    (round $ 360 * cos r)
{-# INLINE approximateRotation #-}

rotateAtCenter :: (Floating a) => a -> Projection2D a -> Projection2D a
rotateAtCenter r projection =
  let (V2 x y) = V2 projection.p10 projection.p20
   in (rotation r *** projection {p10 = 0, p20 = 0}) {p10 = x, p20 = y}
{-# INLINE rotateAtCenter #-}

approximatelyRotateAtCenter :: (Integral a, Floating b, RealFrac b) => b -> Projection2D a -> Projection2D a
approximatelyRotateAtCenter r projection =
  let (V2 x y) = V2 projection.p10 projection.p20
   in (approximateRotation r *** projection {p10 = 0, p20 = 0}) {p10 = x * 360, p20 = y * 360}
{-# INLINE approximatelyRotateAtCenter #-}

rotationAround :: (Floating a) => a -> V2 a -> Projection2D a
rotationAround r (V2 x y) =
  translation (V2 x y) *** rotation r *** translation (-V2 x y)
{-# INLINE rotationAround #-}

approximateRotationAround :: (Floating a, Integral b, RealFrac a) => a -> V2 b -> Projection2D b
approximateRotationAround r (V2 x y) =
  translation (V2 x y) *** approximateRotation r *** translation (-V2 x y)
{-# INLINE approximateRotationAround #-}

applyProjection :: (Num a) => (a -> a -> a) -> Projection2D a -> V2 a -> V2 a
applyProjection divide (Projection2D p00 p01 p02 p10 p11 p12 p20 p21 p22) (V2 x y) =
  let z = p00 + p01 * x + p02 * y
   in (`divide` z) <$> V2 (p10 + p11 * x + p12 * y) (p20 + p21 * x + p22 * y)
{-# INLINE applyProjection #-}
