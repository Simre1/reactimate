module Reactimate.Game.Projection2D where
import Linear (V2)
import Linear.V2 (V2(..))

data Projection2D a = Projection2D
  { p00 :: !a,
    p01 :: !a,
    p02 :: !a,
    p10 :: !a,
    p11 :: !a,
    p12 :: !a,
    p20 :: !a,
    p21 :: !a,
    p22 :: !a
  }


combineProjection :: Num a => Projection2D a -> Projection2D a -> Projection2D a
combineProjection p1 p2 = 
  Projection2D {
    p00 = p1.p00 * p2.p00 + p1.p01 * p2.p10 + p1.p02 * p2.p20,
    p01 = p1.p00 * p2.p01 + p1.p01 * p2.p11 + p1.p02 * p2.p21,
    p02 = p1.p00 * p2.p02 + p1.p01 * p2.p12 + p1.p02 * p2.p22,
    p10 = p1.p10 * p2.p00 + p1.p11 * p2.p10 + p1.p12 * p2.p20,
    p11 = p1.p10 * p2.p01 + p1.p11 * p2.p11 + p1.p12 * p2.p21,
    p12 = p1.p10 * p2.p02 + p1.p11 * p2.p12 + p1.p12 * p2.p22,
    p20 = p1.p20 * p2.p00 + p1.p21 * p2.p10 + p1.p22 * p2.p20,
    p21 = p1.p20 * p2.p01 + p1.p21 * p2.p11 + p1.p22 * p2.p21,
    p22 = p1.p20 * p2.p02 + p1.p21 * p2.p12 + p1.p22 * p2.p22
  }


zeroProjection :: Num a => Projection2D a
zeroProjection = Projection2D
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

idProjection :: Num a => Projection2D a
idProjection = zeroProjection {p00 = 1, p11 = 1, p22 = 1}

translateProjection :: Num a => V2 a -> Projection2D a
translateProjection (V2 x y) = idProjection {p01 = x, p02 = y} 

rotateProjection :: Num a => V2 a -> Projection2D a
rotateProjection (V2 x y) = idProjection {p01 = x, p02 = y} 
