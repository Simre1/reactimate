{-# LANGUAGE AllowAmbiguousTypes #-}

module Reactimate.Game.Shapes where

import Data.Kind (Type)
import Foreign
import Linear.V2 (V2 (..))
import Linear.V4 (V4)

data Ellipse = Ellipse
  { position :: {-# UNPACK #-} !(V2 Int),
    radii :: {-# UNPACK #-} !(V2 Int)
  }
  deriving (Eq, Show)

data Rectangle = Rectangle
  { position :: {-# UNPACK #-} !(V2 Int),
    size :: {-# UNPACK #-} !(V2 Int)
  }
  deriving (Eq, Show)

data Triangle = Triangle
  { position1 :: {-# UNPACK #-} !(V2 Int),
    position2 :: {-# UNPACK #-} !(V2 Int),
    position3 :: {-# UNPACK #-} !(V2 Int)
  }
  deriving (Eq, Show)

data CircularArc = CircularArc
  { position :: {-# UNPACK #-} !(V2 Int),
    radius :: {-# UNPACK #-} !Int,
    startDegree :: {-# UNPACK #-} !Int,
    endDegree :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

data BasicShape
  = BSRectangle !Rectangle
  | BSEllipse !Ellipse
  | BSTriangle !Triangle
  | BSCircularArc !CircularArc
  deriving (Eq, Show)

data ColouredShape x = ColouredShape
  { colour :: V4 Word8,
    shape :: x
  }
  deriving (Eq, Show)

instance Storable Ellipse where
  sizeOf _ = byteSize @(V2 Int) + byteSize @(V2 Int)
  alignment _ = 0
  peek ptr = do
    position <- peek $ castPtr ptr
    size <- peek (ptr `plusPtr` byteSize @(V2 Int))
    pure $ Ellipse position size
  poke ptr (Ellipse position size) = do
    poke (castPtr ptr) position
    poke (ptr `plusPtr` byteSize @(V2 Int)) size

instance Storable Rectangle where
  sizeOf _ = byteSize @(V2 Int) + byteSize @(V2 Int)
  alignment _ = 0
  peek ptr = do
    position <- peek $ castPtr ptr
    size <- peek (ptr `plusPtr` byteSize @(V2 Int))
    pure $ Rectangle position size
  poke ptr (Rectangle position size) = do
    poke (castPtr ptr) position
    poke (ptr `plusPtr` byteSize @(V2 Int)) size

instance Storable Triangle where
  sizeOf _ = 3 * byteSize @(V2 Int)
  alignment _ = 0
  peek ptr = do
    position1 <- peek $ castPtr ptr
    position2 <- peek $ castPtr (ptr `plusPtr` byteSize @(V2 Int))
    position3 <- peek $ castPtr (ptr `plusPtr` (byteSize @(V2 Int) + byteSize @(V2 Int)))
    pure $ Triangle position1 position2 position3
  poke ptr (Triangle position1 position2 position3) = do
    poke (castPtr ptr) position1
    poke (castPtr (ptr `plusPtr` byteSize @(V2 Int))) position2
    poke (castPtr (ptr `plusPtr` (byteSize @(V2 Int) + byteSize @(V2 Int)))) position3

instance Storable CircularArc where
  sizeOf _ = byteSize @(V2 Int) + 3 * byteSize @Int
  alignment _ = 0
  peek ptr = do
    position <- peek $ castPtr ptr
    radius <- peek (ptr `plusPtr` byteSize @Int)
    startDegree <- peek (ptr `plusPtr` (2 * byteSize @Int))
    endDegree <- peek (ptr `plusPtr` (3 * byteSize @Int))
    pure $ CircularArc position radius startDegree endDegree
  poke ptr (CircularArc position radius startDegree endDegree) = do
    poke (castPtr ptr) position
    poke (ptr `plusPtr` byteSize @Int) radius
    poke (ptr `plusPtr` (2 * byteSize @Int)) startDegree
    poke (ptr `plusPtr` (3 * byteSize @Int)) endDegree

instance Storable BasicShape where
  sizeOf _ = byteSize @Word8 + maximum [byteSize @Rectangle, byteSize @Triangle, byteSize @Ellipse, byteSize @CircularArc]
  alignment _ = 0
  peek ptr = do
    type_ <- peek @Word8 (castPtr ptr)
    let shapePtr = ptr `plusPtr` byteSize @Word8
    case type_ of
      0 -> BSRectangle <$> peek shapePtr
      1 -> BSEllipse <$> peek shapePtr
      2 -> BSTriangle <$> peek shapePtr
      3 -> BSCircularArc <$> peek shapePtr
      _ -> error $ "Memory corruption. A primitive shape could not be read from " ++ show shapePtr
  poke ptr primitiveShape = do
    let shapePtr = ptr `plusPtr` byteSize @Word8
    case primitiveShape of
      BSRectangle rectangle -> do
        poke @Word8 (castPtr ptr) 0
        poke shapePtr rectangle
      BSEllipse ellipse -> do
        poke @Word8 (castPtr ptr) 1
        poke shapePtr ellipse
      BSTriangle triangle -> do
        poke @Word8 (castPtr ptr) 2
        poke shapePtr triangle
      BSCircularArc circularArc -> do
        poke @Word8 (castPtr ptr) 3
        poke shapePtr circularArc

instance (Storable shape) => Storable (ColouredShape shape) where
  sizeOf _ = byteSize @(V4 Word8) + byteSize @shape
  alignment _ = 0
  peek ptr = do
    colour <- peek $ castPtr ptr
    shape <- peek (ptr `plusPtr` byteSize @(V4 Word8))
    pure $ ColouredShape colour shape
  poke ptr (ColouredShape colour shape) = do
    poke (castPtr ptr) colour
    poke (ptr `plusPtr` byteSize @(V4 Word8)) shape

byteSize :: forall (a :: Type). (Storable a) => Int
byteSize = sizeOf (bottom :: a)
  where
    bottom = bottom
