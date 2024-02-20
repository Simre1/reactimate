{-# LANGUAGE TypeFamilies #-}

module Reactimate.Game.Graphics
  ( renderGame,
    Camera (..),

    -- * Picture
    Picture,
    makePicture,
    staticPicture,
    PictureAtoms (..),
    Blit (..),

    -- ** Colours
    packColour,
    Colour,
    packAlphaColour,
    AlphaColour,

    -- ** Image loading
    Image (..),
    withImage,
  )
where

import Control.Monad
import Data.Colour
import Data.Colour.SRGB (RGB (..), toSRGB24)
import Data.Hashable (Hashable (..))
import Data.Hashable.Generic (genericHashWithSalt)
import Data.IntMap.Strict qualified as IM
import Data.Sequence qualified as S
import Data.Text (Text, unpack)
import Data.Vector.Storable qualified as VS
import Data.Word (Word8)
import Foreign (Storable (..))
import Foreign.Ptr
import GHC.Generics (Generic)
import Linear.V2
import Linear.V4
import Reactimate (Signal, arrIO, once)
import Reactimate.Game.Assets (Asset (..), withAsset)
import Reactimate.Game.Environment (GameEnv (..))
import Reactimate.Game.Shapes
import SDL qualified
import SDL.Image qualified as SDL
import SDL.Primitive qualified as SDL

-- | Renders the given `Picture` with the `Camera` each frame.
renderGame :: GameEnv -> Signal (Camera, Picture) ()
renderGame gameEnv =
  arrIO $
    uncurry (renderScreen gameEnv.window gameEnv.renderer)

{-# INLINE renderPicture #-}

-- | A `Picture` is a collection of `PictureAtoms`. `Picture` implements `Semigroup`,
-- so multiple `Picture`s can be combined.
newtype Picture = Picture
  { pictureParts :: IM.IntMap PicturePart
  }
  deriving (Eq)

instance Semigroup Picture where
  (Picture objects1) <> (Picture objects2) =
    Picture $
      IM.unionWith (<>) objects1 objects2

instance Monoid Picture where
  mempty = Picture IM.empty

data PicturePart
  = PicturePart
      { position :: !(V2 Int),
        pictureParts :: !(S.Seq PicturePart)
      }
  | PicturePartAtoms
      { pictureAtoms :: !PictureAtoms
      }
  deriving (Eq)

instance Semigroup PicturePart where
  pp1@(PicturePart position1 parts1) <> pp2@(PicturePart position2 parts2) =
    if position1 == position2
      then PicturePart position1 (parts1 <> parts2)
      else PicturePart (V2 0 0) $ S.fromList [pp1, pp2]
  pp1 <> pp2@(PicturePartAtoms _) = pp1 <> PicturePart (V2 0 0) (S.singleton pp2)
  pp1@(PicturePartAtoms _) <> pp2 = PicturePart (V2 0 0) (S.singleton pp1) <> pp2

-- | `PictureAtoms` are the building blocks of `Picture`s.
data PictureAtoms
  = BasicShapes !(VS.Vector (ColouredShape BasicShape))
  | Texture !Image !(VS.Vector Blit)
  deriving (Eq)

-- | A `Blit` contains source and target rectangles. They are used to copy
-- parts of some source texture onto a target texture. For `PictureAtoms`, the target is the screen.
data Blit = Blit
  { source :: !Rectangle,
    target :: !Rectangle
  }
  deriving (Eq, Show)

instance Storable Blit where
  sizeOf _ = sizeOf (undefined :: V2 Rectangle)
  alignment _ = alignment (undefined :: V2 Rectangle)
  peek ptr = do
    V2 r1 r2 <- peek $ castPtr ptr
    pure $ Blit r1 r2
  poke ptr (Blit r1 r2) = do
    poke (castPtr ptr) (V2 r1 r2)

-- | A `Camera` can move around and zoom out and in. Only objects which are
-- visible by the `Camera` are rendered.
data Camera = Camera
  { position :: {-# UNPACK #-} !(V2 Int),
    viewport :: {-# UNPACK #-} !(V2 Int)
  }
  deriving (Eq, Show, Generic)

-- | Make a `Picture` from a `PictureAtoms` at the given z-level.
-- `PictureAtoms` with higher z-level are rendered over ones with lower z-level.
makePicture :: Int -> PictureAtoms -> Picture
makePicture zIndex pictureAtoms =
  Picture $ IM.singleton zIndex $ PicturePartAtoms pictureAtoms

-- | Creates the `Picture` once and then reuses it in all subsequenct renders. If you have some static content,
-- use this function to save some computation time.
staticPicture :: Signal a Picture -> Signal a Picture
staticPicture = once

-- | Moves points from global coordinates to screen coordinates.
adjustPosition :: Camera -> V2 Int -> V2 Int -> V2 Int
adjustPosition (Camera (V2 cx cy) (V2 vx vy)) (V2 wx wy) (V2 x y) =
  V2 ((x - cx) * wx `quot` vx) (wy - ((y - cy) * wy `quot` vy))

-- | Pack an `AlphaColour` so that it can be used for `PictureAtoms`. Include the /colour/ package to make colours!
packAlphaColour :: AlphaColour Float -> V4 Word8
packAlphaColour colour =
  let (RGB r g b) = toSRGB24 (colour `over` black)
      alpha = truncate $ alphaChannel colour * 255
   in V4 r g b alpha

-- | Pack a `Colour` so that it can be used for `PictureAtoms`. Include the /colour/ package to make colours!
packColour :: Colour Float -> V4 Word8
packColour colour =
  let (RGB r g b) = toSRGB24 colour
   in V4 r g b 255

renderScreen :: SDL.Window -> SDL.Renderer -> Camera -> Picture -> IO ()
renderScreen window renderer camera picture = do
  windowSize <- SDL.get (SDL.windowSize window)

  SDL.rendererDrawColor renderer SDL.$= V4 255 255 255 255
  SDL.clear renderer
  renderPicture (RenderContext window renderer (fromIntegral <$> windowSize) camera) picture
  SDL.present renderer

data RenderContext = RenderContext
  { window :: !SDL.Window,
    renderer :: !SDL.Renderer,
    renderSize :: !(V2 Int),
    camera :: !Camera
  }

renderPicture :: RenderContext -> Picture -> IO ()
renderPicture rc (Picture pictureParts) = forM_ pictureParts $ \picturePart -> renderPicturePart rc (V2 0 0) picturePart

renderPicturePart :: RenderContext -> V2 Int -> PicturePart -> IO ()
renderPicturePart rc offset (PicturePart position nestedParts) = forM_ nestedParts $ renderPicturePart rc (position + offset)
renderPicturePart rc offset (PicturePartAtoms atoms) =
  renderAtoms rc offset atoms

renderAtoms :: RenderContext -> V2 Int -> PictureAtoms -> IO ()
renderAtoms rc offset atoms = case atoms of
  BasicShapes basicShapes -> VS.forM_ basicShapes $ \(ColouredShape colour basicShape) -> case basicShape of
    (BSRectangle (Rectangle position size)) -> do
      let (V2 x y) = adjustPosition rc.camera rc.renderSize (position + offset)
          (V2 sx sy) = quot <$> size * rc.renderSize <*> rc.camera.viewport
      SDL.fillRectangle renderer (fromIntegral <$> V2 (x + sx) (y - sy)) (fromIntegral <$> V2 x y) colour
    (BSEllipse (Ellipse position size)) -> do
      let pos = fromIntegral <$> adjustPosition rc.camera rc.renderSize (position + offset)
          (V2 sx sy) = fmap fromIntegral $ quot <$> size * rc.renderSize <*> rc.camera.viewport
      SDL.fillEllipse renderer pos sx sy colour
    (BSTriangle (Triangle position1 position2 position3)) -> do
      let pos1 = fromIntegral <$> adjustPosition rc.camera rc.renderSize (position1 + offset)
          pos2 = fromIntegral <$> adjustPosition rc.camera rc.renderSize (position2 + offset)
          pos3 = fromIntegral <$> adjustPosition rc.camera rc.renderSize (position3 + offset)
      SDL.fillTriangle renderer pos1 pos2 pos3 colour
    (BSCircularArc (CircularArc position radius startDegree endDegree)) -> do
      let pos = fromIntegral <$> adjustPosition rc.camera rc.renderSize (position + offset)
      SDL.fillPie renderer pos (fromIntegral radius) (fromIntegral startDegree) (fromIntegral endDegree) colour
  Texture image blits -> VS.forM_ blits $ \(Blit source target) -> do
    let (V2 _ ty) = image.size
        (V2 spx spy) = source.position
        (V2 _ ssy) = source.size
        textureSource = SDL.Rectangle (fmap fromIntegral $ SDL.P $ V2 spx (ty - spy - ssy)) (fromIntegral <$> source.size)
        (V2 tpx tpy) = adjustPosition rc.camera rc.renderSize (target.position + offset)
        ts@(V2 _ tsy) = quot <$> target.size * rc.renderSize <*> rc.camera.viewport
        screenTarget = SDL.Rectangle (fmap fromIntegral $ SDL.P $ V2 tpx (tpy - tsy)) (fromIntegral <$> ts)
    SDL.copy renderer image.texture (Just textureSource) (Just screenTarget)
  where
    renderer = rc.renderer

-- toSDLRectangle (Rectangle position@(V2 x y) size@(V2 sx sy)) = SDL.Rectangle (SDL.P $ fromIntegral <$> V2 x (ty - y)) (fromIntegral <$> size)

data ImagePath = ImagePath
  { renderer :: !SDL.Renderer,
    path :: !Text
  }
  deriving (Eq, Show, Ord, Generic)

instance Hashable ImagePath where
  hashWithSalt i (ImagePath renderer path) =
    let i' = genericHashWithSalt i renderer
     in hashWithSalt i' path

-- | An `Image` contains the GPU texture of the image so that it can be used
-- for rendering
data Image = Image
  { texture :: !SDL.Texture,
    size :: !(V2 Int)
  }
  deriving (Eq)

instance Asset ImagePath where
  type AssetValue ImagePath = Image
  type AssetEnv ImagePath = ()
  loadAsset () (ImagePath renderer path) = do
    texture <- SDL.loadTexture renderer $ unpack path
    textureInfo <- SDL.queryTexture texture
    pure $ Image texture $ fromIntegral <$> V2 textureInfo.textureWidth textureInfo.textureHeight
  freeAsset _ image = do
    SDL.destroyTexture image.texture

-- | Load an image from the given path during the setup phase such that you can use it for rendering
withImage :: GameEnv -> Text -> (Image -> Signal a b) -> Signal a b
withImage gameEnv path = withAsset gameEnv.assets () (ImagePath gameEnv.renderer path)

-- | `PictureAtoms` are stored in a spatial map. `boundingBoxSize` is the
-- size of each quadrant.
-- boundingBoxSize :: V2 Int
-- boundingBoxSize = pure 2048

-- -- | Computes the bounding box index for a global coordinate
-- positionToBoundingBox :: V2 Int -> V2 Int
-- positionToBoundingBox position =
--   let fractionalBox = liftA2 (%) (position + ((`quot` 2) <$> boundingBoxSize)) boundingBoxSize
--    in floor <$> fractionalBox

-- -- | Computes all the bounding box indices for a rectangular box
-- boundingBoxes :: V2 Int -> V2 Int -> [V2 Int]
-- boundingBoxes position size =
--   let halfSize = (`quot` 2) <$> size
--       (V2 left bottom) = positionToBoundingBox $ position - halfSize
--       (V2 right top) = positionToBoundingBox $ position + halfSize
--    in sequenceA $ V2 [left .. right] [bottom .. top]
