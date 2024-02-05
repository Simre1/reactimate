{-# LANGUAGE LinearTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Reactimate.Game.Graphics (render, SDL.Window, Picture, Camera (..), makePicture, PictureAtoms (..), staticPicture, packColour, packAlphaColour) where

import Control.Monad
import Data.Colour
import Data.Colour.SRGB (RGB (..), toSRGB24)
import Data.IntMap.Strict qualified as IM
import Data.Sequence qualified as S
import Data.Vector.Storable qualified as VS
import Data.Word (Word8)
import Foreign.Ptr
import Foreign.Storable (Storable (..))
import GHC.Generics (Generic)
import Linear.V2
import Linear.V4
import Paths_reactimate_game (getDataDir)
import Reactimate (Signal, arrIO)
import Reactimate.Delay (once)
import Reactimate.Environment (withResourceSetup)
import Reactimate.Game.Shapes
import Reactimate.Signal (addFinalizer)
import SDL qualified
import SDL.Primitive qualified as SDL
import System.FilePath ((</>))

-- | Renders the `Picture` to the given `Window` with the `Camera` each frame.
render :: (r -> SDL.Window) -> Signal r (Camera, Picture) ()
render getWindow =
  withResourceSetup
    ( \fin r -> do
        dataDir <- getDataDir
        let window = getWindow r
            primitivesVertexShader = dataDir </> "shaders" </> "primitives.vert"
            primitivesFragmentShader = dataDir </> "shaders" </> "primitives.frag"
        -- shader <- R.loadShader (Just primitivesVertexShader) (Just primitivesFragmentShader) windowResources
        -- addFinalizer fin $ R.unloadShader shader windowResources
        renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
        addFinalizer fin $ SDL.destroyRenderer renderer
        pure (r, (renderer, window))
    )
    $ arrIO
    $ \((camera, picture), (renderer, window)) -> renderScreen window renderer camera picture
{-# INLINE render #-}

-- | A `Picture` is a collection of `PictureAtom`s. `Picture` implements `Semigroup`,
-- so multiple `Picture`s can be combined.
newtype Picture = Picture
  { pictureParts :: IM.IntMap PicturePart
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

instance Semigroup PicturePart where
  pp1@(PicturePart position1 parts1) <> pp2@(PicturePart position2 parts2) =
    if position1 == position2
      then PicturePart position1 (parts1 <> parts2)
      else PicturePart (V2 0 0) $ S.fromList [pp1, pp2]
  pp1 <> pp2@(PicturePartAtoms _) = pp1 <> PicturePart (V2 0 0) (S.singleton pp2)
  pp1@(PicturePartAtoms _) <> pp2 = PicturePart (V2 0 0) (S.singleton pp1) <> pp2

newtype PictureAtoms = BasicShapes (VS.Vector (ColouredShape BasicShape)) deriving (Eq, Show)

-- | A `Camera` can move around and zoom out and in. Only objects which are
-- visible by the `Camera` are rendered.
data Camera = Camera
  { position :: {-# UNPACK #-} !(V2 Int),
    viewport :: {-# UNPACK #-} !(V2 Int)
  }
  deriving (Eq, Show, Generic)

-- | Make a `Picture` from a `PictureAtom` at the given z-level.
-- `PictureAtom`s with higher z-level are rendered over ones with lower z-level.
makePicture :: Int -> PictureAtoms -> Picture
makePicture zIndex pictureAtoms =
  Picture $ IM.singleton zIndex $ PicturePartAtoms pictureAtoms

-- | Creates the `Picture` once and then reuses it in all subsequenct renders. If you have some static content,
-- use this function to save some computation time.
staticPicture :: Signal r a Picture -> Signal r a Picture
staticPicture = once

-- | Moves points from global coordinates to screen coordinates.
adjustPosition :: Camera -> V2 Int -> V2 Int -> V2 Int
adjustPosition (Camera (V2 cx cy) _) (V2 _ wY) (V2 x y) =
  V2 (x - cx) (wY - y + cy)

packAlphaColour :: AlphaColour Float -> V4 Word8
packAlphaColour colour =
  let (RGB r g b) = toSRGB24 (colour `over` black)
      alpha = truncate $ alphaChannel colour * 255
   in V4 r g b alpha

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
  where
    renderer = rc.renderer

-- | `PictureAtom`s are stored in a spatial map. `boundingBoxSize` is the
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
