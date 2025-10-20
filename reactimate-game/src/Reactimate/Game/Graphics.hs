{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Reactimate.Game.Graphics
  ( -- * Graphics effect
    Graphics,
    runGraphics,
    SDL.WindowConfig (..),
    SDL.defaultWindow,
    getWindowSize,
    getRenderer,

    -- * Rendering
    renderGame,
    Camera (..),

    -- ** Picture
    Picture,
    makePicture,
    staticPicture,
    drawRectangle,
    drawPolygon,
    blitImage,
    Blit (..),

    -- *** Image projections
    translatePicture,
    rotatePicture,
    projectPicture,

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
import Data.Bifunctor (Bifunctor (..))
import Data.Colour
import Data.Colour.SRGB (RGB (..), toSRGB24)
import Data.Foldable (toList)
import Data.Hashable (Hashable (..))
import Data.IntMap.Strict qualified as IM
import Data.Sequence qualified as S
import Data.Text (Text, unpack)
import Data.Vector.Storable qualified as VS
import Data.Word (Word8)
import Foreign (Storable (..))
import Foreign.C (CInt)
import Foreign.Ptr
import GHC.Generics (Generic)
import Linear.V2
import Linear.V4
import Reactimate
import Reactimate.Game.Assets (Asset (..), Assets, withAssetNow)
import Reactimate.Game.Projection2D
import Reactimate.Game.Shapes
import SDL qualified
import SDL.Image qualified as SDL
import SDL.Primitive qualified as SDL
import SDL.Raw.Types qualified as SDLRaw

data Graphics s = Graphics
  { window :: SDL.Window,
    renderer :: SDL.Renderer,
    ioe :: IOE s
  }

runGraphics ::
  (IOE :> es) =>
  -- | Window name
  Text ->
  SDL.WindowConfig ->
  Signal (Graphics : es) a b ->
  Signal es a b
runGraphics name initialWindowConfig signal = makeSignal $ do
  SDL.initializeAll
  window <- SDL.createWindow name initialWindowConfig
  finalize $ SDL.destroyWindow window

  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  finalize $ SDL.destroyRenderer renderer

  ioe <- getHandle

  let graphics = Graphics {window, renderer, ioe}
  runHandle graphics (unSignal signal)

getWindowSize :: Graphics s -> Step s (V2 Int)
getWindowSize (Graphics window _ (IOE lift)) =
  lift $ fmap fromIntegral <$> SDL.get (SDL.windowSize window)

getRenderer :: Graphics s -> Step s (SDL.Renderer)
getRenderer (Graphics _ renderer _) = pure renderer

getWindow :: Graphics s -> Step s (SDL.Window)
getWindow (Graphics window _ _) = pure window

-- | Renders the given `Picture` with the `Camera` each frame.
renderGame :: (Graphics :> es) => Signal es (Camera, Picture) ()
renderGame =
  arrStep $ \(Handle (Graphics window renderer (IOE lift))) -> lift . uncurry (renderScreen window renderer)
{-# INLINE renderGame #-}

-- | A `Picture` is a collection of `PictureAtoms`. `Picture` implements `Semigroup`,
-- so multiple `Picture`s can be combined.
newtype Picture = Picture
  { pictureParts :: IM.IntMap PicturePart
  }

instance Semigroup Picture where
  (Picture objects1) <> (Picture objects2) =
    Picture $
      IM.unionWith (<>) objects1 objects2

instance Monoid Picture where
  mempty = Picture IM.empty

data PicturePart
  = PicturePart
      { projection :: !(Projection2D Int),
        pictureParts :: S.Seq PicturePart
      }
  | PictureRender
      { render :: Render ()
      }

instance Semigroup PicturePart where
  pp1@(PicturePart movement1 parts1) <> pp2@(PicturePart movement2 parts2) =
    if movement1 == movement2
      then PicturePart movement1 (parts1 <> parts2)
      else PicturePart idProjection $ S.fromList [pp1, pp2]
  pp1 <> pp2@(PictureRender _) = pp1 <> PicturePart idProjection (S.singleton pp2)
  pp1@(PictureRender _) <> pp2 = PicturePart idProjection (S.singleton pp1) <> pp2

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

-- | A `Camera` can move around and zoom out and in.
data Camera = Camera
  { position :: {-# UNPACK #-} !(V2 Int),
    viewport :: {-# UNPACK #-} !(V2 Int)
  }
  deriving (Eq, Show, Generic)

-- | Make a `Picture` from a `PictureAtoms` at the given z-level.
-- `PictureAtoms` with higher z-level are rendered over ones with lower z-level.
makePicture :: Int -> Render () -> Picture
makePicture zIndex action =
  Picture $ IM.singleton zIndex $ PictureRender action

-- | Creates the `Picture` once and then reuses it in all subsequenct renders. If you have some static content,
-- use this function to save some computation time.
staticPicture :: Signal es a Picture -> Signal es a Picture
staticPicture = once

-- | Pack an `AlphaColour` so that it can be used for `PictureAtoms`. Include the /colour/ package to make colours!
packAlphaColour :: AlphaColour Float -> V4 Word8
packAlphaColour colour =
  let (RGB r g b) = toSRGB24 (colour `over` black)
      alpha = truncate $ alphaChannel colour * 255
   in V4 r g b alpha
{-# INLINE packAlphaColour #-}

-- | Pack a `Colour` so that it can be used for `PictureAtoms`. Include the /colour/ package to make colours!
packColour :: Colour Float -> V4 Word8
packColour colour =
  let (RGB r g b) = toSRGB24 colour
   in V4 r g b 255
{-# INLINE packColour #-}

renderScreen :: SDL.Window -> SDL.Renderer -> Camera -> Picture -> IO ()
renderScreen window renderer camera picture = do
  windowSize <- SDL.get (SDL.windowSize window)

  SDL.rendererDrawColor renderer SDL.$= V4 255 255 255 255
  SDL.clear renderer
  renderPicture (RenderContext window renderer) (computeCameraProjection (fromIntegral <$> windowSize) camera) picture
  SDL.present renderer

data RenderContext = RenderContext
  { window :: !SDL.Window,
    renderer :: !SDL.Renderer
  }

renderPicture :: RenderContext -> Projection2D Int -> Picture -> IO ()
renderPicture rc projection (Picture pictureParts) = forM_ pictureParts $ \picturePart -> renderPicturePart rc projection picturePart

renderPicturePart :: RenderContext -> Projection2D Int -> PicturePart -> IO ()
renderPicturePart rc projection1 (PicturePart projection2 nestedParts) = forM_ nestedParts $ renderPicturePart rc (projection1 *+* projection2)
renderPicturePart rc projection (PictureRender (Render f)) =
  f rc projection

-- | Draw a filled rectangle with the given colour
drawRectangle :: V4 Word8 -> Rectangle -> Render ()
drawRectangle (V4 r g b a) (Rectangle position (V2 sizeX sizeY)) = Render $ \rc projection ->
  let vertices = VS.fromList $ makeVertex . fmap fromIntegral . applyProjection quot projection <$> [position, position + V2 sizeX 0, position + V2 sizeX sizeY, position + V2 0 sizeY]
      indices = VS.fromList [0, 1, 2, 3, 2, 0]
   in SDL.renderGeometry rc.renderer Nothing vertices indices
  where
    makeVertex (V2 dX dY) =
      SDL.Vertex
        (SDLRaw.FPoint dX dY)
        (SDLRaw.Color r g b a)
        (SDLRaw.FPoint 0 0)

-- | Draw a filled polygon with the given colour
drawPolygon :: V4 Word8 -> [V2 Int] -> Render ()
drawPolygon colour vertices = Render $ \rc projection ->
  let points = applyProjection quot projection <$> vertices
      xs = VS.fromList $ fmap (fromIntegral . getX) points
      ys = VS.fromList $ fmap (fromIntegral . getY) points
   in SDL.fillPolygon rc.renderer xs ys colour

-- | Blit rectangular portions of an image to the screen.
blitImage :: [Blit] -> Image -> Render ()
blitImage blits (Image texture (V2 iWidth iHeight)) = Render $ \rc projection ->
  let (vertices, indices) = bimap (VS.fromList . toList) (VS.fromList . toList) $ generateGeometry projection (S.empty, S.empty) 0 blits
   in SDL.renderGeometry rc.renderer (Just texture) vertices indices
  where
    generateGeometry :: Projection2D Int -> (S.Seq SDL.Vertex, S.Seq CInt) -> CInt -> [Blit] -> (S.Seq SDL.Vertex, S.Seq CInt)
    generateGeometry _ geometry _ [] = geometry
    generateGeometry projection (vertices, indices) n (Blit (Rectangle source (V2 sourceWidth sourceHeight)) (Rectangle dest (V2 destWidth destHeight)) : blits) =
      let sourceVertices = fmap fromIntegral . (source +) <$> [V2 0 0, V2 sourceWidth 0, V2 0 sourceHeight, V2 sourceWidth sourceHeight]
          destVertices = fmap fromIntegral . applyProjection quot projection . (dest +) <$> [V2 0 0, V2 destWidth 0, V2 0 destHeight, V2 destWidth destHeight]
       in generateGeometry
            projection
            (vertices <> S.fromList (zipWith makeVertex sourceVertices destVertices), indices <> S.fromList ((n +) <$> [0, 1, 2, 3, 2, 1]))
            (n + 4)
            blits
    makeVertex (V2 sX sY) (V2 dX dY) =
      SDL.Vertex
        (SDLRaw.FPoint dX dY)
        (SDLRaw.Color 255 255 255 255)
        ( SDLRaw.FPoint
            (sX / fromIntegral iWidth)
            (abs $ 1 - (sY / fromIntegral iHeight))
        )

translatePicture :: V2 Int -> Picture -> Picture
translatePicture v (Picture parts) = Picture $ IM.map translatePicturePart parts
  where
    translatePicturePart (PicturePart projection parts) = PicturePart (translateProjection v projection) parts
    translatePicturePart (PictureRender render) = PicturePart (translation v) $ S.fromList [PictureRender render]

-- | Rotate the `Picture` around the origin (0,0)
rotatePicture :: Float -> Picture -> Picture
rotatePicture r (Picture parts) = Picture $ IM.map translatePicturePart parts
  where
    translatePicturePart (PicturePart projection parts) = PicturePart (approximateRotation r *+* projection) parts
    translatePicturePart (PictureRender render) = PicturePart (approximateRotation r) $ S.fromList [PictureRender render]

-- | Apply a homogenous projection to the picture. A projection can translate, rotate, reflect or skew a picture.
projectPicture :: Projection2D Int -> Picture -> Picture
projectPicture projection (Picture parts) = Picture $ IM.map projectPicturePart parts
  where
    projectPicturePart (PicturePart innerProjection parts) = PicturePart (projection *+* innerProjection) parts
    projectPicturePart (PictureRender render) = PicturePart projection $ S.fromList [PictureRender render]

computeCameraProjection :: V2 Int -> Camera -> Projection2D Int
computeCameraProjection (V2 wx wy) (Camera (V2 cx cy) (V2 vx vy)) =
  zeroProjection {p00 = vx * vy, p11 = wx * vy, p22 = -wy * vx, p10 = -cx * wx * vy, p20 = vx * wy * (vy + cy)}

data ImagePath = ImagePath
  { path :: !Text
  }
  deriving (Eq, Show, Ord, Generic)

instance Hashable ImagePath where
  hashWithSalt i (ImagePath path) = hashWithSalt i path

-- | An `Image` contains the GPU texture of the image so that it can be used
-- for rendering
data Image = Image
  { texture :: !SDL.Texture,
    size :: !(V2 Int)
  }
  deriving (Eq)

instance Asset ImagePath where
  type AssetValue ImagePath = Image
  type AssetEffects ImagePath = '[Graphics]
  loadAsset (Handle (Graphics _ renderer (IOE lift))) (ImagePath path) = lift $ do
    texture <- SDL.loadTexture renderer $ unpack path
    textureInfo <- SDL.queryTexture texture
    pure $ Image texture $ fromIntegral <$> V2 textureInfo.textureWidth textureInfo.textureHeight
  freeAsset _ image = do
    SDL.destroyTexture image.texture

-- | Load an image from the given path during the setup phase such that you can use it for rendering
withImage :: (Graphics :> es, IOE :> es, Assets :> es) => Text -> (Image -> Signal es a b) -> Signal es a b
withImage path = withAssetNow (ImagePath path)

newtype Render a = Render (RenderContext -> Projection2D Int -> IO a)
  deriving (Functor, Semigroup, Monoid)

instance Applicative Render where
  pure a = Render $ \_ _ -> pure a
  (Render f) <*> (Render a) = Render $ \rc projection -> f rc projection <*> a rc projection
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad Render where
  (Render makeA) >>= f = Render $ \rc projection -> do
    a <- makeA rc projection
    let (Render makeB) = f a
    makeB rc projection
  {-# INLINE (>>=) #-}

getX :: V2 a -> a
getX (V2 x _) = x

getY :: V2 a -> a
getY (V2 _ y) = y
