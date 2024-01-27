module Reactimate.Game.Graphics (render, SDL.Window, Picture, Camera (..), makePicture, PictureAtom (..), renderOnce) where

import Control.Applicative (Applicative (..))
import Control.Monad
import Data.Colour
import Data.Colour.SRGB (RGB (..), toSRGB24)
import Data.IntMap.Strict qualified as IM
import Data.PQueue.Prio.Max qualified as MQ
import Data.Ratio ((%))
import Data.Word (Word8)
import GHC.Generics (Generic)
import Linear.V2
import Linear.V4
import Reactimate (Signal, arrIO, withSetup)
import SDL qualified
import Reactimate.Delay (once)
import Reactimate.Environment (withResourceSetup)
import Reactimate.Signal (addFinalizer)

render :: (r -> SDL.Window) -> Signal r (Camera, Picture) ()
render getWindow = withResourceSetup
  ( \fin r -> do
      let window = getWindow r
      renderer <- createRenderer window
      addFinalizer fin $ SDL.destroyRenderer renderer
      pure (r, (window, renderer))
  )
  $ arrIO
  $ \((camera, picture), (window, renderer)) -> do
    renderPicture window renderer camera picture
{-# INLINE render #-}

createRenderer :: SDL.Window -> IO SDL.Renderer
createRenderer window = SDL.createRenderer window (-1) SDL.defaultRenderer

renderOnce :: Signal r a Picture -> Signal r a Picture
renderOnce = once 

newtype Picture = Picture
  { objects :: IM.IntMap (IM.IntMap (MQ.MaxPQueue Int PictureAtom))
  }
  deriving (Eq, Show)

data Camera = Camera
  { position :: !(V2 Int),
    viewport :: !(V2 Int)
  }
  deriving (Eq, Show, Generic)

data PictureAtom
  = Rectangle !(V2 Int) !(V2 Int) !(AlphaColour Float)
  deriving (Eq, Show)

makePicture :: Int -> PictureAtom -> Picture
makePicture zIndex pictureAtom@(Rectangle position size _) =
  let boxes = boundingBoxes position size
   in foldMap insertBoundingBox boxes
  where
    insertBoundingBox (V2 h v) = Picture $ IM.singleton h $ IM.singleton v $ MQ.singleton zIndex pictureAtom

instance Semigroup Picture where
  (Picture objects1) <> (Picture objects2) =
    Picture $
      IM.unionWith
        (IM.unionWith MQ.union)
        objects1
        objects2

instance Monoid Picture where
  mempty = Picture IM.empty

-- Small bug: Will render objects multiple times if they are in multiple bounding boxes in the spatial graph
renderPicture :: SDL.Window -> SDL.Renderer -> Camera -> Picture -> IO ()
renderPicture window renderer camera@(Camera position viewport) (Picture objects) = do
  let boxes = boundingBoxes position viewport
  windowSize <- SDL.get $ SDL.windowSize window

  SDL.rendererDrawColor renderer SDL.$= V4 255 255 255 255
  SDL.clear renderer
  SDL.rendererScale renderer SDL.$= realToFrac @Float <$> ((fromIntegral <$> windowSize) / (fromIntegral <$> viewport))
  
  forM_ boxes $ \(V2 h v) ->
    case IM.lookup h objects of
      Just innerObjects -> case IM.lookup v innerObjects of
        Just atoms -> forM_ atoms $ renderAtom renderer camera
        Nothing -> pure ()
      Nothing -> pure ()
  
  SDL.present renderer


renderAtom :: SDL.Renderer -> Camera -> PictureAtom -> IO ()
renderAtom renderer camera atom = do
  case atom of
    Rectangle position size colour -> do
      let drawPosition = adjustPosition camera position
      SDL.rendererDrawColor renderer SDL.$= colourToV4 colour
      SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ fromIntegral <$> drawPosition) (fromIntegral <$> size)
      pure ()
  pure ()

adjustPosition :: Camera -> V2 Int -> V2 Int
adjustPosition (Camera (V2 cx cy) (V2 w h)) (V2 x y) =
  V2 (x + (w `quot` 2) - cx) ((h `quot` 2) - (y - cy))

colourToV4 :: AlphaColour Float -> V4 Word8
colourToV4 colour =
  let (RGB r g b) = toSRGB24 (colour `over` black)
      alpha = truncate $ alphaChannel colour * 255
   in V4 r g b alpha

boundingBoxSize :: V2 Int
boundingBoxSize = pure 2048

positionToBoundingBox :: V2 Int -> V2 Int
positionToBoundingBox position =
  let fractionalBox = liftA2 (%) (position + ((`quot` 2) <$> boundingBoxSize)) boundingBoxSize
   in floor <$> fractionalBox

boundingBoxes :: V2 Int -> V2 Int -> [V2 Int]
boundingBoxes position size =
  let halfSize = (`quot` 2) <$> size
      (V2 left bottom) = positionToBoundingBox $ position - halfSize
      (V2 right top) = positionToBoundingBox $ position + halfSize
   in sequenceA $ V2 [left .. right] [bottom .. top]
