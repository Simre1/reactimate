{-# LANGUAGE OverloadedStrings #-}

import Data.Bool (bool)
import Data.Colour.Names (black, blue, red)
import Data.Vector.Storable qualified as VS
import Reactimate
import Reactimate.Physics2D
import Reactimate.Game

main :: IO ()
main = reactimate $ setupGame (GameConfig "Physics example" defaultWindow 60) $ \gameEnv ->
  withPhysics $ \space ->
    withSetup_ (space.gravity $= V2 0 (-200)) $
      actionIO (spaceStep space (1 / 60))
        >>> switchRepeatedly (setupFallingBodies space) (const $ setupFallingBodies space)
        >>> render
        >>> renderGame gameEnv
        >>> bool Nothing (Just ())
        <$> sampleBehavior (gameShouldQuit gameEnv)

render :: Signal (V2 Double, V2 Double) (Camera, Picture)
render =
  arr $ \(pos1, pos2) ->
    ( Camera (V2 0 0) (V2 800 600),
      makePicture 0 $ do
        drawRectangle (packColour red) $ Rectangle (round <$> pos1 - V2 50 50) (V2 100 100)
        drawRectangle (packColour blue) $ Rectangle (round <$> pos2 - V2 50 50) (V2 100 100)
    )

-- As `setupFallingBodies` is switched out, the corresponding `Subspace` with its bodies is also removed from the `Space`.
setupFallingBodies :: Space -> Signal a ((V2 Double, V2 Double), Maybe ())
setupFallingBodies space = withSubspace space $ \subspace -> withSetup (addBodies subspace (V2 400 500)) $ \(body1, body2) ->
  arrIO
    ( \_ -> do
        pos1@(V2 _ y1) <- get body1.position
        pos2@(V2 _ y2) <- get body2.position
        pure ((pos1, pos2), if y1 < 0 || y2 < 0 then Just () else Nothing)
    )
  where
    addBodies :: Subspace -> V2 Double -> IO (Body, Body)
    addBodies space pos = do
      body1 <- addDynamicBody space 1 (1 / 0)
      body1.position $= pos + V2 0 100
      shape1 <- addBoxShape body1 (V2 100 100) 0
      shape1.friction $= 0.3

      body2 <- addDynamicBody space 1 (1 / 0)
      body2.position $= pos
      shape2 <- addBoxShape body2 (V2 100 100) 0
      shape2.friction $= 0.3

      pure (body1, body2)
