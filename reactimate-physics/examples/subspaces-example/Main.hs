{-# LANGUAGE OverloadedStrings #-}

import Data.Bool (bool)
import Data.Colour.Names (black, blue, red)
import Data.Vector.Storable qualified as VS
import Reactimate
import Reactimate.Game
import Reactimate.Physics2D

main :: IO ()
main = runSetup $ reactimate $ runGame "Physics example" defaultWindow 60 $ do
  mapEffects (runPhysics mempty) $
    withSetup (getHandle @Physics >>= \physics -> prestep $ writeVar physics.gravity (V2 0 (-200))) $ \_ ->
      actionStep (\(Handle physics) -> stepPhysics physics (1 / 60))
        >>> rSwitch setupFallingBodies (const $ setupFallingBodies)
        >>> render
        >>> renderGame
        >>> bool Nothing (Just ())
        <$> gameShouldQuit

render :: Signal es (V2 Double, V2 Double) (Camera, Picture)
render =
  arr $ \(pos1, pos2) ->
    ( Camera (V2 0 0) (V2 800 600),
      makePicture 0 $ do
        drawRectangle (packColour red) $ Rectangle (round <$> pos1 - V2 50 50) (V2 100 100)
        drawRectangle (packColour blue) $ Rectangle (round <$> pos2 - V2 50 50) (V2 100 100)
    )

-- As `setupFallingBodies` is switched out, the corresponding `Space` with its bodies is also removed from the whole physics simulation.
setupFallingBodies :: (Physics :> es) => Signal es a ((V2 Double, V2 Double), Maybe ())
setupFallingBodies = makeSignal $ do
  space <- getSpace
  (body1, body2) <- prestep $ addBodies space (V2 400 500)
  pure $ \_ -> do
    pos1@(V2 _ y1) <- readVar body1.position
    pos2@(V2 _ y2) <- readVar body2.position
    pure ((pos1, pos2), if y1 < 0 || y2 < 0 then Just () else Nothing)
  where
    addBodies :: Space s -> V2 Double -> Step s (Body s, Body s)
    addBodies space pos = do
      body1 <- addDynamicBody space 1 (1 / 0)
      writeVar body1.position (pos + V2 0 100)
      shape1 <- addBoxShape body1 (V2 100 100) 0

      body2 <- addDynamicBody space 1 (1 / 0)
      writeVar body2.position (pos)
      shape2 <- addBoxShape body2 (V2 100 100) 0

      pure (body1, body2)
