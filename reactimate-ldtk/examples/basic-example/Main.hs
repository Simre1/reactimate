{-# LANGUAGE OverloadedStrings #-}

import Data.Bool (bool)
import Data.Colour.Names
import Data.Vector.Storable qualified as VS
import Reactimate
import Reactimate.Game
import Reactimate.LDtk
import System.Environment (getArgs)
import Debug.Trace

gameConfig :: GameConfig
gameConfig = GameConfig "ldtk example" defaultWindow {windowInitialSize = V2 512 512} 60

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ldtkPath] ->
      reactimate $ setupGame gameConfig $ \gameEnv ->
        renderLDtk ldtkPath
          >>> render gameEnv
          >>> sampleBehavior (bool Nothing (Just ()) <$> gameShouldQuit gameEnv)
    _ -> putStrLn "Run this example with the basic example ldtk file path as the argument"

render :: GameEnv -> Signal Picture ()
render gameEnv = arr (Camera (V2 0 0) (V2 256 256),) >>> renderGame gameEnv

renderLDtk :: FilePath -> Signal () Picture
renderLDtk filepath = withLDtkRoot filepath $ \ldtkRoot -> withLevel ldtkRoot "Level_0" $ \level ->
  constant (withMatchRules rules level) >>> arr (,Nothing)
  where
    rules :: [MatchRule Picture]
    rules = pure $ matchEntity Nothing $ do
      entitySize <- getEntitySize
      entityPosition <- getEntityPosition
      pure $ makePicture 0 $
        drawRectangle (packColour red) $ Rectangle entityPosition entitySize
