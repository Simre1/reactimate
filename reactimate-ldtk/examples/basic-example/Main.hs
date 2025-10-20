{-# LANGUAGE OverloadedStrings #-}

import Data.Bool (bool)
import Data.Colour.Names
import Data.Vector.Storable qualified as VS
import Debug.Trace
import Reactimate
import Reactimate.Game
import Reactimate.LDtk
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ldtkPath] ->
      runSetup $
        reactimate $
          runGame "ldtk example" defaultWindow {windowInitialSize = V2 512 512} 60 $
            renderLDtk ldtkPath
              >>> render
              >>> (bool Nothing (Just ()) <$> gameShouldQuit)
    _ -> putStrLn "Run this example with the basic example ldtk file path as the argument"

render :: (Graphics :> es) => Signal es Picture ()
render = arr (Camera (V2 0 0) (V2 256 256),) >>> renderGame

renderLDtk :: (IOE :> es) => FilePath -> Signal es () Picture
renderLDtk filepath = withLDtkRoot filepath $ \ldtkRoot -> withLevel ldtkRoot "Level_0" $ \level ->
  constant (withMatchRules rules level) >>> arr (,Nothing)
  where
    rules :: [MatchRule Picture]
    rules = pure $ matchEntity Nothing $ do
      entitySize <- getEntitySize
      entityPosition <- getEntityPosition
      pure $
        makePicture 0 $
          drawRectangle (packColour red) $
            Rectangle entityPosition entitySize
