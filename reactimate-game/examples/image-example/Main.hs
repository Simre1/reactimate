import Data.Text (Text, pack)
import Data.Vector.Storable qualified as VS
import Reactimate
import Reactimate.Game
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [imagePath] ->
      reactimate $
        setupGame (GameConfig (pack "Basic Example") defaultWindow) $ \gameEnv ->
          limitSampleRate 60 $
            game gameEnv (pack imagePath) >>> render gameEnv >>> constant Nothing
    _ -> putStrLn "Run this example with the image path as the argument"
  pure ()

game :: GameEnv -> Text -> Signal () (Camera, Picture)
game gameEnv imagePath = withImage gameEnv imagePath $ \image ->
  constant
    ( camera,
      makePicture 0 $
        Texture image $
          VS.singleton $
            Blit (Rectangle (V2 0 0) image.size) (Rectangle (V2 200 150) (V2 400 300))
    )

camera :: Camera
camera = Camera (V2 0 0) (V2 800 600)
