import Data.Text (Text, pack)
import Reactimate
import Reactimate.Game
import Reactimate.Stateful (sumUp)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [imagePath] ->
      reactimate $
        setupGame (GameConfig (pack "Image Example") defaultWindow 60) $ \gameEnv ->
          game gameEnv (pack imagePath) >>> renderGame gameEnv >>> constant Nothing
    _ -> putStrLn "Run this example with the image path as the argument"
  pure ()

game :: GameEnv -> Text -> Signal () (Camera, Picture)
game gameEnv imagePath = withImage gameEnv imagePath $ \image ->
  constant 0.01
    >>> sumUp
    >>> arr
      ( \x ->
          ( camera,
            picture image x
          )
      )
  where
    picture image x =
      translatePicture (V2 400 300) $
        rotatePicture x $
          makePicture 0 $
            blitImage [Blit (Rectangle (V2 0 0) image.size) (Rectangle (V2 (-200) (-150)) (V2 400 300))] image

camera :: Camera
camera = Camera (V2 0 0) (V2 800 600)
