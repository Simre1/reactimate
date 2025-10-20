import Data.Bool
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
      runSetup $
        reactimate $
          runGame (pack "Image Example") defaultWindow 60 $
            runAssets $
              game (pack imagePath) >>> renderGame >>> bool Nothing (Just ()) <$> gameShouldQuit
    _ -> putStrLn "Run this example with the image path as the argument"
  pure ()

game :: (Graphics :> es, IOE :> es, Assets :> es) => Text -> Signal es () (Camera, Picture)
game imagePath = withImage imagePath $ \image ->
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
