module Reactimate.Game.Setup
  ( setupGame,
    GameConfig (..),
    SDL.WindowConfig (..),
    SDL.defaultWindow,
  )
where

import Control.Concurrent (threadDelay)
import Data.IORef
import Data.Text (Text)
import GHC.Generics (Generic)
import Reactimate
import Reactimate.Game.Assets (makeAssets)
import Reactimate.Game.Environment
import Reactimate.Signal (Signal (..), unSignal)
import Reactimate.Time (Time (..))
import SDL qualified

data GameConfig = GameConfig
  { name :: !Text,
    window :: !SDL.WindowConfig,
    fps :: !Int
  }
  deriving (Eq, Show, Generic)

-- | Initializes the game environment and provides you the `GameEnv`. You will need this for rendering and grabbing input.
setupGame :: GameConfig -> (GameEnv -> Signal a b) -> Signal a b
setupGame config signal = Signal $ \fin -> do
  SDL.initializeAll
  window <- SDL.createWindow config.name config.window
  addFinalizer fin $ SDL.destroyWindow window

  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  addFinalizer fin $ SDL.destroyRenderer renderer

  assets <- makeAssets

  !buildTime <- SDL.time
  dTimeRef <- newIORef 0
  cTimeRef <- newIORef buildTime

  wantedTimeRef <- newIORef buildTime

  let gameEnv = GameEnv window renderer assets (Time dTimeRef cTimeRef)

  f <- unSignal (signal gameEnv) fin

  pure $ \a -> do
    SDL.pumpEvents
    oldTime <- readIORef gameEnv.time.cTime
    !newTime <- SDL.time
    let !dTime = newTime - oldTime
    writeIORef gameEnv.time.cTime newTime
    writeIORef gameEnv.time.dTime dTime

    b <- f a

    wantedTime <- readIORef wantedTimeRef
    threadDelay $ round $ min frameTime (max 0 $ wantedTime - newTime) * 10 ^ (6 :: Int)

    writeIORef wantedTimeRef (max newTime $ wantedTime + frameTime)

    pure b
  where
    frameTime = 1 / fromIntegral config.fps
