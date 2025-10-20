{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Colour.Names (red)
import Data.Colour.SRGB (sRGB24)
import Data.List (group)
import Reactimate
import Reactimate.Game
import SDL qualified

name = "Snake"

window = defaultWindow {windowInitialSize = fromIntegral . (* 30) <$> gameBounds}

fps = 11

main :: IO ()
main =
  runSetup $
    reactimate $
      runGame name window fps $
        captureInput >>> (feedback initialGameState stepGame >>> render) &&& shouldQuit >>> arr snd

data GameInput = MoveLeft | MoveRight | MoveUp | MoveDown | NoAction | Quit deriving (Eq, Ord, Show)

data GameState = GameState
  { snake :: [V2 Int],
    direction :: V2 Int,
    food :: Maybe (V2 Int),
    running :: Bool,
    score :: Int
  }

captureInput :: (Input :> es) => Signal es () GameInput
captureInput =
  inputEvents
    >>> arr (foldl' collectInput [])
    >>> feedback [] (arr $ \(newActions, oldActions) -> fmap head $ group $ drop 1 oldActions ++ newActions)
    >>> arr
      ( \case
          (i : _) -> i
          [] -> NoAction
      )
  where
    collectInput [Quit] _ = [Quit]
    collectInput is event = case SDL.eventPayload event of
      SDL.QuitEvent -> [Quit]
      SDL.KeyboardEvent keyboardEvent ->
        if SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
          then case SDL.keysymScancode $ SDL.keyboardEventKeysym keyboardEvent of
            SDL.ScancodeLeft -> is ++ [MoveLeft]
            SDL.ScancodeRight -> is ++ [MoveRight]
            SDL.ScancodeUp -> is ++ [MoveUp]
            SDL.ScancodeDown -> is ++ [MoveDown]
            _ -> is
          else is
      _ -> is

initialGameState :: GameState
initialGameState =
  GameState
    { snake = [V2 5 5],
      direction = V2 1 0,
      food = Just (V2 10 10),
      running = False,
      score = 0
    }

gameBounds :: V2 Int
gameBounds = V2 32 18

stepGame :: (IOE :> es) => Signal es (GameInput, GameState) GameState
stepGame =
  runRNGWithIO $
    generateRandomRange (V2 1 1, gameBounds - V2 2 2)
      &&& identity
      >>> arrIO
        ( \(rngV2, (input, GameState snake direction food running score)) ->
            if running
              then
                let nextDirection = adaptDirection input direction
                    nextHead = nextDirection + head snake
                    snakeEats = Just nextHead == food
                    nextSnake =
                      if snakeEats || length snake == 1
                        then nextHead : snake
                        else nextHead : init snake
                    nextFood = if snakeEats then Just rngV2 else food
                    nextScore = if snakeEats then score + 1 else score
                 in if isDead nextSnake
                      then do
                        putStrLn $ "Score: " ++ show score
                        pure $ GameState nextSnake nextDirection nextFood False nextScore
                      else pure $ GameState nextSnake nextDirection nextFood True nextScore
              else
                if shouldStartGame input
                  then pure $ initialGameState {running = True}
                  else pure $ GameState snake direction food False score
        )
  where
    shouldStartGame i = case i of
      MoveLeft -> True
      MoveRight -> True
      MoveUp -> True
      MoveDown -> True
      _ -> False
    isDead snake =
      let snakeHead@(V2 x y) = head snake
          (V2 maxX maxY) = gameBounds
       in snakeHead `elem` tail snake || x < 0 || x >= maxX || y < 0 || y >= maxY
    adaptDirection i d =
      let wantedDirection = case i of
            MoveLeft -> V2 (-1) 0
            MoveRight -> V2 1 0
            MoveUp -> V2 0 1
            MoveDown -> V2 0 (-1)
            _ -> d
       in if wantedDirection + d == V2 0 0 then d else wantedDirection

render :: (Graphics :> es) => Signal es GameState ()
render = arr (\gs -> (camera, gameStatePicture gs)) >>> renderGame
  where
    gameStatePicture (GameState snake _ food _ _) = makePicture 0 $ do
      zipWithM_ snakeShape [0 ..] snake
      drawFood food
    snakeShape (i' :: Int) pos = do
      let i = fromIntegral $ abs $ (i' `mod` 160) - 80
      drawRectangle (packColour $ sRGB24 0 (255 - i * 2) (i * 3)) (Rectangle pos (V2 1 1))
    drawFood = maybe mempty $ \pos -> drawRectangle (packColour red) (Rectangle pos (V2 1 1))
    camera = Camera (V2 0 0) gameBounds

shouldQuit :: Signal es GameInput (Maybe ())
shouldQuit = arr $ \i -> if i == Quit then Just () else Nothing
