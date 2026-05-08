{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{- HLINT ignore "Use uncurry" -}

module Main where

import Control.Arrow
import Reactimate
import Reactimate.Game
import SDL qualified
import Control.Category
import Prelude hiding ((.))

mySample :: Signal '[IOE] a b -> [a] -> IO [b]
mySample signal values = runSetup $ sample signal values

--
--
--
--
--
--
--
--
--
--
--
--
--------------------------------------
------- PART 0 -----------------------
--------------------------------------

plus10 :: Signal es Int Int
plus10 = arr $ \a -> a + 10

times2 :: Signal es Int Int
times2 = arr $ \a -> a * 2

--- mySample times2 [1,2,3]

add :: Signal es (Int, Int) Int
add = arr $ uncurry (+)

---------------------------------
---------------------------------
---------------------------------
---------------------------------
---------------------------------
---------------------------------
---------------------------------

sumUp :: Signal es Int Int
sumUp = feedback 0 $ arr $ \(a, mySum) -> a + mySum


reactimateExample :: IO Int
reactimateExample = runSetup $ reactimate $
  feedback 0 (arr $ \(_, state) -> state + 1) >>> arrIO (\x -> print x >> pure x) >>> arr (\x -> if x > 10 then Just x else Nothing)

--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--------------------------------------
------- PART 1 -----------------------
--------------------------------------

runGameSignal :: Signal [Input, Time, Graphics, IOE] () Picture -> IO ()
runGameSignal signal =
  runSetup $
    reactimate $
      runGame "example" SDL.defaultWindow {SDL.windowInitialSize = V2 400 400} 30 $
        (arr (const $ Camera (V2 0 0) (V2 100 100)) &&& signal) >>> renderGame >>> arr (const Nothing)

data GameInput = MoveRight | MoveLeft | DoNothing
  deriving (Show)

getGameInput :: (Input :> es) => Signal es () GameInput
getGameInput =
  keyboardState
    >>> arr
      ( \state ->
          if
            | state SDL.ScancodeLeft -> MoveLeft
            | state SDL.ScancodeRight -> MoveRight
            | otherwise -> DoNothing
      )

-- main :: IO ()
-- main = runGameSignal $ getGameInput >>> arr print >>> arr (const mempty)

--------------------------------------
------- PART 2 -----------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------


player :: Signal es GameInput Int
player = feedback 50 $ arr $ \(input, position) ->
  dPosition input + position
  where
    dPosition input = case input of
      MoveLeft -> -1
      MoveRight -> 1
      DoNothing -> 0

-- main :: IO ()
-- main = runGameSignal $ getGameInput >>> player >>> arr print >>> arr (const mempty)
 
--------------------------------------
------- PART 3 -----------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------

renderPlayer :: Signal es Int Picture
renderPlayer = arr $ \pos -> makePicture 0 $ do
  drawRectangle (V4 20 200 20 255) (Rectangle (V2 pos 5) (V2 20 5))

ball :: Signal es () (V2 Int)
ball = feedbackState (V2 50 50, V2 1 1) $ arr $ \((), (position, direction@(V2 dX dY))) ->
  let V2 nextX nextY = position + direction
      nextDx =
        if
          | nextX < 0 -> 1
          | nextX > 95 -> -1
          | otherwise -> dX
      nextDy = if nextY > 95 then (-1) else dY
      nextDirection = V2 nextDx nextDy
      nextPosition = position + nextDirection
   in (nextPosition, (nextPosition, nextDirection))

renderBall :: Signal es (V2 Int) Picture
renderBall = arr $ \pos -> makePicture 0 $ do
  drawRectangle (V4 200 20 20 255) (Rectangle pos (V2 5 5))

-- main :: IO ()
-- main = runGameSignal $ proc () -> do
--   i <- getGameInput -< ()
--   playerPosition <- player -< i
--   playerPicture <- renderPlayer -< playerPosition
--   ballPosition <- ball -< ()
--   ballPicture <- renderBall -< ballPosition
--   returnA -< playerPicture <> ballPicture


--------------------------------------
------- PART 4 -----------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------


ball2 :: Signal es Int (V2 Int)
ball2 = feedbackState (V2 50 50, V2 1 1) $ arr $ \(playerPosition, (position, direction@(V2 dX dY))) ->
  let V2 nextX nextY = position + direction
      nextDx =
        if
          | nextX < 0 -> 1
          | nextX > 95 -> -1
          | otherwise -> dX
      nextDy =
        if
          | nextY > 95 -> -1
          | nextY < 10 && (nextX - playerPosition >= 0 && nextX - playerPosition <= 20) -> 1
          | otherwise -> dY
      nextDirection = V2 nextDx nextDy
      nextPosition = position + nextDirection
   in (nextPosition, (nextPosition, nextDirection))

-- main :: IO ()
-- main = runGameSignal $ proc () -> do
--   i <- getGameInput -< ()
--   playerPosition <- player -< i
--   playerPicture <- renderPlayer -< playerPosition
--   ballPosition <- ball2 -< playerPosition
--   ballPicture <- renderBall -< ballPosition
--   returnA -< playerPicture <> ballPicture


--------------------------------------
------- PART 5 -----------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------

ball3 :: V2 Int -> V2 Int -> Signal es Int (V2 Int)
ball3 initialPosition initialSpeed = feedbackState (initialPosition, initialSpeed) $ arr $ \(playerPosition, (position, direction@(V2 dX dY))) ->
  let V2 nextX nextY = position + direction
      nextDx =
        if
          | nextX < 0 -> 1
          | nextX > 95 -> -1
          | otherwise -> dX
      nextDy =
        if
          | nextY > 95 -> -1
          | nextY < 10 && (nextX - playerPosition >= 0 && nextX - playerPosition <= 20) -> 1
          | otherwise -> dY
      nextDirection = V2 nextDx nextDy
      nextPosition = position + nextDirection
   in (nextPosition, (nextPosition, nextDirection))


-- main :: IO ()
-- main = runGameSignal $ proc () -> do
--   i <- getGameInput -< ()
--   playerPosition <- player -< i
--   playerPicture <- renderPlayer -< playerPosition
--   ballPicture1 <- renderBall <<< ball3 (V2 10 60) (V2 1 1) -< playerPosition
--   ballPicture2 <- renderBall <<< ball3 (V2 10 30) (V2 (-1) 1) -< playerPosition
--   returnA -< playerPicture <> ballPicture1 <> ballPicture2


--------------------------------------
------- PART 6 -----------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------

game :: Input :> es => Signal es () (Picture, Maybe ())
game = proc () -> do
  i <- getGameInput -< ()
  playerPosition <- player -< i
  playerPicture <- renderPlayer -< playerPosition
  ballPosition1@(V2 _ y1) <- ball3 (V2 10 60) (V2 1 1) -< playerPosition
  ballPosition2@(V2 _ y2) <- ball3 (V2 10 30) (V2 (-1) 1) -< playerPosition
  ballPicture1 <- renderBall -< ballPosition1
  ballPicture2 <- renderBall -< ballPosition2
  returnA -< (playerPicture <> ballPicture1 <> ballPicture2, if y1 < 0 || y2 < 0 then Just () else Nothing)

-- main :: IO ()
-- main = runGameSignal $ switch game $ \() -> arr (const mempty)
 
main :: IO ()
main = runGameSignal $ rSwitch game $ \() -> game

--------------------------------------
------- PART 7 -----------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------

game2 :: Input :> es => Signal es () (Picture, Maybe ())
game2 = proc () -> do
      input <- getGameInput -< ()
      playerPosition <- player -< input
      playerPicture <- renderPlayer -< playerPosition
      ballPictures <- myConcat ((\x -> uncurry ball3 x >>> renderBall) <$> balls )-< playerPosition
      returnA -< (playerPicture <> mconcat ballPictures , Nothing)

myConcat :: [Signal es a b] -> Signal es a [b]
myConcat [] = arr $ \_ -> []
myConcat (signal:signals) = signal &&& myConcat signals >>> arr (\(a,b) -> a : b)

balls = [ (V2 x y, V2 (signum x - y) 1) | x <- [1..20], y <- [1..20]]

-- main :: IO ()
-- main = runGameSignal $ rSwitch game2 (const game2)

--------------------------------------
------- PART 8 -----------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------


newtype PureSignal a b = PureSignal (a -> (b, PureSignal a b))

pureArr :: (a -> b) -> PureSignal a b
pureArr f = PureSignal $ \a ->
  let b = f a
  in (b, pureArr f)

instance Category PureSignal where
  id = pureArr (\x -> x)
  (PureSignal f2) . (PureSignal f1) = PureSignal $ \a ->
    let (b, nextF1) = f1 a
        (c, nextF2) = f2 b
    in (c, nextF2 . nextF1)

pureFeedback :: b -> PureSignal (a,b) b -> PureSignal a b
pureFeedback initial (PureSignal f) = PureSignal $ \a ->
  let (nextB, nextF) = f (a, initial)
  in (nextB, pureFeedback nextB nextF)


pureSample :: PureSignal a b -> [a] -> [b]
pureSample (PureSignal f) [] = []
pureSample (PureSignal f) (a:as) =
  let (b, nextF) = f a
  in b : pureSample nextF as
