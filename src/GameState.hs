{-# LANGUAGE MultiWayIf #-}
{-|
This module defines the logic of the game and the communication with the `Board.RenderState`
-}
module GameState where

import RenderState (BoardInfo (..), Point, DeltaBoard)
import qualified RenderState as Board
import Data.Sequence ( Seq(..))
import qualified Data.Sequence as S
import System.Random ( uniformR, RandomGen(split), StdGen, Random (randomR), mkStdGen )
import Data.Maybe (isJust)

data Movement = North | South | East | West deriving (Show, Eq)
data SnakeSeq = SnakeSeq {snakeHead :: Point, snakeBody :: Seq Point} deriving (Show, Eq)
data GameState = GameState
  { snakeSeq :: SnakeSeq
  , applePosition :: Point
  , movement :: Movement
  --, boardInfo :: BoardInfo
  , randomGen :: StdGen
  }
  deriving (Show, Eq)

-- | calculate the oposite movement. This is done because if snake is moving up
-- We can not change direction to south.
opositeMovement :: Movement -> Movement
opositeMovement North = South
opositeMovement South = North
opositeMovement East = West
opositeMovement West = East

-- | Purely creates a random point within the board limits
makeRandomPoint :: BoardInfo -> GameState -> (Point, GameState)
makeRandomPoint (BoardInfo n i) gstate = (newPoint , gstate{randomGen = g1'})
  where (g1, g2)  = split (randomGen gstate)
        (n', g1') = uniformR (1, n) g1
        (i', _) = uniformR (1, i) g2
        newPoint  = (n', i')

-- | Check if a point is in the snake
inSnake :: Point -> SnakeSeq  -> Bool
inSnake x0 (SnakeSeq x1 seq) = x0 == x1 || isJust (x0 `S.elemIndexL` seq)

-- | Calculates de new head of the snake
nextHead :: BoardInfo -> GameState -> Point
nextHead (BoardInfo h w) (GameState (SnakeSeq (x, y) _) _ mov _) =
  case mov of
    North -> if x - 1 <= 0 then (h, y) else (x - 1, y)
    South -> if x + 1  > h then (1, y) else (x + 1, y)
    East  -> if y + 1  > w then (x, 1) else (x, y + 1)
    West  -> if y - 1 <= 0 then (x, w) else (x, y - 1)

-- | Calculates a new random apple, avoiding creating the apple in the same place, or in the snake body
newApple :: BoardInfo -> GameState -> (Point, GameState)
newApple bi gstate@(GameState ss old_apple move sg) =
    if new_apple == old_apple || new_apple `inSnake` ss
      then newApple bi gstate'
      else (new_apple, gstate'{applePosition = new_apple})
  where (new_apple, gstate') = makeRandomPoint bi gstate

-- | move the snake's head forward without removing the tail. (This is the case of eating an apple)
extendSnake ::  Point -> BoardInfo -> GameState -> (DeltaBoard, GameState)
extendSnake new_head binfo gstate@(GameState (SnakeSeq old_head snake_body) _ _ _) = (delta, gstate {snakeSeq = new_snake})
 where new_snake = SnakeSeq new_head (old_head :<| snake_body)
       delta     = [(new_head, Board.SnakeHead), (old_head, Board.Snake)]

-- | displace snake, that is: remove the tail and move the head forward (This is the case of eating an apple)
displaceSnake :: Point -> BoardInfo -> GameState -> (DeltaBoard, GameState)
displaceSnake new_head binfo gstate@(GameState (SnakeSeq old_head snake_body) apple movement sg) =
  case snake_body of
    S.Empty -> let new_snake = SnakeSeq new_head S.empty
                   delta = [(new_head, Board.SnakeHead), (old_head, Board.Empty)]
                in (delta , gstate {snakeSeq = new_snake})
    xs :|> t -> let new_snake = SnakeSeq new_head (old_head :<| xs)
                    delta = [(new_head, Board.SnakeHead), (old_head, Board.Snake), (t, Board.Empty)]
                 in (delta, gstate {snakeSeq = new_snake})

-- | Moves the snake based on the current direction.
move :: BoardInfo -> GameState -> ([Board.RenderMessage], GameState)
move bi gstate@(GameState s applePos _ _) =
  if | isColision -> ([Board.GameOver], gstate)
     | isEatingApple -> let (delta, gstate') = extendSnake newHead bi gstate
                            (newApplePos, gstate'') = newApple bi gstate'
                            delta' = (newApplePos, Board.Apple):delta
                         in ([Board.RenderBoard delta', Board.Score], gstate'')
     | otherwise -> let (delta, gstate') = displaceSnake newHead bi gstate 
                     in ([Board.RenderBoard delta], gstate')
  where newHead           = nextHead bi gstate
        isColision        = newHead `inSnake` s
        isEatingApple     = newHead == applePos