
{-|
This module defines the logic of the game and the communication with the `Board.RenderState`
-}
module Snake where

import RenderState (BoardInfo (..), Point, DeltaBoard)
import qualified RenderState as Board
import Data.Sequence ( Seq(..))
import qualified Data.Sequence as S
import System.Random ( uniformR, RandomGen(split), StdGen, Random (randomR) )
import Data.Maybe (isJust)

-- | The Snake can move North, South, East, West
data Movement = North | South | East | West deriving (Show, Eq)

-- | The snake itself is represented as a Point representing the head and Seq Point representing the body
-- Notice that representing the Snake as an array would be wrong since the empty array is an invalid state.
-- Hence the snake head must be explicit
data SnakeSeq = SnakeSeq {snakeHead :: Point, snakeBody :: Seq Point} deriving (Show, Eq)

-- | The state containing the logic of the game. 
data AppState = AppState
  { snakeSeq :: SnakeSeq    -- ^ The snake
  , applePosition :: Point  -- ^ The position of the Apple
  , movement :: Movement    -- ^ The current direction the snake is moving
  , boardInfo :: BoardInfo  -- ^ The dimension of the board
  , randomGen :: StdGen     -- ^ A random seed to purely generate Apples. This is quite different from other languages, and our restrictions of not using monads force us to keep track of the seed
  }
  deriving (Show, Eq)

-- | Calculate the Opposite movement. This is convenient since the snake can't change its movement to the opposite directly
opositeMovement :: Movement -> Movement
opositeMovement North = South
opositeMovement South = North
opositeMovement East = West
opositeMovement West = East

-- | Check if a point is in the snake
inSnake :: Point -> SnakeSeq  -> Bool
inSnake x0 (SnakeSeq x1 seq) = x0 == x1 || isJust (x0 `S.elemIndexL` seq)

-- | Calculates de new head of the snake considering the board limit and the toroidal topology
nextHead :: AppState -> Point
nextHead (AppState (SnakeSeq (x, y) _) _ mov (BoardInfo h w) _) =
  case mov of
    North -> if x - 1 <= 0 then (h, y) else (x - 1, y)
    South -> if x + 1  > h then (1, y) else (x + 1, y)
    East  -> if y + 1  > w then (x, 1) else (x, y + 1)
    West  -> if y - 1 <= 0 then (x, w) else (x, y - 1)

-- | Purely creates a random point within the board limits. The return type is the random point an a new random generator.
makeRandomPoint :: BoardInfo -> StdGen -> (Point, StdGen)
makeRandomPoint (BoardInfo n i) sg = (newPoint , g1')
  where (g1, g2)  = split sg
        (n', g1') = uniformR (1, n) g1
        (i', _) = uniformR (1, i) g2
        newPoint  = (n', i')

-- | Calculates a new random apple, avoiding creating the apple in the same place, or in the snake body
newApple :: AppState -> (Point, StdGen)
newApple app@(AppState ss x0 move bi sg) =
    if x0' == x0 || x0' `inSnake` ss
      then newApple app{randomGen = sg'}
      else (x0', sg')
  where (x0', sg') = makeRandomPoint bi sg

-- | The logic of the movement. The return type is the updated state and a iterator of messages which are passed to the `Board.RenderState` 

-- A brief explanation of the logic.
--    If there is a colision of the snake with itself, the state remains unaltered and a `Board.GameOver` message is sent to the Renderer.
--    If there is no colision, then every move of the snake pushes the head and the body foward. If you think carefully most of the
--    body remains unchange: only the last square becomes and Empty cell, the old head becomes a part of the body and the new head replace an Empty cell
--    This is a little bit different if the snake get the apple or if the snake has no body at all.
move :: AppState -> (AppState, [Board.RenderMessage])
move s@(AppState (SnakeSeq oldHead sb) applePos _ _ g) =
  if isColision
    then (s, [Board.GameOver])
    else
      case isEatingApple of
        True ->
          case sb of
            S.Empty ->
              let newSnake = SnakeSeq newHead (S.singleton oldHead)
                  newState = s {snakeSeq = newSnake, applePosition = newApplePos, randomGen = g'}
                  delta = [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (newApplePos, Board.Apple)]
              in (newState,  [Board.RenderBoard delta, Board.Score])
            xs ->
              let newSnake = SnakeSeq newHead (oldHead :<| xs)
                  newState = s {snakeSeq = newSnake, applePosition = newApplePos, randomGen = g'}
                  delta = [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (newApplePos, Board.Apple)]
              in (newState, [Board.RenderBoard delta, Board.Score])
        False ->
          case sb of
            S.Empty ->
              let newSnake = SnakeSeq newHead S.empty
                  newState = s {snakeSeq = newSnake}
                  delta = [(newHead, Board.SnakeHead), (oldHead, Board.Empty)]
              in (newState, [Board.RenderBoard delta])
            x :<| S.Empty  ->
              let newSnake = SnakeSeq newHead (S.singleton oldHead)
                  newState = s {snakeSeq = newSnake}
                  delta = [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (x, Board.Empty)]
              in (newState, [Board.RenderBoard delta])
            x :<| (xs :|> t)  ->
              let newSnake = SnakeSeq newHead (oldHead :<| x :<| xs)
                  newState = s {snakeSeq = newSnake}
                  delta = [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (t, Board.Empty)]
              in (newState, [Board.RenderBoard delta])

  where newHead           = nextHead s
        isColision        = newHead `elem` sb
        isEatingApple     = newHead == applePos
        (newApplePos, g') = newApple s