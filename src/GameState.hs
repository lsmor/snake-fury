{-|
This module defines the logic of the game and the communication with the `Board.RenderState`
-}
module GameState where 

import RenderState (BoardInfo (..), Point, DeltaBoard)
import qualified RenderState as Board
import Data.Sequence ( Seq(..))
import qualified Data.Sequence as S
import System.Random ( uniformR, RandomGen(split), StdGen, Random (randomR) )
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
makeRandomPoint :: BoardInfo -> StdGen -> (Point, StdGen)
makeRandomPoint (BoardInfo n i) sg = (newPoint , g1')
  where (g1, g2)  = split sg
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
newApple :: BoardInfo -> GameState -> (Point, StdGen)
newApple bi app@(GameState ss x0 move sg) =
    if x0' == x0 || x0' `inSnake` ss
      then newApple bi app{randomGen = sg'}
      else (x0', sg')
  where (x0', sg') = makeRandomPoint bi sg


-- | Moves the snake based on the current direction.
move :: BoardInfo -> GameState -> ([Board.RenderMessage], GameState)
move bi s@(GameState (SnakeSeq oldHead sb) applePos _ g) =
  if isColision
    then ([Board.GameOver], s)
    else 
      case isEatingApple of
        True ->
          case sb of
            S.Empty ->
              let newSnake = SnakeSeq newHead (S.singleton oldHead)
                  newState = s {snakeSeq = newSnake, applePosition = newApplePos, randomGen = g'}
                  delta = [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (newApplePos, Board.Apple)]
              in ([Board.RenderBoard delta, Board.Score], newState)
            xs ->
              let newSnake = SnakeSeq newHead (oldHead :<| xs)
                  newState = s {snakeSeq = newSnake, applePosition = newApplePos, randomGen = g'}
                  delta = [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (newApplePos, Board.Apple)]
              in ([Board.RenderBoard delta, Board.Score], newState)
        False ->
          case sb of
            S.Empty ->
              let newSnake = SnakeSeq newHead S.empty
                  newState = s {snakeSeq = newSnake}
                  delta = [(newHead, Board.SnakeHead), (oldHead, Board.Empty)]
              in ([Board.RenderBoard delta], newState)
            x :<| S.Empty  ->
              let newSnake = SnakeSeq newHead (S.singleton oldHead)
                  newState = s {snakeSeq = newSnake}
                  delta = [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (x, Board.Empty)]
              in ([Board.RenderBoard delta], newState)
            x :<| (xs :|> t)  ->
              let newSnake = SnakeSeq newHead (oldHead :<| x :<| xs)
                  newState = s {snakeSeq = newSnake}
                  delta = [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (t, Board.Empty)]
              in ([Board.RenderBoard delta], newState)

  where newHead           = nextHead bi s
        isColision        = newHead `elem` sb
        isEatingApple     = newHead == applePos
        (newApplePos, g') = newApple bi s