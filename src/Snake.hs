module Snake where

import Board (BoardInfo (..), Point, DeltaBoard)
import qualified Board
import Data.Sequence ( Seq )
import qualified Data.Sequence as S


data Movement = North | South | East | West deriving (Show, Eq)
data SnakeSeq = SnakeSeq {snakeHead :: Point, snakeBody :: Seq Point}


nextHead :: SnakeSeq -> Movement -> BoardInfo -> Point
nextHead (SnakeSeq (x, y) _) mov (BoardInfo h w) =
  case mov of
    North -> if x - 1 <= 0 then (h, y) else (x - 1, y)
    South -> if x + 1  > h then (1, y) else (x + 1, y)
    East  -> if y + 1  > w then (x, 1) else (x, y + 1)
    West  -> if y - 1 <= 0 then (x, w) else (x, y - 1)


moveSnake :: SnakeSeq -> Movement -> BoardInfo -> DeltaBoard
moveSnake s@(SnakeSeq x0 S.Empty) m i = undefined
moveSnake s@(SnakeSeq x0 (b1 S.:<| _)) m i = undefined
