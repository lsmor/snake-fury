{-# LANGUAGE ViewPatterns, PatternGuards #-}

module Snake where

import Board (BoardInfo (..), Point, DeltaBoard)
import qualified Board
import Data.Sequence ( Seq(..))
import qualified Data.Sequence as S
import System.Random

data Movement = North | South | East | West deriving (Show, Eq)
data SnakeSeq = SnakeSeq {snakeHead :: Point, snakeBody :: Seq Point}
data AppState = AppState
  { snakeSeq :: SnakeSeq
  , applePosition :: Point
  , movement :: Movement
  , boardInfo :: BoardInfo
  , randomGen :: StdGen
  }


nextHead :: AppState -> Point
nextHead (AppState (SnakeSeq (x, y) _) _ mov (BoardInfo h w) _) =
  case mov of
    North -> if x - 1 <= 0 then (h, y) else (x - 1, y)
    South -> if x + 1  > h then (1, y) else (x + 1, y)
    East  -> if y + 1  > w then (x, 1) else (x, y + 1)
    West  -> if y - 1 <= 0 then (x, w) else (x, y - 1)

-- Actually all patterns are covered but HLS says no...
move :: AppState -> AppState
move s@(AppState (SnakeSeq h sb) applePos _ _ _) =
  case sb of
    S.Empty          | isEatingApple     -> s {snakeSeq = SnakeSeq n (S.singleton n)}
    S.Empty          | not isEatingApple -> s {snakeSeq = SnakeSeq n S.Empty}
    x :<| S.Empty    | not isEatingApple -> s {snakeSeq = SnakeSeq n (S.singleton h)}
    x :<| (xs :|> t) | not isEatingApple -> s {snakeSeq = SnakeSeq n (h :<| xs)}
    xs               | isEatingApple     -> s {snakeSeq = SnakeSeq n xs}
  where n = nextHead s
        isEatingApple = n == applePos

