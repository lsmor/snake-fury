{-# LANGUAGE ViewPatterns, PatternGuards #-}

module Snake where

import Board (BoardInfo (..), Point, DeltaBoard)
import qualified Board
import Data.Sequence ( Seq(..))
import qualified Data.Sequence as S
import System.Random
import Debug.Trace(trace)

data Movement = North | South | East | West deriving (Show, Eq)
data SnakeSeq = SnakeSeq {snakeHead :: Point, snakeBody :: Seq Point} deriving (Show, Eq)
data AppState = AppState
  { snakeSeq :: SnakeSeq
  , applePosition :: Point
  , movement :: Movement
  , boardInfo :: BoardInfo
  , randomGen :: StdGen
  }
  deriving (Show, Eq)

-- Calcualates de new head of the snake
nextHead :: AppState -> Point
nextHead (AppState (SnakeSeq (x, y) _) _ mov (BoardInfo h w) _) =
  case mov of
    North -> if x - 1 <= 0 then (h, y) else (x - 1, y)
    South -> if x + 1  > h then (1, y) else (x + 1, y)
    East  -> if y + 1  > w then (x, 1) else (x, y + 1)
    West  -> if y - 1 <= 0 then (x, w) else (x, y - 1)

-- Calculates a new random apple
newApple :: AppState -> (Point, StdGen)
newApple (AppState _ _ _ (BoardInfo n i) sg) = ((n', i') , g1')
  where (g1, g2) = split sg
        (n', g1') = uniformR (1, n) g1
        (i', g2') = uniformR (1, i) g2

-- Actually all patterns are covered but HLS says no...
move :: AppState -> (AppState, DeltaBoard)
move s@(AppState (SnakeSeq h sb) applePos _ _ g) =
  case sb of
    S.Empty          | isEatingApple     -> (s {snakeSeq = SnakeSeq n (S.singleton h), applePosition = newApplePos, randomGen = g'},  [(n, Board.Snake), (newApplePos, Board.Apple)])
    S.Empty          | not isEatingApple -> (s {snakeSeq = SnakeSeq n S.Empty}, [(n, Board.Snake), (h, Board.Empty)] )
    x :<| S.Empty    | not isEatingApple -> (s {snakeSeq = SnakeSeq n (S.singleton h)}, [(n, Board.Snake), (x, Board.Empty)] )
    x :<| (xs :|> t) | not isEatingApple -> (s {snakeSeq = SnakeSeq n (h :<| x :<| xs)}, [(n, Board.Snake), (t, Board.Empty)] )
    xs               | isEatingApple     -> (s {snakeSeq = SnakeSeq n (h :<| xs), applePosition = newApplePos, randomGen = g'}, [(n, Board.Snake), (newApplePos, Board.Apple)] )
  where n = nextHead s
        isEatingApple = n == applePos
        (newApplePos, g') = newApple s


ppAppState :: AppState -> String
ppAppState (AppState ss x0 move bi sg) = "snake: " <> show ss <> "\n apple: " <> show x0 <> "\n mov: " <> show move <> "\n binfo: " <> show bi
