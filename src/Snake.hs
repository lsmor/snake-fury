{-# LANGUAGE ViewPatterns, PatternGuards #-}

module Snake where

import Board (BoardInfo (..), Point, DeltaBoard)
import qualified Board
import Data.Sequence ( Seq(..))
import qualified Data.Sequence as S
import System.Random ( uniformR, RandomGen(split), StdGen, Random (randomR) )
import Data.Maybe (isJust)

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

-- Purely creates a random point within the board limits
makeRandomPoint :: BoardInfo -> StdGen -> (Point, StdGen)
makeRandomPoint (BoardInfo n i) sg = (newPoint , g1')
  where (g1, g2)  = split sg
        (n', g1') = uniformR (1, n) g1
        (i', _) = uniformR (1, i) g2
        newPoint  = (n', i')

-- Check if a point is in the snake
inSnake :: Point -> SnakeSeq  -> Bool
inSnake x0 (SnakeSeq x1 seq) = x0 == x1 || isJust (x0 `S.elemIndexL` seq)

-- Calcualates de new head of the snake
nextHead :: AppState -> Point
nextHead (AppState (SnakeSeq (x, y) _) _ mov (BoardInfo h w) _) =
  case mov of
    North -> if x - 1 <= 0 then (h, y) else (x - 1, y)
    South -> if x + 1  > h then (1, y) else (x + 1, y)
    East  -> if y + 1  > w then (x, 1) else (x, y + 1)
    West  -> if y - 1 <= 0 then (x, w) else (x, y - 1)

-- Calculates a new random apple, avoiding creating the apple in the same place, or in the snake body
newApple :: AppState -> (Point, StdGen)
newApple app@(AppState ss x0 move bi sg) =
    if x0' == x0 || x0' `inSnake` ss
      then newApple app{randomGen = sg'}
      else (x0', sg')
  where (x0', sg') = makeRandomPoint bi sg


-- Actually all patterns are covered but HLS says no...
move :: AppState -> (AppState, DeltaBoard)
move s@(AppState (SnakeSeq oldHead sb) applePos _ _ g) =
  case isEatingApple of
    True ->
      case sb of
        S.Empty ->
          let newSnake = SnakeSeq newHead (S.singleton oldHead)
              newState = s {snakeSeq = newSnake, applePosition = newApplePos, randomGen = g'}
              delta = [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (newApplePos, Board.Apple)]
           in (newState,  delta)
        xs ->
          let newSnake = SnakeSeq newHead (oldHead :<| xs)
              newState = s {snakeSeq = newSnake, applePosition = newApplePos, randomGen = g'}
              delta = [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (newApplePos, Board.Apple)]
           in (newState,  delta)
    False ->
      case sb of
        S.Empty ->
          let newSnake = SnakeSeq newHead S.empty
              newState = s {snakeSeq = newSnake}
              delta = [(newHead, Board.SnakeHead), (oldHead, Board.Empty)]
           in (newState,  delta)
        x :<| S.Empty  ->
          let newSnake = SnakeSeq newHead (S.singleton oldHead)
              newState = s {snakeSeq = newSnake}
              delta = [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (x, Board.Empty)]
           in (newState,  delta)
        x :<| (xs :|> t)  ->
          let newSnake = SnakeSeq newHead (oldHead :<| x :<| xs)
              newState = s {snakeSeq = newSnake}
              delta = [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (t, Board.Empty)]
           in (newState,  delta)
  where newHead           = nextHead s
        isEatingApple     = newHead == applePos
        (newApplePos, g') = newApple s

ppAppState :: AppState -> String
ppAppState (AppState ss x0 move bi sg) = "snake: " <> show ss <> "\n apple: " <> show x0 <> "\n mov: " <> show move <> "\n binfo: " <> show bi
