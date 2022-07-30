{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Control.Monad.Reader (ReaderT (runReaderT), ask, runReader, MonadReader, Reader)
import Control.Monad.State.Strict (StateT, get, put, modify, gets, runStateT, MonadState, State, runState)

data Movement = North | South | East | West deriving (Show, Eq)
data SnakeSeq = SnakeSeq {snakeHead :: Point, snakeBody :: Seq Point} deriving (Show, Eq)

-- | The are two kind of events, a `Tick`
--   representing movement which is not force by the user input
--   and `UserEvent` which is the opposite.
data Event = Tick | UserEvent Movement

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
makeRandomPoint :: (MonadReader BoardInfo m, MonadState GameState m) =>  m Point
makeRandomPoint = do
  BoardInfo n i <- ask
  g             <- gets randomGen
  let (g1, g2)  = split g
      (n', g1') = uniformR (1, n) g1
      (i', _) = uniformR (1, i) g2
      newPoint  = (n', i')
  modify $ \x -> x{randomGen = g1'}
  pure newPoint

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
newApple :: (MonadReader BoardInfo m, MonadState GameState m) =>  m Point
newApple = do
  bi <- ask
  GameState snake_body old_apple move sg <- get
  new_apple <- makeRandomPoint
  if new_apple == old_apple || new_apple `inSnake` snake_body
     then newApple
     else modify (\x -> x{applePosition = new_apple}) >> pure new_apple

-- | move the snake's head forward without removing the tail. (This is the case of eating an apple)
extendSnake :: (MonadReader BoardInfo m, MonadState GameState m) => Point -> m DeltaBoard
extendSnake new_head = do
  binfo <- ask
  SnakeSeq old_head snake_body <- gets snakeSeq
  let new_snake = SnakeSeq new_head (old_head :<| snake_body)
      delta     = [(new_head, Board.SnakeHead), (old_head, Board.Snake)]
  modify $ \gstate -> gstate{snakeSeq = new_snake}
  pure delta

-- | displace snake, that is: remove the tail and move the head forward (This is the case of eating an apple)
displaceSnake :: (MonadReader BoardInfo m, MonadState GameState m) => Point -> m DeltaBoard
displaceSnake new_head = do
  binfo <- ask
  SnakeSeq old_head snake_body <- gets snakeSeq
  case snake_body of
    S.Empty -> let new_snake = SnakeSeq new_head S.empty
                   delta = [(new_head, Board.SnakeHead), (old_head, Board.Empty)]
                in modify (\x -> x{snakeSeq = new_snake}) >> pure delta
    xs :|> t -> let new_snake = SnakeSeq new_head (old_head :<| xs)
                    delta = [(new_head, Board.SnakeHead), (old_head, Board.Snake), (t, Board.Empty)]
                 in modify (\x -> x{snakeSeq = new_snake}) >> pure delta

-- | Moves the snake based on the current direction.
step :: (MonadReader BoardInfo m, MonadState GameState m) => m [Board.RenderMessage]
step = do
  bi <- ask
  gstate@(GameState s applePos _ _) <- get
  let  newHead           = nextHead bi gstate
       isEatingApple     = newHead == applePos
       isColision        = newHead `inSnake` s

  if | isColision -> pure [Board.GameOver]
     | isEatingApple -> do delta <- extendSnake newHead
                           newApplePos <- newApple
                           let delta' = (newApplePos, Board.Apple):delta
                           pure [Board.RenderBoard delta', Board.Score]
     | otherwise -> do delta <- displaceSnake newHead
                       pure [Board.RenderBoard delta]

-- | Given a event runs the step.
move :: Monad m => Event -> BoardInfo -> GameState -> m ([Board.RenderMessage], GameState)
move Tick bi gstate = step `runReaderT` bi `runStateT` gstate
move (UserEvent m) bi gstate =
  if movement gstate == opositeMovement m
     then step `runReaderT` bi `runStateT` gstate
     else step `runReaderT` bi `runStateT` gstate{movement = m}