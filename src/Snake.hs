{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}

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
import Control.Monad.State.Strict
    ( StateT(StateT), MonadState(get), gets, modify', State, runState )
import Debug.Trace (trace)

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

-- | our App as a synonym of State monad with AppState as it state
type App a = State AppState a


-- | Calculate the Opposite movement. This is convenient since the snake can't change its movement to the opposite directly
opositeMovement :: Movement -> Movement
opositeMovement North = South
opositeMovement South = North
opositeMovement East = West
opositeMovement West = East

-- | Check if a point is in the snake
inSnake :: Point -> SnakeSeq  -> Bool
inSnake x0 (SnakeSeq x1 seq) = x0 == x1 || isJust (x0 `S.elemIndexL` seq)

-- | Calculates the new snake considering the board limit, the toroidal topology and the apple.
-- It returns, a triplet (a list of changes :: DeltaBoard, isCollision :: Bool, isEatingApple :: Bool )
-- and updates the state with the new snake. Notice that we only care about the snake body, not the 
-- apple or the randomGen
moveSnake :: App (DeltaBoard, Bool, Bool)
moveSnake = do
  AppState (SnakeSeq oldHead@(x, y) sb) applePos mov (BoardInfo h w) _ <- get
  let newHead = case mov of
            North -> if x - 1 <= 0 then (h, y) else (x - 1, y)
            South -> if x + 1  > h then (1, y) else (x + 1, y)
            East  -> if y + 1  > w then (x, 1) else (x, y + 1)
            West  -> if y - 1 <= 0 then (x, w) else (x, y - 1)
  let isCollision       = newHead `elem` sb
  let isEatingApple     = newHead == applePos
  let (newSnake, delta) = 
       case isEatingApple of
        True ->
          case sb of    -- The new snake body -|                The changes on the board -|
            S.Empty -> (SnakeSeq newHead (S.singleton oldHead), [(newHead, Board.SnakeHead), (oldHead, Board.Snake)])
            xs      -> (SnakeSeq newHead      (oldHead :<| xs), [(newHead, Board.SnakeHead), (oldHead, Board.Snake)])
        False ->
          case sb of            -- The new snake body -|                  The changes on the board -|
            S.Empty          -> (SnakeSeq newHead                S.empty, [(newHead, Board.SnakeHead), (oldHead, Board.Empty)                  ])
            x :<| S.Empty    -> (SnakeSeq newHead  (S.singleton oldHead), [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (x, Board.Empty)])
            x :<| (xs :|> t) -> (SnakeSeq newHead (oldHead :<| x :<| xs), [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (t, Board.Empty)])
  modify' $ \s -> s {snakeSeq = newSnake}
  return (delta, isCollision, isEatingApple)

-- | creates a random point updating the randomGen.
makeRandomPoint :: App Point
makeRandomPoint = do
  BoardInfo n i <- gets boardInfo     -- Get the boardInfo and the random generator from the state
  sg            <- gets randomGen
  let (g1, g2)  = split sg            -- define pure computations inside a monad
  let (n', g1') = uniformR (1, n) g1
  let (i', _)   = uniformR (1, i) g2
  let newPoint  = (n', i')
  modify' $ \s -> s {randomGen = g1'}  -- modify the old state updating the randomGen
  return newPoint 

-- | Calculates a new random apple, avoiding creating the apple in the same place, or in the snake body.
-- It updates the apple position and the randomGen
newApple :: App Point
newApple = do 
  currentApple <- gets applePosition  -- Get the current snake and apple from the state
  currentSnake <- gets snakeSeq
  newPoint     <- makeRandomPoint     -- Create a new Point. Notice, this updates the automatically the randomGen.
  -- If the new point is the old apple or is within the snake, then run a new apple again, else return update the state with the new Apple
  if newPoint == currentApple || newPoint `inSnake` currentSnake
      then newApple
      else modify' (\s -> s {applePosition = newPoint}) >> return newPoint

-- | The logic of the movement. It updates the state of the game and returns an iterator with changes that should be apply to the renderer
step :: App [Board.RenderMessage]
step = do 
  -- Notice how clean the logic is:
  --  Move the snake -> 
  --     If collision, send GameOver message;
  --     if isEatingApple, calculate a new one, update the list of messages a send them;
  --     else, just send the messages
  (delta, isCollision, isEatingApple) <- moveSnake
  if 
    | isCollision -> return [Board.GameOver]
    | isEatingApple -> do 
        newApplePos <- newApple
        let delta' = (newApplePos, Board.Apple):delta
        return [Board.RenderBoard delta', Board.Score]
    | otherwise -> return [Board.RenderBoard delta]


runStep :: AppState -> ([Board.RenderMessage], AppState)
runStep = runState step