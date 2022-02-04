
module Main where

import qualified RenderState as Board
import qualified Snake
import RenderState (BoardInfo(BoardInfo))
import System.Random ( getStdGen, randomRIO )
import qualified Data.Sequence as S
import System.Environment (getArgs)
import Control.Concurrent
    ( forkIO, newEmptyMVar, newMVar )
import System.IO (stdin, hSetBuffering, BufferMode (NoBuffering), hSetEcho, stdout, hSetBinaryMode)
import Control.Concurrent.BoundedChan
    ( newBoundedChan )
import EventQueue
    ( writeClock,
      EventQueue(EventQueue) )
import App (AppState (AppState), run, Env (..), Config (..))
import TUI (writeUserInput)

-- | Produces a random point. Use for game initialization, random point generation is done purely within Snake module.
getRandomPoint :: Int -> Int -> IO Board.Point
getRandomPoint h w = (,) <$> randomRIO (1, h) <*> randomRIO (1, w)

-- | Sample two random points (snake head and apple) ensuring they are different
inititalizePoints :: Int -> Int -> IO (Board.Point, Board.Point)
inititalizePoints h w = do
  (snakeInit, appleInit) <- (,) <$> getRandomPoint h w <*> getRandomPoint h w
  if snakeInit == appleInit
    then inititalizePoints h w
    else return (snakeInit, appleInit)

-- | given the initial parameters height, width and initial time, It creates the initial state, the initial render state and the event queue
gameInitialization :: Int -> Int -> Int -> IO (Snake.GameState, Board.RenderState , EventQueue)
gameInitialization hight width initialspeed = do
  (snakeInit, appleInit) <- inititalizePoints hight width
  sg <- getStdGen
  newUserEventQueue <- newBoundedChan 3
  newClock <- newEmptyMVar
  newSpeed <- newMVar initialspeed
  let binf = BoardInfo hight width
      gameState = Snake.GameState (Snake.SnakeSeq snakeInit S.Empty) appleInit Snake.North binf sg
      renderState = Board.buildInitialBoard binf snakeInit appleInit
      eventQueue = EventQueue newClock newUserEventQueue newSpeed
  return (gameState, renderState, eventQueue)

-- | main.
main :: IO ()
main = do
    -- enable reading key strokes
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    hSetBuffering stdout NoBuffering
    hSetBinaryMode stdout True

    -- Game Initializacion
    [h, w, timeSpeed] <- fmap read <$> getArgs
    (gameState, renderState, eventQueue) <- gameInitialization h w timeSpeed

    -- Game Loop. We run three different threads, one for the clock, one for the gameloop and one for user inputs.
    _ <- forkIO $ writeClock eventQueue
    _ <- forkIO $ writeUserInput eventQueue
    let initialState = AppState gameState renderState
    let cfg = Config (BoardInfo h w) timeSpeed
    let env = Env cfg eventQueue
    run initialState env
