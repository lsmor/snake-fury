
module Main where

import qualified Board
import qualified Snake
import Board (BoardInfo(BoardInfo), updateMessages)
import System.Random ( getStdGen, randomRIO )
import qualified Data.Sequence as S
import System.Environment (getArgs)
import Control.Concurrent
    ( forkIO, newEmptyMVar, threadDelay, newMVar )
import System.IO (stdin, hSetBuffering, BufferMode (NoBuffering), hSetEcho, stdout)
import Control.Concurrent.BoundedChan
    ( newBoundedChan )
import qualified Data.ByteString.Builder as B
import EventQueue
    ( readEvent,
      writeClock,
      writeSpeed,
      writeUserInput,
      Clock(Tick),
      Event(UserEvent, ClockEvent),
      EventQueue(EventQueue) )

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
gameInitialization :: Int -> Int -> Int -> IO (Snake.AppState, Board.RenderState , EventQueue)
gameInitialization hight width initialspeed = do
  (snakeInit, appleInit) <- inititalizePoints hight width
  sg <- getStdGen
  newUserEventQueue <- newBoundedChan 3
  newClock <- newEmptyMVar
  newSpeed <- newMVar (initialspeed, initialspeed)
  let binf = BoardInfo hight width
      gameState = Snake.AppState (Snake.SnakeSeq snakeInit S.Empty) appleInit Snake.North binf sg
      renderState = Board.buildInitialBoard binf snakeInit appleInit
      eventQueue = EventQueue newClock newUserEventQueue newSpeed
  return (gameState, renderState, eventQueue)

-- | Given the app state, the render state and the event queue, updates everything in one time step, then execute again.
gameloop :: Snake.AppState -> Board.RenderState -> EventQueue -> IO ()
gameloop app b queue =  do
    currentSpeed <- writeSpeed (Board.score b) queue                 -- Update speed based in the score
    threadDelay currentSpeed                                         -- waits for the time specify in the global speed
    event <- readEvent queue                                         -- Read the next event in the queue
    let (app',deltas) =                                              -- based in the type of the event, updates the state
          case event of                                              -- and produces the messages neccesary for update the rendering
                ClockEvent Tick -> Snake.move app
                UserEvent move ->
                  if Snake.movement app == Snake.opositeMovement move
                    then Snake.move app
                    else Snake.move $ app {Snake.movement = move}
        board' = b `Board.updateMessages` deltas                     -- udpate the RenderState
    putStr "\ESC[2J"                                                 -- clean the state and print out the new render state
    B.hPutBuilder stdout $ Board.render board'
    gameloop app' board' queue                                       -- re-execute the game loop with the updated state.

-- | main.
main :: IO ()
main = do
    -- enable reading key strokes
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    
    -- Game Initializacion
    [h, w, timeSpeed] <- fmap read <$> getArgs
    (gameState, renderState, eventQueue) <- gameInitialization h w timeSpeed

    -- Game Loop. We run three different threads, one for the clock, one for the gameloop and one for user inputs.
    _ <- forkIO $ writeClock eventQueue
    _ <- forkIO $ gameloop gameState renderState eventQueue
    writeUserInput eventQueue
