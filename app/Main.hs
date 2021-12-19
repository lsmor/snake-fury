module Main where

import qualified Board
import qualified Snake
import Board (BoardInfo(BoardInfo))
import System.Random ( getStdGen, randomRIO )
import qualified Data.Sequence as S
import System.Environment (getArgs)
import Control.Concurrent
    ( forkIO, newEmptyMVar, putMVar, threadDelay, MVar, readMVar )
import System.IO (stdin, hReady, hSetBuffering, BufferMode (NoBuffering), hSetEcho, stdout)
import Control.Concurrent.BoundedChan
    ( newBoundedChan, tryReadChan, tryWriteChan, BoundedChan )
import qualified Data.ByteString.Builder as B


data Clock = Tick
data Event = ClockEvent Clock | UserEvent Snake.Movement
type UserInputQueue = BoundedChan Snake.Movement
type TimeQueue = MVar Clock
data EventQueue = EventQueue {clock :: TimeQueue, userInput :: UserInputQueue}


-- EventQueue utils
writeClock :: Int -> EventQueue -> IO ()
writeClock timeSpeed queue@(EventQueue c _) = threadDelay timeSpeed >> putMVar c Tick >> writeClock timeSpeed queue

writeUserInput :: EventQueue -> IO ()
writeUserInput queue@(EventQueue _ userqueue) = do
    c <- getKey
    case c of
      "\ESC[A" -> tryWriteChan userqueue Snake.North >> writeUserInput queue
      "\ESC[D" -> tryWriteChan userqueue Snake.West  >> writeUserInput queue
      "\ESC[C" -> tryWriteChan userqueue Snake.East  >> writeUserInput queue
      "\ESC[B" -> tryWriteChan userqueue Snake.South >> writeUserInput queue
      _   -> writeUserInput queue

readEvent :: EventQueue -> IO Event
readEvent (EventQueue c userqueue) = do
  mv <- tryReadChan userqueue
  case mv of
    Nothing -> ClockEvent <$> readMVar c
    Just move -> return $ UserEvent move


getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

getRandomPoint :: Int -> Int -> IO Board.Point
getRandomPoint h w = (,) <$> randomRIO (1, h) <*> randomRIO (1, w)

inititalizePoints :: Int -> Int -> IO (Board.Point, Board.Point)
inititalizePoints h w = do
  (snakeInit, appleInit) <- (,) <$> getRandomPoint h w <*> getRandomPoint h w
  if snakeInit == appleInit
    then inititalizePoints h w
    else return (snakeInit, appleInit)



main :: IO ()
main = do
    -- enable reading key strokes
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    -- hSetBuffering stdout $ BlockBuffering Nothing
    -- Game Init
    [h, w, timeSpeed] <- fmap read <$> getArgs
    (snakeInit, appleInit) <- inititalizePoints h w
    sg <- getStdGen
    let binf = BoardInfo h w
        gameState = Snake.AppState (Snake.SnakeSeq snakeInit S.Empty) appleInit Snake.North binf sg
        board = Board.buildInitialBoard binf snakeInit appleInit
    newUserEventQueue <- newBoundedChan 3
    newClock <- newEmptyMVar
    let eventQueue = EventQueue newClock newUserEventQueue
    _ <- forkIO $ writeClock timeSpeed eventQueue
    _ <- forkIO $ gameloop gameState board timeSpeed eventQueue
    writeUserInput eventQueue

  where
    gameloop :: Snake.AppState -> Board.RenderState -> Int -> EventQueue -> IO ()
    gameloop app b timeSpeed queue = do
        threadDelay timeSpeed
        event <- readEvent queue
        let (app',delta) =
              case event of
                    ClockEvent Tick -> Snake.move app
                    UserEvent move ->
                      if Snake.movement app == Snake.opositeMovement move
                        then Snake.move app
                        else Snake.move $ app {Snake.movement = move}
            board' = b `Board.updateRenderState` delta
        putStr "\ESC[2J"
        B.hPutBuilder stdout $ Board.render board'
        gameloop app' board' timeSpeed queue

