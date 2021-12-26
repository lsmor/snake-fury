{-# LANGUAGE TypeApplications #-}
module Main where

import qualified Board
import qualified Snake
import Board (BoardInfo(BoardInfo), updateMessages)
import System.Random ( getStdGen, randomRIO )
import qualified Data.Sequence as S
import System.Environment (getArgs)
import Control.Concurrent
    ( forkIO, newEmptyMVar, putMVar, threadDelay, MVar, readMVar, newMVar, takeMVar, swapMVar )
import System.IO (stdin, hReady, hSetBuffering, BufferMode (NoBuffering), hSetEcho, stdout)
import Control.Concurrent.BoundedChan
    ( newBoundedChan, tryReadChan, tryWriteChan, BoundedChan )
import qualified Data.ByteString.Builder as B
import Control.Monad (guard)


data Clock = Tick
data Event = ClockEvent Clock | UserEvent Snake.Movement
type UserInputQueue = BoundedChan Snake.Movement
type TimeQueue = MVar Clock
data EventQueue = EventQueue {clock :: TimeQueue, userInput :: UserInputQueue}
type GlobalSpeed = MVar (Int, Int)


-- EventQueue utils
writeClock :: GlobalSpeed -> EventQueue -> IO ()
writeClock timeSpeed queue@(EventQueue c _) = readMVar timeSpeed >>= threadDelay . fst >> putMVar c Tick >> writeClock timeSpeed queue

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
    -- Game Initializacion
    [h, w, timeSpeed] <- fmap read <$> getArgs
    (snakeInit, appleInit) <- inititalizePoints h w
    sg <- getStdGen
    let binf = BoardInfo h w
        gameState = Snake.AppState (Snake.SnakeSeq snakeInit S.Empty) appleInit Snake.North binf sg
        board = Board.buildInitialBoard binf snakeInit appleInit
    newUserEventQueue <- newBoundedChan 3
    newClock <- newEmptyMVar
    globalSpeed <- newMVar (timeSpeed, timeSpeed)
    -- Game Loop
    let eventQueue = EventQueue newClock newUserEventQueue
    _ <- forkIO $ writeClock globalSpeed eventQueue
    _ <- forkIO $ gameloop gameState board globalSpeed eventQueue
    writeUserInput eventQueue

  where
    gameloop :: Snake.AppState -> Board.RenderState -> GlobalSpeed -> EventQueue -> IO ()
    gameloop app b globalSpeed queue = do
        (currentSpeed, initialSpeed) <- readMVar globalSpeed
        threadDelay currentSpeed
        let speedfactor =  1 - (fromIntegral @Int @Double (min (Board.score b) 100 `quot` 10) / 10.0)
            newSpeed    = floor $ fromIntegral initialSpeed * speedfactor
        _ <- swapMVar globalSpeed (newSpeed, initialSpeed)
        event <- readEvent queue
        let (app',deltas) =
              case event of
                    ClockEvent Tick -> Snake.move app
                    UserEvent move ->
                      if Snake.movement app == Snake.opositeMovement move
                        then Snake.move app
                        else Snake.move $ app {Snake.movement = move}
            board' = b `Board.updateMessages` deltas
        putStr "\ESC[2J"
        B.hPutBuilder stdout $ Board.render board'
        gameloop app' board' globalSpeed queue