{-# LANGUAGE TypeApplications #-}

module EventQueue where

import qualified Board
import qualified Snake
import Board (BoardInfo(BoardInfo), updateMessages)
import System.Random ( getStdGen, randomRIO )
import qualified Data.Sequence as S
import System.Environment (getArgs)
import Control.Concurrent
    ( forkIO, newEmptyMVar, putMVar, threadDelay, MVar, readMVar, newMVar, swapMVar )
import System.IO (stdin, hReady, hSetBuffering, BufferMode (NoBuffering), hSetEcho, stdout)
import Control.Concurrent.BoundedChan
    ( newBoundedChan, tryReadChan, tryWriteChan, BoundedChan )
import qualified Data.ByteString.Builder as B


data Clock = Tick
data Event = ClockEvent Clock | UserEvent Snake.Movement
type UserInputQueue = BoundedChan Snake.Movement
type TimeQueue = MVar Clock
data EventQueue = EventQueue {clock :: TimeQueue, userInput :: UserInputQueue, speed :: MVar (Int, Int)}


-- EventQueue utils
writeClock :: EventQueue -> IO ()
writeClock  queue@(EventQueue clockQueue _ globalSpeed) = readMVar globalSpeed >>= threadDelay . fst >> putMVar clockQueue Tick >> writeClock queue

writeUserInput :: EventQueue -> IO ()
writeUserInput queue@(EventQueue _ userqueue _) = do
    c <- getKey
    case c of
      "\ESC[A" -> tryWriteChan userqueue Snake.North >> writeUserInput queue
      "\ESC[D" -> tryWriteChan userqueue Snake.West  >> writeUserInput queue
      "\ESC[C" -> tryWriteChan userqueue Snake.East  >> writeUserInput queue
      "\ESC[B" -> tryWriteChan userqueue Snake.South >> writeUserInput queue
      _   -> writeUserInput queue

writeSpeed :: Int -> EventQueue -> IO Int
writeSpeed score queue = do
    (currentSpeed, initialSpeed) <- readMVar $ speed queue
    let level = min score 50 `quot` 10              -- maximun of 5 levels every 10 apples
        speedFactor = 1 - fromIntegral level / 10.0 -- every level speeds up the time by a 10%
        newSpeed  = floor @Double $ fromIntegral initialSpeed * speedFactor
    if currentSpeed == newSpeed
      then return currentSpeed
      else swapMVar (speed queue) (newSpeed, initialSpeed) >> return newSpeed

readEvent :: EventQueue -> IO Event
readEvent (EventQueue clockQueue userQueue _) = do
  mv <- tryReadChan userQueue
  case mv of
    Nothing -> ClockEvent <$> readMVar clockQueue
    Just move -> return $ UserEvent move


getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)
