{-# LANGUAGE TypeApplications #-}

{-|
This module handle the external events of the game. That is: the user inputs and the time. 
-}
module EventQueue where

import qualified RenderState as Board
import qualified Snake
import RenderState (BoardInfo(BoardInfo), updateMessages)
import System.Random ( getStdGen, randomRIO )
import qualified Data.Sequence as S
import System.Environment (getArgs)
import Control.Concurrent
    ( forkIO, newEmptyMVar, putMVar, threadDelay, MVar, readMVar, newMVar, swapMVar )
import System.IO (stdin, hReady, hSetBuffering, BufferMode (NoBuffering), hSetEcho, stdout)
import Control.Concurrent.BoundedChan
    ( newBoundedChan, tryReadChan, tryWriteChan, BoundedChan )
import qualified Data.ByteString.Builder as B


-- Notice that the Clock type seems useless since we can define data Event = Tick | UserEvent Movement. The reason to 
-- Create a separate type is to be able to define TimeQueue with a precise type. If clock didn't exists TimeQueue should've been
-- type TimeQueue = MVar Event which is not precise because events may be also user inputs, and we want to keep to different queues
-- One for the time and one for the user key strokes. In haskell we want precise types even if they look useless.

-- | The clock/time is represented by a single bit called Tick. 
data Clock = Tick

-- | The are two kind of events, a `ClockEvent`, representing movement which is not force by the user input, and `UserEvent` which is the opposite.
data Event = ClockEvent Clock | UserEvent Snake.Movement

-- | the `UserInputQueue` is an asynchronous bounded channel which contains snake movements. This channel is feeded by key strokes
type UserInputQueue = BoundedChan Snake.Movement

-- | The `TimeQueue` is a mutable variable which contains a Tick. This queue is feeded at constant time. 
type TimeQueue = MVar Clock

-- | The `EventQueue` has a `TimeQueue` a `UserInputQueue` and the global speed of consumption. The speed is represented by the current speed and the initial speed
data EventQueue = EventQueue {clock :: TimeQueue, userInput :: UserInputQueue, speed :: MVar (Int, Int)}


-- | Given the EventQueue feed the time queue at the speed determine by the global speed. Notice by desing this function blocks if the time queue
-- has a Tick already. This is usefull because we don't know if the next movement is going to be a user event of a clock event. So we need a function
-- which keeps the queue filled. 
writeClock :: EventQueue -> IO ()
writeClock  queue@(EventQueue clockQueue _ globalSpeed) = readMVar globalSpeed >>= threadDelay . fst >> putMVar clockQueue Tick >> writeClock queue

-- | This function translate key strokes to movements and push then into the queue. 
-- The player is free to push keys as fast a he/she can but the userqueue is bounded,
-- meaning that if we push a movement to a filled queue it gets discarded.
-- This is intented for the game play, If we press keys faster than the game speed
-- they will be enqueued and pushed into the game with delay. 
writeUserInput :: EventQueue -> IO ()
writeUserInput queue@(EventQueue _ userqueue _) = do
    c <- getKey
    case c of
      "\ESC[A" -> tryWriteChan userqueue Snake.North >> writeUserInput queue -- \ESC[A/D/C/B are the escape codes for the arrow keys.
      "\ESC[D" -> tryWriteChan userqueue Snake.West  >> writeUserInput queue
      "\ESC[C" -> tryWriteChan userqueue Snake.East  >> writeUserInput queue
      "\ESC[B" -> tryWriteChan userqueue Snake.South >> writeUserInput queue
      _   -> writeUserInput queue

-- | Given the current score, updates the global shared speed every 10 points by a factor of 10%. Returns the current state
writeSpeed :: Int -> EventQueue -> IO Int
writeSpeed score queue = do
    (currentSpeed, initialSpeed) <- readMVar $ speed queue
    let level = min score 50 `quot` 10              -- maximun of 5 levels every 10 apples
        speedFactor = 1 - fromIntegral level / 10.0 -- every level speeds up the time by a 10%
        newSpeed  = floor @Double $ fromIntegral initialSpeed * speedFactor
    if currentSpeed == newSpeed
      then return currentSpeed
      else swapMVar (speed queue) (newSpeed, initialSpeed) >> return newSpeed

-- | pulls an Event from the queue. If the user input queue is empty, read from the clock queue
readEvent :: EventQueue -> IO Event
readEvent (EventQueue clockQueue userQueue _) = do
  mv <- tryReadChan userQueue
  case mv of
    Nothing -> ClockEvent <$> readMVar clockQueue
    Just move -> return $ UserEvent move

-- In StackOverflow we trust.
getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)
