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

-- | The are two kind of events, a `ClockEvent`, representing movement which is not force by the user input, and `UserEvent` which is the opposite.
data Event = Tick | UserEvent Snake.Movement

-- | the `UserInputQueue` is an asynchronous bounded channel which contains snake movements. This channel is feeded by key strokes
type UserInputQueue = BoundedChan Snake.Movement

-- | The `EventQueue` has a `TimeQueue` a `UserInputQueue` and the global speed of consumption. The speed is represented by the current speed
data EventQueue = EventQueue {userInput :: UserInputQueue, speed :: MVar Int}

-- | Given the EventQueue feed the time queue at the speed determine by the global speed. Notice by desing this function blocks if the time queue
-- has a Tick already. This is usefull because we don't know if the next movement is going to be a user event of a clock event. So we need a function
-- which keeps the queue filled. 
-- writeClock :: EventQueue -> IO ()
-- writeClock  queue@(EventQueue clockQueue _ globalSpeed) = readMVar globalSpeed >>= threadDelay >> putMVar clockQueue Tick >> writeClock queue


-- | Given the current score, updates the global shared speed every 10 points by a factor of 10%. Returns the current state
calculateSpeed :: Int -> Int -> Int ->  Int
calculateSpeed score initialSpeed currentSpeed =
    let level = min score 50 `quot` 10              -- maximun of 5 levels every 10 apples
        speedFactor = 1 - fromIntegral level / 10.0 -- every level speeds up the time by a 10%
     in floor @Double $ fromIntegral initialSpeed * speedFactor
