{-# LANGUAGE TypeApplications #-}

{-|
This module handle the external events of the game. That is: the user inputs and the time. 
-}
module EventQueue where

import qualified GameState as Snake
import Control.Concurrent
    ( MVar )
import Control.Concurrent.BoundedChan
    ( BoundedChan )


-- | The are two kind of events, a `ClockEvent`, representing movement which is not force by the user input, and `UserEvent` which is the opposite.
data Event = Tick | UserEvent Snake.Movement

-- | the `UserInputQueue` is an asynchronous bounded channel which contains snake movements. This channel is feeded by key strokes
type UserInputQueue = BoundedChan Snake.Movement

-- | The `EventQueue` has a `TimeQueue` a `UserInputQueue` and the global speed of consumption. The speed is represented by the current speed
data EventQueue = EventQueue {userInput :: UserInputQueue, speed :: MVar Int}

-- | Given the current score, updates the global shared speed every 10 points by a factor of 10%. Returns the current state
calculateSpeed :: Int -> Int -> Int
calculateSpeed score initialSpeed =
    let level = min score 50 `quot` 10              -- maximun of 5 levels every 10 apples
        speedFactor = 1 - fromIntegral level / 10.0 -- every level speeds up the time by a 10%
     in floor @Double $ fromIntegral initialSpeed * speedFactor
