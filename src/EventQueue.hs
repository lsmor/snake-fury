{-# LANGUAGE TypeApplications #-}

{- |
This module handle the external events of the game. That is: the user inputs and the time.
-}
module EventQueue where

import Control.Concurrent (
  MVar,
  readMVar,
  swapMVar,
 )
import Control.Concurrent.BoundedChan (
  BoundedChan,
  tryReadChan,
  tryWriteChan,
 )
import GameState (Movement (..))
import qualified GameState as Snake
import System.IO (hReady, stdin)

-- | The are two kind of events, a `ClockEvent`, representing movement which is not force by the user input, and `UserEvent` which is the opposite.
data Event = Tick | UserEvent Snake.Movement

-- | the `UserInputQueue` is an asynchronous bounded channel which contains snake movements. This channel is feeded by key strokes
type UserInputQueue = BoundedChan Snake.Movement

-- | The `EventQueue` has a `UserInputQueue` and the global speed of consumption (as a mutable reference) and the initial speed of the game.
data EventQueue = EventQueue
  { -- | An asynchronous queue of movements the snake needs to do.
    userInput :: UserInputQueue
  , -- | A mutable reference to a Int. This is used for modifying the speed of the game as we play
    currentSpeed :: MVar Int
  , -- | The initial speed
    initialSpeed :: Int
  }

-- | Given the current score and the initial speed, calculates the new speed.
--   The speed is increased by 10% every 10 points, up to 50 points.
calculateSpeed :: Int -> Int -> Int
calculateSpeed score initialSpeed =
  let level = min score 50 `quot` 10 -- maximun of 5 levels every 10 apples
      speedFactor = 1 - fromIntegral level / 10.0 -- every level speeds up the time by a 10%
   in floor @Double $ fromIntegral initialSpeed * speedFactor

{- | Given the current score and the event queue, updates the new speed and returns it.
   This action is mutable, therefore must be run in the IO mondad
-}
setSpeed :: Int -> EventQueue -> IO Int
setSpeed s (EventQueue _ m_current initial_speed) = do
  current_speed <- readMVar m_current -- Read the current reference to speed
  let new_speed = calculateSpeed s initial_speed -- calculate new speed based on the score. This is a pure calculation, hence the let at the begining
  if new_speed == current_speed
    then pure current_speed -- If the new speed is equal to the current one, just return it
    else swapMVar m_current new_speed >> pure new_speed -- if not, swap the content of the reference with the new speed

{-
 |---------------|
 |- User Inputs -|
 |---------------|
-}

-- In StackOverflow we trust.
getKey :: IO [Char]
getKey = reverse <$> getKey' ""
 where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char : chars)

{- | This function translates key strokes to movements and push then into the queue.
 The player is free to push keys as fast a he/she can but the userqueue is bounded,
 meaning that if we push a movement to a filled queue it gets discarded.
 This is intented for the game play, If we press keys faster than the game speed
 they will be enqueued and pushed into the game with delay.
-}
writeUserInput :: EventQueue -> IO ()
writeUserInput queue@(EventQueue userqueue _ _) = do
  c <- getKey
  case parseUserInput c of
    Just dir -> tryWriteChan userqueue dir >> writeUserInput queue 
    Nothing  -> writeUserInput queue

{- | Parse common arrow-like keys
- \ESC[A/D/C/B are the escape codes for the arrow keys.
- hjkl are vim-like movements
- wasd are common in games
-}
parseUserInput :: String -> Maybe Snake.Movement

parseUserInput "\ESC[A" = Just Snake.North
parseUserInput "w" = Just Snake.North
parseUserInput "k" = Just Snake.North

parseUserInput "\ESC[D" = Just Snake.West
parseUserInput "a" = Just Snake.West
parseUserInput "h" = Just Snake.West

parseUserInput "\ESC[C" = Just Snake.East
parseUserInput "d" = Just Snake.East
parseUserInput "l" = Just Snake.East

parseUserInput "\ESC[B" = Just Snake.South
parseUserInput "s" = Just Snake.South
parseUserInput "j" = Just Snake.South

parseUserInput _ = Nothing

-- | Read the EventQueue and generates an Event to pass to the user logic
readEvent :: EventQueue -> IO Event
readEvent (EventQueue userqueue _ _) = do
  mv <- tryReadChan userqueue
  case mv of
    Nothing -> pure Tick
    Just move -> return $ UserEvent move