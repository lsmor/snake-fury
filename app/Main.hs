module Main where

import RenderState (RenderState, updateRenderState, render)
import GameState ( opositeMovement, GameState(movement), move )
import EventQueue
    ( EventQueue, Event(UserEvent, Tick), writeUserInput, readEvent )
import System.Environment (getArgs)
import Control.Concurrent
    ( forkIO, threadDelay )
import System.IO (stdin, hSetBuffering, BufferMode (NoBuffering), hSetEcho, stdout, hSetBinaryMode)
import Initialization (gameInitialization)


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

    -- Game Loop. We run two different threads, one for the gameloop (main) and one for user inputs.
    _ <- forkIO $ writeUserInput eventQueue
    let initialState = gameState
    gameloop initialState renderState timeSpeed eventQueue
  where
    -- The game loop is easy:
    --   - wait some time
    --   - read an Event from the queue
    --   - Update the GameState
    --   - Update the RenderState based on message delivered by GameState update
    --   - Render into the console
    gameloop :: GameState -> RenderState -> Int -> EventQueue -> IO ()
    gameloop app b timeSpeed queue = do
        threadDelay timeSpeed
        event <- readEvent queue
        let (app',delta) =
              case event of
                    Tick -> move app
                    UserEvent m ->
                      if movement app == opositeMovement m
                        then move app
                        else move $ app {movement = m}
        let board' = b `updateRenderState` delta
        putStr "\ESC[2J"       --This cleans the console screen
        putStr $ render board'
        gameloop app' board' timeSpeed queue