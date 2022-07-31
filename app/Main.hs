{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent (
  forkIO,
 )
import EventQueue (
  writeUserInput
 )
import Initialization (gameInitialization)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBinaryMode, hSetBuffering, hSetEcho, stdin, stdout)
import App (AppState (AppState), run, Env (Env))

-- | main.
main :: IO ()
main = do
  -- enable reading key strokes
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  hSetBuffering stdout NoBuffering
  hSetBinaryMode stdout True

  -- Game Initializacion
  [h, w, fps] <- fmap read <$> getArgs
  let timeSpeed = 1_000_000 `div` fps -- One second is 1_000_000 microseconds, which is the unit used by GHC internally.
  (binf, gameState, renderState, eventQueue) <- gameInitialization h w timeSpeed

  -- Game Loop. We run two different threads, one for the gameloop (main) and one for user inputs.
  _ <- forkIO $ writeUserInput eventQueue
  let initialState = AppState gameState renderState
      environment  = Env binf eventQueue
  run environment initialState
