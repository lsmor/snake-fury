{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

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
import Renderer ( AppState(AppState), run, Env(Env), withWindow, withRenderer, SDLConfig(..) )
import RenderState (BoardInfo(BoardInfo))
import qualified SDL
import qualified SDL.Font as Font

-- | main.
main :: IO ()
main = do
  -- enable reading key strokes
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  hSetBuffering stdout NoBuffering
  hSetBinaryMode stdout True

  -- sdl initialization
  Font.initialize
  SDL.initializeAll

  font <- Font.load "assets/l_10646.ttf" 20

  -- Game Initializacion
  [h, w, fps] <- fmap read <$> getArgs
  let timeSpeed = 1_000_000 `div` fps -- One second is 1_000_000 microseconds, which is the unit used by GHC internally.
  (binf, gameState, renderState, eventQueue) <- gameInitialization h w timeSpeed

  withWindow SDL.defaultWindow "Snake Fury" $ \win -> do
    withRenderer SDL.defaultRenderer win $ \ren -> do

      let initialState = AppState gameState renderState
      let env = Env binf eventQueue (SDLConfig win ren font)

      run env initialState
