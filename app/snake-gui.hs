{-# LANGUAGE OverloadedStrings #-}

module Main where

import GUI ( withRenderer, withWindow, Env (Env), run )
import qualified SDL
import System.Environment ( getArgs )
import Initialization (gameInitialization)
import App (AppState(AppState), Config (Config))
import RenderState (BoardInfo(BoardInfo))


main :: IO ()
main = do
  [h, w, timeSpeed] <- fmap read <$> getArgs
  (gameState, renderState, eventQueue) <- gameInitialization h w timeSpeed

  SDL.initializeAll
  withWindow SDL.defaultWindow "Snake Fury" $ \win -> do
    withRenderer SDL.defaultRenderer win $ \ren -> do

      let initialState = AppState gameState renderState
      let cfg = Config (BoardInfo h w) timeSpeed
      let env = Env cfg eventQueue win ren

      run initialState env