{-# LANGUAGE OverloadedStrings #-}

module Main where

import GUI ( withRenderer, withWindow, Env (Env), run )
import qualified SDL
import System.Environment ( getArgs )
import Initialization (gameInitialization)
import App (AppState(AppState), Config (Config))
import RenderState (BoardInfo(BoardInfo))
import qualified SDL.Font as Font


main :: IO ()
main = do
  [h, w, timeSpeed] <- fmap read <$> getArgs
  
  Font.initialize
  SDL.initializeAll

  font <- Font.load "assets/l_10646.ttf" 20

  (gameState, renderState, eventQueue) <- gameInitialization h w timeSpeed

  withWindow SDL.defaultWindow "Snake Fury" $ \win -> do
    withRenderer SDL.defaultRenderer win $ \ren -> do

      let initialState = AppState gameState renderState
      let cfg = Config (BoardInfo h w) timeSpeed
      let env = Env cfg eventQueue win ren font

      run initialState env