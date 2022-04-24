{-# LANGUAGE OverloadedStrings #-}
module Main where

import GUI ( renderBoardSDL, withRenderer, withWindow )
import qualified SDL
import qualified RenderState as R
import Control.Monad.IO.Class (MonadIO ())
import Control.Monad (unless)



appLoop :: MonadIO m => SDL.Window -> SDL.Renderer  -> m ()
appLoop w renderer = do
  let bInfo = R.BoardInfo 5 5
      initB = R.buildInitialBoard bInfo (1,1) (3, 2)

  events <- SDL.pollEvents
  let eventIsQPress event =
        case SDL.eventPayload event of
          SDL.QuitEvent -> True
          SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
          _ -> False
  let qPressed = any eventIsQPress events

  renderBoardSDL w renderer initB

  SDL.delay 100

  unless qPressed (appLoop w renderer)


main :: IO ()
main = do
  SDL.initializeAll
  withWindow SDL.defaultWindow "Snake Fury" $ \w -> do
    withRenderer SDL.defaultRenderer w (appLoop w)