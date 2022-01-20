{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import Linear (V4(..))
import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)

withWindow :: MonadIO m => WindowConfig -> Text -> (Window -> m a) -> m ()
withWindow cfg title io = do 
  w <- createWindow title cfg 
  showWindow w
  void $ io w 
  destroyWindow w

withRenderer :: MonadIO m => RendererConfig -> Window -> (Renderer -> m a) -> m ()
withRenderer cfg window io = do
  renderer <- createRenderer window (-1) cfg 
  void $ io renderer
  destroyRenderer renderer

main :: IO ()
main = do
  initializeAll
  withWindow defaultWindow "My SDL Application" $ \w -> 
    withRenderer defaultRenderer w appLoop

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  present renderer
  unless qPressed (appLoop renderer)