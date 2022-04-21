{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE MonoLocalBinds #-}

module TUI where

-- In this module we define the functions needed to interact with the terminal.

import EventQueue (EventQueue (EventQueue))
import qualified Snake
import Control.Concurrent.BoundedChan (tryWriteChan)
import System.IO (hReady, stdin, stdout)
import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder (Builder)
import RenderState (RenderState (RenderState), BoardInfo (BoardInfo), emptyGrid, updateMessages)
import Data.Foldable (foldl')
import Control.Monad.State (StateT, MonadState, gets, modify', evalStateT)
import App (AppState (renderState), AppT (runApp), MonadRender (updateRenderState, render), Env, gameloop)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
    ( MonadIO(..), ReaderT(runReaderT), MonadReader )


-- # User Inputs

-- In StackOverflow we trust.
getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

-- | This function translate key strokes to movements and push then into the queue. 
-- The player is free to push keys as fast a he/she can but the userqueue is bounded,
-- meaning that if we push a movement to a filled queue it gets discarded.
-- This is intented for the game play, If we press keys faster than the game speed
-- they will be enqueued and pushed into the game with delay. 
writeUserInput :: EventQueue -> IO ()
writeUserInput queue@(EventQueue userqueue _) = do
    c <- getKey
    case c of
      "\ESC[A" -> tryWriteChan userqueue Snake.North >> writeUserInput queue -- \ESC[A/D/C/B are the escape codes for the arrow keys.
      "\ESC[D" -> tryWriteChan userqueue Snake.West  >> writeUserInput queue
      "\ESC[C" -> tryWriteChan userqueue Snake.East  >> writeUserInput queue
      "\ESC[B" -> tryWriteChan userqueue Snake.South >> writeUserInput queue
      _   -> writeUserInput queue


-- # Rendering

-- | Pretry printer Score
ppScore :: Int -> Builder
ppScore n =
  "----------\n" <>
  "Score: " <> B.intDec n  <> "\n" <>
  "----------\n"

-- | Transform the RenderState into a Builder
toBuilder :: RenderState -> Builder
toBuilder (RenderState b binf@(BoardInfo _ w) gOver s) =
  if gOver
    then ppScore s <> fst (boardToString $ emptyGrid binf)
    else ppScore s <> fst (boardToString b)
  where
    boardToString =  foldl' fprint (mempty, 0)
    fprint (!acc, !i) cell =
      if ((i + 1) `mod` w) == 0
        then (acc <> cell <> B.charUtf8 '\n', i + 1 )
        else (acc <> cell , i + 1)


-- Defining TUI
newtype Tui a = Tui { runTui :: AppT Env (StateT AppState IO) a }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadState AppState)

-- How is renderer in the terminal.
instance MonadRender Tui where
  updateRenderState msgs = do
    r <- gets renderState
    let r' = updateMessages r msgs
    modify' $ \s -> s {renderState = r'}
  render = do
    r <- gets renderState
    liftIO $ B.hPutBuilder stdout "\ESC[2J" >> B.hPutBuilder stdout (toBuilder r)


run :: AppState -> Env -> IO ()
run initialState env = flip evalStateT initialState . flip runReaderT env . runApp . runTui $ gameloop
