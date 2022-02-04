{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module TUI where

-- In this module we define the functions needed to interact with the terminal.

import EventQueue (EventQueue (EventQueue))
import qualified Snake
import Control.Concurrent.BoundedChan (tryWriteChan)
import System.IO (hReady, stdin)
import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder (Builder)
import RenderState (RenderState (RenderState), BoardInfo (BoardInfo), emptyGrid)
import Data.Foldable (foldl')


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
writeUserInput queue@(EventQueue _ userqueue _) = do
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
toBuilder (RenderState b binf@(BoardInfo h w) gOver s) =
  if gOver
    then ppScore s <> fst (boardToString $ emptyGrid binf)
    else ppScore s <> fst (boardToString b)
  where
    boardToString =  foldl' fprint (mempty, 0)
    fprint (!s, !i) cell =
      if ((i + 1) `mod` w) == 0
        then (s <> cell <> B.charUtf8 '\n', i + 1 )
        else (s <> cell , i + 1)