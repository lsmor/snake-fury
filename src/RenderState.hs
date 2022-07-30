
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}


{-|
This module defines the board and the score. It includes not only the rendering of it but also the update logic of each.
-}
module RenderState where

import Data.Array ( (//), listArray, Array, elems )
import Control.Monad ( foldM_ )
import Data.Foldable ( foldl', traverse_ )
import Debug.Trace(trace)
import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder (Builder)
import Control.Monad.Reader (ReaderT (runReaderT), asks, ask, MonadReader)
import Control.Monad.State.Strict (State, put, get, runState, evalState, MonadState, StateT (runStateT), gets, modify)
import System.IO (stdout)
import Control.Monad.IO.Class (MonadIO (liftIO))

type Point = (Int, Int)
data CellType = Empty | Snake | SnakeHead | Apple deriving (Show, Eq)

data BoardInfo = BoardInfo {height :: Int, width :: Int} deriving (Show, Eq)
type Board = Array Point CellType
type DeltaBoard = [(Point, CellType)]

data RenderMessage = RenderBoard DeltaBoard | GameOver | Score deriving (Show, Eq)
data RenderState   = RenderState {board :: Board, gameOver :: Bool, score :: Int}

class HasRenderState state where
  getRenderState :: state -> RenderState
  setRenderState :: state -> RenderState -> state


-- | Creates the empty grip from its info
emptyGrid :: BoardInfo -> Board
emptyGrid (BoardInfo h w) = listArray boardBounds emptyCells
    where boardBounds =  ((1, 1), (h, w))
          emptyCells  = replicate (h*w) Empty

-- | Given BoardInfo, init point of snake and init point of apple, builds a board
buildInitialBoard
  :: BoardInfo -- ^ Board size
  -> Point     -- ^ initial point of the snake
  -> Point     -- ^ initial Point of the apple
  -> RenderState
buildInitialBoard bInfo initSnake initApple =
  RenderState b False 0
 where b = emptyGrid bInfo // [(initSnake, SnakeHead), (initApple, Apple)]

-- | Given tye current render state, and a message -> update the render state
updateRenderState :: (MonadReader BoardInfo m, MonadState state m, HasRenderState state)  => RenderMessage -> m ()
updateRenderState message = do
  (RenderState b gOver s) <- gets getRenderState
  case message of
    RenderBoard delta -> modify (`setRenderState` RenderState (b // delta) gOver s)
    GameOver          -> modify (`setRenderState` RenderState b  True s )
    Score             -> modify (`setRenderState` RenderState b  gOver (s + 1) )


updateMessages :: (MonadReader BoardInfo m, MonadState state m, HasRenderState state) =>  [RenderMessage] -> m ()
updateMessages = traverse_ updateRenderState

-- | Pretry printer Score
ppScore :: Int -> Builder
ppScore n =
  "----------\n" <>
  "Score: " <> B.intDec n  <> "\n" <>
  "----------\n"

-- | Provisional Pretty printer
ppCell :: CellType -> Builder
ppCell Empty     = "- "
ppCell Snake     = "0 "
ppCell SnakeHead = "$ "
ppCell Apple     = "X "

-- | purely builds the board given necessary info
buildBoard :: BoardInfo -> RenderState -> Builder
buildBoard binf@(BoardInfo h w) (RenderState b gOver s) =
  if gOver
    then ppScore s <> fst (boardToString $ emptyGrid binf)
    else ppScore s <> fst (boardToString b)
  where
    boardToString =  foldl' fprint (mempty, 0)
    fprint (!s, !i) cell =
      if ((i + 1) `mod` w) == 0
        then (s <> ppCell cell <> B.charUtf8 '\n', i + 1 )
        else (s <> ppCell cell , i + 1)

-- | runs one step in the render state: Process the messages and build the board with the resulting state
renderStep :: (MonadReader BoardInfo m, MonadState state m, HasRenderState state) => [RenderMessage] -> m Builder
renderStep msgs = do 
  updateMessages msgs
  binf <- ask
  rstate <- gets getRenderState
  pure $ buildBoard binf rstate

render :: (MonadReader BoardInfo m, MonadState state m, HasRenderState state, MonadIO m) => [RenderMessage] -> m ()
render msgs = do
  builder <- renderStep msgs
  liftIO $ putStr "\ESC[2J" --This cleans the console screen
  liftIO $ B.hPutBuilder stdout builder
