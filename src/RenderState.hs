
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}


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
import Control.Monad.State.Strict (State, put, get, runState, evalState, MonadState, StateT (runStateT))

type Point = (Int, Int)
data CellType = Empty | Snake | SnakeHead | Apple deriving (Show, Eq)

data BoardInfo = BoardInfo {height :: Int, width :: Int} deriving (Show, Eq)
type Board = Array Point CellType
type DeltaBoard = [(Point, CellType)]

data RenderMessage = RenderBoard DeltaBoard | GameOver | Score deriving (Show, Eq)
data RenderState   = RenderState {board :: Board, gameOver :: Bool, score :: Int}

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
updateRenderState :: (MonadReader BoardInfo m, MonadState RenderState m) => RenderMessage -> m ()
updateRenderState message = do
  (RenderState b gOver s) <- get
  case message of
    RenderBoard delta -> put $ RenderState (b // delta) gOver s
    GameOver          -> put $ RenderState b  True s
    Score             -> put $ RenderState b  gOver (s + 1)


updateMessages :: (MonadReader BoardInfo m, MonadState RenderState m) =>  [RenderMessage] -> m ()
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


renderStep :: (MonadReader BoardInfo m, MonadState RenderState m) => [RenderMessage] -> m Builder
renderStep msgs = do 
  updateMessages msgs
  binf@(BoardInfo h w)    <- ask
  (RenderState b gOver s) <- get
  let boardToString =  foldl' fprint (mempty, 0)
      fprint (!s, !i) cell =
        if ((i + 1) `mod` w) == 0
          then (s <> ppCell cell <> B.charUtf8 '\n', i + 1 )
          else (s <> ppCell cell , i + 1)
  if gOver  
    then pure $ ppScore s <> fst (boardToString $ emptyGrid binf)
    else pure $ ppScore s <> fst (boardToString b)

render :: Monad m => [RenderMessage] -> BoardInfo -> RenderState -> m (Builder, RenderState)
render msgs = runStateT . runReaderT (renderStep msgs)