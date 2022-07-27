
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}


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
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks, ask)
import Control.Monad.Trans.State.Strict (State, put, get, runState, evalState)
import Control.Monad.Trans (lift)

type Point = (Int, Int)
data CellType = Empty | Snake | SnakeHead | Apple deriving (Show, Eq)

data BoardInfo = BoardInfo {height :: Int, width :: Int} deriving (Show, Eq)
type Board = Array Point CellType
type DeltaBoard = [(Point, CellType)]

data RenderMessage = RenderBoard DeltaBoard | GameOver | Score deriving (Show, Eq)
data RenderState   = RenderState {board :: Board, gameOver :: Bool, score :: Int}

-- Type for the RenderStep
type RenderStep a = ReaderT BoardInfo (State RenderState) a

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
updateRenderState :: RenderMessage -> RenderStep ()
updateRenderState message = do
  (RenderState b gOver s) <- lift get
  case message of
    RenderBoard delta -> lift . put $ RenderState (b // delta) gOver s
    GameOver          -> lift . put $ RenderState b  True s
    Score             -> lift . put $ RenderState b  gOver (s + 1)


updateMessages :: [RenderMessage] -> RenderStep ()
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


renderStep :: [RenderMessage] -> RenderStep Builder
renderStep msgs = do 
  binf@(BoardInfo h w)    <- ask
  (RenderState b gOver s) <- lift get
  let boardToString =  foldl' fprint (mempty, 0)
      fprint (!s, !i) cell =
        if ((i + 1) `mod` w) == 0
          then (s <> ppCell cell <> B.charUtf8 '\n', i + 1 )
          else (s <> ppCell cell , i + 1)
  updateMessages msgs
  if gOver  
    then pure $ ppScore s <> fst (boardToString $ emptyGrid binf)
    else pure $ ppScore s <> fst (boardToString b)

render :: [RenderMessage] -> BoardInfo -> RenderState ->  (Builder, RenderState)
render msgs = runState . runReaderT (renderStep msgs)