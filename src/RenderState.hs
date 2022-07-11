
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}


{-|
This module defines the board and the score. It includes not only the rendering of it but also the update logic of each.
-}
module RenderState where

import Data.Array ( (//), listArray, Array, elems )
import Control.Monad ( foldM_ )
import Data.Foldable ( foldl' )
import Debug.Trace(trace)


type Point = (Int, Int)
data CellType = Empty | Snake | SnakeHead | Apple deriving (Show, Eq)

data BoardInfo = BoardInfo {height :: Int, width :: Int} deriving (Show, Eq)
type Board = Array Point CellType
type DeltaBoard = [(Point, CellType)]

data RenderMessage = RenderBoard DeltaBoard | GameOver
data RenderState   = RenderState {board :: Board, info :: BoardInfo, gameOver :: Bool}

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
  RenderState b bInfo False 
 where b = emptyGrid bInfo // [(initSnake, SnakeHead), (initApple, Apple)]

-- | Given tye current render state, and a message -> update the render state
updateRenderState :: RenderState -> RenderMessage -> RenderState
updateRenderState (RenderState b binf gOver) message = 
  case message of
    RenderBoard delta -> RenderState (b // delta) binf gOver
    GameOver          -> RenderState b binf True

-- | Provisional Pretty printer
ppCell :: CellType -> String
ppCell Empty     = "- "
ppCell Snake     = "0 "
ppCell SnakeHead = "$ "
ppCell Apple     = "X "


-- | convert the RenderState in a String ready to be flushed into the console.
render :: RenderState -> String
render (RenderState b binf@(BoardInfo h w) gOver) =
  if gOver
    then fst $ boardToString(emptyGrid binf)
    else fst $ boardToString b
  where 
    boardToString =  foldl' fprint ("", 0)
    fprint (!s, !i) cell = 
      if ((i + 1) `mod` w) == 0 
        then (s <> ppCell cell <> "\n", i + 1 )
        else (s <> ppCell cell , i + 1)