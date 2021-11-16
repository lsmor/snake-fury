{-# LANGUAGE BangPatterns #-}

module Board where

import Data.Array ( (//), listArray, Array, elems )
import Control.Monad ( foldM_ )
import Data.Foldable ( foldl' )
import Debug.Trace(trace)


type Point = (Int, Int)
data CellType = Empty | Snake | SnakeHead | Apple deriving (Show, Eq)

data BoardInfo = BoardInfo {height :: Int, width :: Int} deriving (Show, Eq)
type Board = Array Point CellType
type DeltaBoard = [(Point, CellType)]

-- | Creates the empty grip from its info
emptyGrid :: BoardInfo -> Board
emptyGrid (BoardInfo h w) = listArray boardBounds emptyCells
    where boardBounds =  ((1, 1), (h, w))
          emptyCells  = replicate (h*w) Empty

-- | Given BoardInfo, init point of snake and init point of apple, builds a board
buildInitialBoard 
  :: BoardInfo -- ^ Board size
  -> Point -- ^ initial point of the snake
  -> Point -- ^ initial Point of the apple
  -> Board
buildInitialBoard bInfo initSnake initApple = emptyGrid bInfo // [(initSnake, SnakeHead), (initApple, Apple)]

updateBoard :: Board -> DeltaBoard -> Board
updateBoard = (//)

-- Provisional Pretty printer
ppCell :: CellType -> String
ppCell Empty     = "·"
ppCell Snake     = "0"
ppCell SnakeHead = "$"
ppCell Apple     = "X"

ppBoard :: Board -> BoardInfo -> String
ppBoard b (BoardInfo h w) = fst $ foldl' fprint ("", 0) b
  where fprint (!s, !i) cell = 
            if ((i + 1) `mod` w) == 0 
               then (s <> ppCell cell <> "\n", i + 1 )
               else (s <> ppCell cell , i + 1)