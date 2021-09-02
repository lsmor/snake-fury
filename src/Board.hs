module Board where

import Data.Array ( (//), listArray, Array )

type Point = (Int, Int)
data CellType = Empty | Snake | Apple deriving (Show, Eq)

data BoardInfo = BoardInfo {height :: Int, width :: Int} deriving (Show, Eq)
type Board = Array Point CellType 
type DeltaBoard = [(Point, CellType)]


emptyBoard :: BoardInfo -> Board
emptyBoard (BoardInfo h w) = listArray boardBounds emptyCells
    where boardBounds =  ((1, 1), (h, w))
          emptyCells  = replicate (h*w) Empty

-- | Given BoardInfo, init point of snake and init point of apple, builds a board
buildInitialBoard :: BoardInfo -> Point -> Point -> Board
buildInitialBoard bInfo initSnake initApple = emptyBoard bInfo // [(initSnake, Snake), (initApple, Apple)]

