
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}


{-|
This module defines the board and the score. It includes not only the rendering of it but also the update logic of each.
-}
module RenderState where

import Data.Array ( (//), listArray, Array )
import Data.Foldable ( foldl' )
import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder (Builder)

-- | a Point is a pair of Ints
type Point = (Int, Int)

-- | There are 4 type of CellType. Each CellType is drawn differently.
data CellType = Empty | Snake | SnakeHead | Apple 

-- | The height and width of the board
data BoardInfo = BoardInfo {height :: Int, width :: Int} deriving (Show, Eq)

-- | The board is an Array of CellType. The array is not indexed by Int, but by Point (2D Int)
type Board = Array Point CellType

-- | It represents a list of changes done in the board.
type DeltaBoard = [(Point, CellType)]

-- | The `RenderState` includes all the information that affects rendering.
data RenderState   = RenderState {board :: Board, info :: BoardInfo, gameOver :: Bool, score :: Int}

-- | The `RenderState` can be update in three ways
data RenderMessage = RenderBoard DeltaBoard | GameOver | Score

-- | Creates the empty grid from its info
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
  RenderState b bInfo False 0
 where b = emptyGrid bInfo // [(initSnake, SnakeHead), (initApple, Apple)]

-- | updates a `RenderState` given a `RenderMessage`
updateRenderState :: RenderState -> RenderMessage -> RenderState
updateRenderState (RenderState b binf gOver s) message =
  case message of
    RenderBoard delta -> RenderState (b // delta) binf gOver s
    GameOver          -> RenderState b binf True s
    Score             -> RenderState b binf gOver (s + 1)

-- | extends `updateRenderState` to handle a list of messages
updateMessages :: RenderState -> [RenderMessage] -> RenderState
updateMessages = foldl' updateRenderState
