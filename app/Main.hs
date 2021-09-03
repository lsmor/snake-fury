module Main where

import qualified Board
import qualified Snake
import Board (BoardInfo(BoardInfo))
import System.Random ( getStdGen, randomRIO )
import qualified Data.Sequence as S
import Control.Monad.Loops ( iterateUntil )
import Data.Array ((//))

getRandomPoint :: Int -> Int -> IO Board.Point
getRandomPoint h w = (,) <$> randomRIO (1, h) <*> randomRIO (1, w) 

main :: IO ()
main = do
    let h = 10
        w = 10
    snakeInit <- getRandomPoint h w
    appleInit <- iterateUntil (snakeInit /= ) $ getRandomPoint h w
    sg        <- getStdGen
    let binf = BoardInfo h w
        gameState = Snake.AppState (Snake.SnakeSeq snakeInit S.Empty) appleInit Snake.North binf sg
        board = Board.buildInitialBoard binf snakeInit appleInit
    print snakeInit
    print appleInit
    putStr $ Board.ppBoard board binf
    let (_,delta) = Snake.move gameState
    putStrLn " "
    putStr $ Board.ppBoard (board // delta) binf