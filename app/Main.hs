module Main where

import qualified Board
import qualified Snake
import Board (BoardInfo(BoardInfo))
import System.Random ( getStdGen, randomRIO )
import qualified Data.Sequence as S
import Control.Monad.Loops ( iterateUntil )
import Data.Array ((//))
import Snake (AppState(boardInfo))


getRandomPoint :: Int -> Int -> IO Board.Point
getRandomPoint h w = (,) <$> randomRIO (1, h) <*> randomRIO (1, w) 

main :: IO ()
main = do
    -- Game Init
    let h = 10
        w = 20
    snakeInit <- getRandomPoint h w
    appleInit <- iterateUntil (snakeInit /= ) $ getRandomPoint h w
    sg        <- getStdGen
    let binf = BoardInfo h w
        gameState = Snake.AppState (Snake.SnakeSeq snakeInit S.Empty) appleInit Snake.North binf sg
        board = Board.buildInitialBoard binf snakeInit appleInit
    
    -- Game loop
    putStr $ Board.ppBoard board binf
    updateOnIntput gameState board

  where updateOnIntput :: Snake.AppState -> Board.Board -> IO ()
        updateOnIntput app b = do
            c <- getLine 
            let updateApp = 
                    case c of
                        "w" -> app {Snake.movement = Snake.North}
                        "a" -> app {Snake.movement = Snake.West}
                        "d" -> app {Snake.movement = Snake.East}
                        "s" -> app {Snake.movement = Snake.South}
                        _   -> app
                (app',delta) = Snake.move updateApp
                board' = b // delta
            putStr "\ESC[2J"
            putStr $ Snake.ppAppState app'
            putStr "\n"
            putStr $ Board.ppBoard board' (boardInfo app')

            updateOnIntput app' board'

