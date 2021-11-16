{-# LANGUAGE NumericUnderscores #-}
module Main where

import qualified Board
import qualified Snake
import Board (BoardInfo(BoardInfo), updateBoard)
import System.Random ( getStdGen, randomRIO )
import qualified Data.Sequence as S
import Control.Monad.Loops ( iterateUntil )
import Snake (AppState(boardInfo))
import System.Environment (getArgs)
import Control.Concurrent
import System.IO (stdin, hReady, hSetBuffering, BufferMode (NoBuffering), hSetEcho)


data Event = Tick | UserEvent Snake.Movement
type EventQueue = MVar Event

writeClock :: Int -> EventQueue -> IO ()
writeClock timeSpeed queue = threadDelay timeSpeed >> putMVar queue Tick >> writeClock timeSpeed queue

writeUserInput :: EventQueue -> IO ()
writeUserInput queue = do
    c <- getKey
    case c of
      "\ESC[A" -> putMVar queue (UserEvent Snake.North) >> writeUserInput queue
      "\ESC[D" -> putMVar queue (UserEvent Snake.West) >> writeUserInput queue
      "\ESC[C" -> putMVar queue (UserEvent Snake.East) >> writeUserInput queue
      "\ESC[B" -> putMVar queue (UserEvent Snake.South) >> writeUserInput queue
      _   -> writeUserInput queue

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

getRandomPoint :: Int -> Int -> IO Board.Point
getRandomPoint h w = (,) <$> randomRIO (1, h) <*> randomRIO (1, w)

main :: IO ()
main = do
    -- enable reading key strokes
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    -- Game Init
    [h, w, timeSpeed] <- fmap read <$> getArgs
    snakeInit <- getRandomPoint h w
    appleInit <- iterateUntil (snakeInit /= ) $ getRandomPoint h w
    sg        <- getStdGen
    let binf = BoardInfo h w
        gameState = Snake.AppState (Snake.SnakeSeq snakeInit S.Empty) appleInit Snake.North binf sg
        board = Board.buildInitialBoard binf snakeInit appleInit
    eventQueue <- newEmptyMVar 

    _ <- forkIO $ writeClock timeSpeed eventQueue
    _ <- forkIO $ gameloop gameState board timeSpeed eventQueue
    writeUserInput eventQueue

  where   
    gameloop :: Snake.AppState -> Board.Board -> Int -> EventQueue -> IO ()
    gameloop app b timeSpeed queue = do
        threadDelay timeSpeed
        event <- takeMVar queue
        let (app',delta) = 
              case event of
                    Tick -> Snake.move app
                    UserEvent move -> Snake.move $ app {Snake.movement = move}
            board' = b `updateBoard` delta
        putStr "\ESC[2J"
        putStr $ Board.ppBoard board' (boardInfo app')
        gameloop app' board' timeSpeed queue

