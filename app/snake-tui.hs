module Main where

import RenderState (BoardInfo(BoardInfo))
import System.Environment (getArgs)
import Control.Concurrent
    ( forkIO )
import System.IO (stdin, hSetBuffering, BufferMode (NoBuffering), hSetEcho, stdout, hSetBinaryMode)
import App (AppState (AppState), Config (..))
import TUI (writeUserInput, run, Env (..))
import Initialization (gameInitialization)

-- | main.
main :: IO ()
main = do
    -- enable reading key strokes
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    hSetBuffering stdout NoBuffering
    hSetBinaryMode stdout True

    -- Game Initializacion
    [h, w, timeSpeed] <- fmap read <$> getArgs
    (gameState, renderState, eventQueue) <- gameInitialization h w timeSpeed

    -- Game Loop. We run three different threads, one for the clock, one for the gameloop and one for user inputs.
    _ <- forkIO $ writeUserInput eventQueue
    let initialState = AppState gameState renderState
    let cfg = Config (BoardInfo h w) timeSpeed
    let env = Env cfg eventQueue
    run initialState env
