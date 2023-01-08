{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module App where
import GameState (GameState, move, HasGameState (getGameState, setGameState), Event)
import RenderState (RenderState (score, gameOver), BoardInfo, HasRenderState (getRenderState, setRenderState), RenderMessage, updateMessages, HasBoardInfo (getBoardInfo), buildBoard)
import qualified RenderState
import Control.Monad.Reader (MonadReader (ask), asks, ReaderT (runReaderT))
import Control.Monad.State (MonadState (get), gets, StateT (runStateT), evalStateT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import EventQueue (EventQueue (initialSpeed, currentSpeed), readEvent, setSpeed, calculateSpeed, HasEventQueue (getEventQueue))
import Control.Concurrent (threadDelay, readMVar, putMVar)
import Control.Monad (forever)
import qualified Data.ByteString.Builder as B
import System.IO (stdout)
import Control.Monad (unless)


data AppState = AppState GameState RenderState
data Env = Env BoardInfo EventQueue
newtype App m a = App {runApp :: ReaderT Env (StateT AppState m) a}
  deriving (Functor , Applicative, Monad, MonadState AppState, MonadReader Env, MonadIO)

{-
Instances for the State of the application.
-}

instance HasGameState AppState where
  getGameState (AppState g _ )   = g
  setGameState (AppState _ r) g = AppState g r

instance HasRenderState AppState where
  getRenderState (AppState _ r )   = r
  setRenderState (AppState g _ ) r = AppState g r

{-
The instances for Environment.
-}

instance HasBoardInfo Env where
  getBoardInfo (Env b _ ) = b

instance HasEventQueue Env where
  getEventQueue (Env _ q) = q


{-
Our small DSL for working with all components of the software
-}

class Monad m => MonadQueue m where
  pullEvent :: m Event          -- ^Pull an Event from the queue

class Monad m => MonadSnake m where
  updateGameState :: Event -> m [RenderMessage]
  updateRenderState :: [RenderMessage] -> m ()

class Monad m => MonadRender m where
  render :: m ()


{-
The instances for App monad.
-}

instance (MonadIO m) => MonadQueue (App m) where
  pullEvent = do
    q <- asks getEventQueue
    liftIO $ readEvent q

instance Monad m => MonadSnake (App m) where
  updateGameState = move
  updateRenderState = updateMessages

instance (MonadIO m) => MonadRender (App m) where
  render = do
    binf <- asks getBoardInfo
    rstate <- gets getRenderState
    liftIO $ putStr "\ESC[2J" --This cleans the console screen
    liftIO $ B.hPutBuilder stdout (buildBoard binf rstate)


{-
The main logic of the game
-}

setSpeedOnScore :: (MonadReader env m, HasEventQueue env, MonadState state m, HasRenderState state, MonadIO m) => m Int
setSpeedOnScore = do
  current_score <- gets (score . getRenderState) -- Read the current reference to speed
  queue         <- asks getEventQueue
  liftIO $ setSpeed current_score queue


gameStep :: (MonadQueue m, MonadSnake m, MonadRender m) => m ()
gameStep = pullEvent >>= updateGameState >>= updateRenderState >> render

gameloop :: (MonadQueue m, MonadSnake m, MonadRender m, MonadState state m, HasRenderState state, MonadReader env m, HasEventQueue env, MonadIO m) => m ()
gameloop = do
  w <- setSpeedOnScore 
  liftIO $ threadDelay w
  gameStep
  isGameOver <- gets (gameOver . getRenderState)
  unless isGameOver gameloop

run :: Env -> AppState -> IO ()
run env app = runApp gameloop `runReaderT` env `evalStateT` app
