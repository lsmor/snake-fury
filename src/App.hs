{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module App where
import GameState (GameState, move, HasGameState (getGameState, setGameState), Event)
import RenderState (RenderState (score), BoardInfo, HasRenderState (getRenderState, setRenderState), RenderMessage, updateMessages, HasBoardInfo (getBoardInfo), buildBoard)
import qualified RenderState
import Control.Monad.Reader (MonadReader (ask), asks, ReaderT (runReaderT))
import Control.Monad.State (MonadState (get), gets, StateT (runStateT), evalStateT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import EventQueue (EventQueue (initialSpeed, currentSpeed), readEvent, setSpeed, calculateSpeed, HasEventQueue (getEventQueue))
import Control.Concurrent (threadDelay, readMVar, putMVar)
import Control.Monad (forever)
import qualified Data.ByteString.Builder as B
import System.IO (stdout)


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
gameloop = forever $ do
  w <- setSpeedOnScore
  liftIO $ threadDelay w
  gameStep

