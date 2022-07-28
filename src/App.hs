{-# LANGUAGE FlexibleContexts #-}

module App where
import GameState (GameState, move, HasGameState (getGameState, setGameState))
import RenderState (RenderState (score), BoardInfo, render, HasRenderState (getRenderState, setRenderState))
import Control.Monad.Reader (MonadReader (ask), asks, ReaderT (runReaderT))
import Control.Monad.State (MonadState (get), gets, StateT (runStateT), evalStateT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import EventQueue (EventQueue, readEvent, setSpeed)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)


data AppState = AppState GameState RenderState

instance HasGameState AppState where
  getGameState (AppState g _ )   = g
  setGameState (AppState _ r) g = AppState g r

instance HasRenderState AppState where
  getRenderState (AppState _ r )   = r
  setRenderState (AppState g _ ) r = AppState g r


gameStep :: (MonadReader BoardInfo m, MonadState state m, HasGameState state, HasRenderState state, MonadIO m) => EventQueue -> m ()
gameStep queue = liftIO (readEvent queue) >>= move >>= render

gameloop :: (MonadReader BoardInfo m, MonadState state m, HasGameState state, HasRenderState state, MonadIO m) => EventQueue -> m ()
gameloop queue = forever $ do
  s <- gets (score . getRenderState)
  new_speed <- liftIO $ setSpeed s queue
  liftIO $ threadDelay new_speed
  gameStep queue

run :: BoardInfo -> AppState -> EventQueue -> IO ()
run binf app queue = runReaderT (evalStateT (gameloop queue) app) binf