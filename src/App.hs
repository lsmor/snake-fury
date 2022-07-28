{-# LANGUAGE FlexibleContexts #-}

module App where
import GameState (GameState, move)
import RenderState (RenderState, BoardInfo, render)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Control.Monad.IO.Class (MonadIO (liftIO))
import EventQueue (EventQueue, readEvent)


gameStep :: (MonadReader BoardInfo m, MonadState GameState m, MonadState RenderState m, MonadIO m) => EventQueue -> m ()
gameStep queue = do 
    event <- liftIO $ readEvent queue 
    msgs  <- move event
    render msgs
