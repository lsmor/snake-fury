{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module App where

import EventQueue ( EventQueue (speed, EventQueue), Event (Tick, UserEvent), calculateSpeed)
import RenderState (BoardInfo, RenderMessage, RenderState)
import qualified RenderState
import Snake (GameState (movement), runStep, opositeMovement)
import Control.Monad.Reader (ReaderT, MonadReader )
import Control.Monad.Reader.Class ( asks )
import Control.Monad.IO.Class ( MonadIO (liftIO) )
import Control.Concurrent (readMVar, swapMVar, threadDelay)
import Control.Monad (void, forever, when)
import Control.Monad.State.Class (MonadState, modify', get)
import Control.Concurrent.BoundedChan (tryReadChan)

{-
The bare minimum to run the application is having a GameState and a RenderState.
Then some function which retrives events, and functions for updating the render state
-}


data AppState = AppState {gameState :: GameState, renderState :: RenderState}
data Config   = Config {boardInfo :: BoardInfo, initialTime :: Int}

class MonadQueue m where
  pullEvent :: m Event
  setSpeed :: Int -> m ()
  getSpeed :: m Int

class MonadRender m where
  updateRenderState :: [RenderMessage] -> m ()
  render :: m ()









newtype AppT e m a = AppT {runApp :: ReaderT e m a}
  deriving (Functor, Applicative, Monad, MonadReader e, MonadIO)

deriving instance MonadState AppState m => MonadState AppState (AppT e m)

class HasConfig env where
  getConfig :: env -> Config

class HasEventQueue env where
  getQueue :: env -> EventQueue


instance (MonadIO m, MonadReader e m, HasEventQueue e) => MonadQueue m where
  setSpeed i = do
    v <- asks (speed . getQueue)
    liftIO $ void $ swapMVar v i
  getSpeed = do
    v <- asks (speed . getQueue)
    liftIO $ readMVar v
  pullEvent = do
    EventQueue userQueue _ <- asks getQueue
    mv <- liftIO $ tryReadChan userQueue
    case mv of
      Nothing   -> return Tick
      Just move -> return $ UserEvent move


-- | Given the app state, the render state and the event queue, updates everything in one time step, then execute again.
gameloop :: (MonadIO m, MonadReader e m, HasConfig e, MonadState AppState m, MonadQueue m, MonadRender m) => m ()
gameloop = forever $ do
    AppState gState rState <- get
    iTime         <- asks $ initialTime . getConfig
    currentSpeed  <- getSpeed
    let newSpeed = calculateSpeed (RenderState.score rState) iTime
    when (currentSpeed /= newSpeed) (setSpeed newSpeed)
    liftIO $ threadDelay newSpeed
    event  <- pullEvent
    let 
      (deltas,gState') =                                           -- based in the type of the event, updates the state
        case event of                                              -- and produces the messages neccesary for update the rendering
          Tick           ->  Snake.runStep gState
          UserEvent move ->
            if Snake.movement gState == Snake.opositeMovement move
              then Snake.runStep gState
              else Snake.runStep $ gState {Snake.movement = move}
    updateRenderState deltas
    render
    modify' $ \s -> s {gameState = gState'}



