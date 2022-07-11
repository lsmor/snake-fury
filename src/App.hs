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
import GameState (GameState (movement), runStep, opositeMovement)
import Control.Monad.Reader (ReaderT, MonadReader )
import Control.Monad.Reader.Class ( asks )
import Control.Monad.IO.Class ( MonadIO (liftIO) )
import Control.Concurrent (readMVar, swapMVar)
import Control.Monad (void, when)
import Control.Monad.State.Class (MonadState, modify', get, gets)
import Control.Concurrent.BoundedChan (tryReadChan)

{-
The bare minimum to run an application's step is having a GameState and a RenderState.
Then some function which retrives events, and functions for updating the render state.
Notice the model below can be run in pure code if we'd like to, because there are no
dependencies on MonadIO or mutable state.
-}


data AppState = AppState {gameState :: GameState, renderState :: RenderState}
data Config   = Config {boardInfo :: BoardInfo, initialTime :: Int}

class Monad m => MonadQueue m where
  pullEvent :: m Event
  setSpeed :: Int -> m ()
  getSpeed :: m Int

class Monad m => MonadGame m where
  updateGameState :: Event -> m [RenderMessage]
  updateRenderState :: [RenderMessage] -> m ()

class Monad m => MonadRender m where
  render :: m ()

gameStep :: (MonadQueue m, MonadGame m, MonadRender m) => m ()
gameStep = pullEvent 
       >>= updateGameState
       >>= updateRenderState 
       >>  render


{-
If we want to run something meaningfull, we need access to the configuration and 
to Some EventQueue via read only environment. Also, we can implement now some common
instances for MonadQueue and MonadGame
-}

class HasConfig env where
  getConfig :: env -> Config

class HasEventQueue env where
  getQueue :: env -> EventQueue

instance (Monad m, MonadIO m, MonadReader e m, HasEventQueue e) => MonadQueue m where
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

instance (Monad m, MonadState AppState m) => MonadGame m where
  updateGameState e = do
    current_state <- gets gameState
    let (mgs, new_state) =
         case e of 
          Tick -> GameState.runStep current_state   -- If it is a tick, just run a step
          UserEvent move ->                     -- If it is user event, modify the current direction acoordingly and run a step
            if movement current_state == opositeMovement move
              then runStep current_state
              else runStep $ current_state {movement = move}
    modify'$ \s -> s {gameState = new_state}
    return mgs
  
  updateRenderState msgs = do
    r <- gets renderState
    let r' = RenderState.updateMessages r msgs
    modify' $ \s -> s {renderState = r'}

{-
Now we can define a proper type which has everything wire together. 
The type below is the classic ReaderT app.
-}

newtype AppT e m a = AppT {runApp :: ReaderT e m a}
  deriving (Functor, Applicative, Monad, MonadReader e, MonadIO)

deriving instance MonadState AppState m => MonadState AppState (AppT e m)

  
updateQueueTime :: (MonadState AppState m, MonadReader e m, HasConfig e, MonadQueue m) => m Int
updateQueueTime = do
    AppState _ rState <- get
    init_time         <- asks $ initialTime . getConfig
    current_speed  <- getSpeed
    let new_speed = calculateSpeed (RenderState.score rState) init_time
    when (current_speed /= new_speed) (setSpeed new_speed)
    return new_speed


