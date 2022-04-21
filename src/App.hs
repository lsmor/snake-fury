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
import RenderState (BoardInfo, Board, RenderMessage, RenderState, updateMessages)
import qualified RenderState
import Snake (GameState (movement), runStep, opositeMovement, Movement)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))
import Control.Monad.Reader.Class ( MonadReader, asks )
import Control.Monad.IO.Class ( MonadIO (liftIO) )
import Data.ByteString.Builder (Builder)
import Control.Concurrent (readMVar, swapMVar, threadDelay)
import Control.Monad (void, forever, when)
import qualified Data.ByteString.Builder as B
import System.IO (stdout)
import Data.Kind (Type)
import Control.Monad.State.Class (MonadState, gets, modify', get, put)
import Control.Monad.Trans (lift)
import Control.Monad.State.Strict (runState, StateT (runStateT), evalState, evalStateT)
import Control.Concurrent.BoundedChan (tryWriteChan, tryReadChan)

data Config   = Config {boardInfo :: BoardInfo, initialTime :: Int}
data AppState = AppState {gameState :: GameState, renderState :: RenderState}
data Env      = Env {config :: Config, queue :: EventQueue}

newtype AppT e m a = AppT {runApp :: ReaderT e m a}  -- TODO: Write about GeneralizedNewtypeDeriving
  deriving (Functor, Applicative, Monad, MonadReader e, MonadIO)

deriving instance MonadState AppState m => MonadState AppState (AppT e m)

class MonadQueue m where
  pullEvent :: m Event
  setSpeed :: Int -> m ()
  getSpeed :: m Int

class MonadRender m where
  updateRenderState :: [RenderMessage] -> m ()
  render :: m ()

instance (MonadIO m, MonadReader Env m) => MonadQueue m where
  setSpeed i = do
    v <- asks (speed . queue)
    liftIO $ void $ swapMVar v i
  getSpeed = do
    v <- asks (speed . queue)
    liftIO $ readMVar v
  pullEvent = do
    EventQueue userQueue _ <- asks queue
    mv <- liftIO $ tryReadChan userQueue
    case mv of
      Nothing   -> return Tick
      Just move -> return $ UserEvent move


-- | Given the app state, the render state and the event queue, updates everything in one time step, then execute again.
gameloop :: (MonadIO m, MonadReader Env m, MonadState AppState m, MonadQueue m, MonadRender m) => m ()
gameloop = forever $ do
    AppState gState rState <- get
    iTime         <- asks $ initialTime . config
    currentSpeed  <- getSpeed
    let newSpeed = calculateSpeed (RenderState.score rState) iTime currentSpeed
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



