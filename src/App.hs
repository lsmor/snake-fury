{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module App where

import EventQueue ( EventQueue (speed), Event, readEvent )
import RenderState (BoardInfo, Board, RenderMessage, RenderState, render)
import Snake (AppState)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader.Class ( MonadReader, asks )
import Control.Monad.IO.Class ( MonadIO (liftIO) )
import Data.ByteString.Builder (Builder)
import Control.Concurrent (readMVar, swapMVar)
import Control.Monad (void)
import qualified Data.ByteString.Builder as B
import System.IO (stdout)


data Config = Config {boardInfo :: BoardInfo, initialTime :: Int}

data Env = Env {config :: Config, queue :: EventQueue, renderState :: RenderState }

newtype AppT m a = AppT {runApp :: ReaderT Env m a}  -- TODO: Write about GeneralizedNewtypeDeriving
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO)

class MonadClock m where
    setSpeed :: Int -> m ()
    getSpeed :: m Int

class MonadEvent m where
    getEvent :: m Event

class MonadRender m where
    render' :: m ()

instance MonadIO m => MonadClock (AppT m) where
  setSpeed i = do
    v <- asks (speed . queue)
    liftIO $ void $ swapMVar v i
  getSpeed = do 
    v <- asks (speed . queue)
    liftIO $ readMVar v

instance MonadIO m => MonadEvent (AppT m) where
  getEvent = do
    q <- asks queue
    liftIO $ readEvent q

instance MonadIO m => MonadRender (AppT m) where
  render' = do
    r <- asks renderState
    liftIO $ B.hPutBuilder stdout $ render r
    
  