{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Renderer where


import qualified SDL
import qualified SDL.Font as Font
import Data.Text (Text)
import GameState (GameState, move, HasGameState (getGameState, setGameState), Event, Movement (..))
import RenderState (RenderState (score, RenderState), BoardInfo (BoardInfo), HasRenderState (getRenderState, setRenderState), RenderMessage, updateMessages, HasBoardInfo (getBoardInfo), buildBoard, CellType (..), emptyGrid)
import qualified RenderState
import Control.Monad.Reader (MonadReader (ask), asks, ReaderT (runReaderT))
import Control.Monad.State (MonadState (get), gets, StateT (runStateT), evalStateT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import EventQueue (EventQueue (initialSpeed, currentSpeed, EventQueue), readEvent, setSpeed, calculateSpeed, HasEventQueue (getEventQueue))
import Control.Concurrent (threadDelay, readMVar, putMVar, forkIO)
import Control.Monad ( forever, void, forM_, unless )
import System.IO (stdout)
import App (MonadQueue (pullEvent), MonadSnake (..), MonadRender (..), gameStep, setSpeedOnScore)
import Linear ( V2(V2), V4(..) )
import Data.Word (Word8)
import Foreign.C (CInt)
import SDL (($=))
import qualified Data.Text as Text
import Data.Bifunctor (Bifunctor(bimap))
import Data.Array (assocs)
import Control.Concurrent.BoundedChan (tryWriteChan)


-- -----------
-- |- Utils -|
-- -----------

-- Defined colors

-- Apple color
red :: V4 Word8
red   = V4 200 30 30 0

-- Snake head color
blue :: V4 Word8
blue  = V4 30 30 200 0

-- Snake body color
green :: V4 Word8
green = V4 30 200 30 0

-- background color
black :: V4 Word8
black = V4 0 0 0 0

-- grid color
white :: V4 Word8
white = V4 255 255 255 0

-- Panel color
gray :: V4 Word8
gray = V4 100 100 100 0


-- Create a rectangle
mkRect :: a -> a -> a -> a-> SDL.Rectangle a
mkRect x y w h = SDL.Rectangle rec_origin rec_size
  where
    rec_origin = SDL.P (SDL.V2 x y)
    rec_size = SDL.V2 w h

-- Create a window, runs the io action and releases resources
withWindow :: MonadIO m => SDL.WindowConfig -> Text -> (SDL.Window -> m a) -> m ()
withWindow cfg title io = do
  w <- SDL.createWindow title cfg
  SDL.showWindow w
  void $ io w
  SDL.destroyWindow w
  SDL.quit

-- Create a renderer, runs the io action and releases resources
withRenderer :: MonadIO m => SDL.RendererConfig -> SDL.Window -> (SDL.Renderer -> m a) -> m a
withRenderer cfg window io = do
  renderer <- SDL.createRenderer window (-1) cfg
  r <- io renderer
  SDL.destroyRenderer renderer
  return r


drawCell :: MonadIO m => V4 Word8 -> SDL.Rectangle CInt -> SDL.Renderer -> m ()
drawCell col r renderer  = do
  SDL.rendererDrawColor renderer $= col -- Set renderer color to White
  SDL.fillRect renderer (Just r)        -- Draw a filled rectangle

-- |---------------|
-- |- User Inputs -|
-- |---------------|

-- This is just for the sake of readability
pattern UpArrow, DownArrow, LeftArrow, RightArrow :: SDL.Event
pattern UpArrow <- SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym SDL.ScancodeUp _ _ )))
pattern DownArrow <- SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym SDL.ScancodeDown _ _ )))
pattern LeftArrow <- SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym SDL.ScancodeLeft _ _ )))
pattern RightArrow <- SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym SDL.ScancodeRight _ _ )))


writeUserInput :: [SDL.Event] -> EventQueue -> IO ()
writeUserInput sdl_events (EventQueue userqueue _ _) =
  forM_ sdl_events $ \e -> do
  case e of
    UpArrow -> void $ tryWriteChan userqueue North
    LeftArrow -> void $ tryWriteChan userqueue West
    RightArrow -> void $ tryWriteChan userqueue East
    DownArrow -> void $ tryWriteChan userqueue South
    _   -> return ()


{-
Defining de App State 
-}


data AppState = AppState GameState RenderState
data SDLConfig = SDLConfig SDL.Window SDL.Renderer Font.Font
data Env = Env BoardInfo EventQueue SDLConfig

getGraphicDevices :: Env -> (SDL.Window , SDL.Renderer, Font.Font)
getGraphicDevices (Env _ _ (SDLConfig win ren font) ) = (win, ren, font)

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
  getBoardInfo (Env b _ _) = b

instance HasEventQueue Env where
  getEventQueue (Env _ q _) = q


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


{-
SDL Utils
-}

renderScore :: (MonadIO m, MonadReader Env m) => Int -> Bool  -> CInt -> SDL.Renderer -> Font.Font -> m (CInt, CInt)
renderScore current_score game_over window_width renderer font = do
  -- Display the score. Create a surface with the text, then copy it to a texture, then render it!
  -- Notice that in order to avoid stretchness, we need to calculate the size of the displayed text
  let game_over_text = if game_over then " Game Over!" else Text.empty
      t = "score: " <> Text.pack (show current_score) <> game_over_text
  (text_width, text_height) <- bimap fromIntegral fromIntegral <$> Font.size font t
  surface <- Font.blended font white t
  texture <- SDL.createTextureFromSurface renderer surface

  -- Draw a grey cell on the top and then the score
  drawCell gray (mkRect 0 0 window_width text_height) renderer
  SDL.copy renderer texture Nothing (Just $ mkRect 0 0 text_width text_height) -- Put the text on the top left corner

  SDL.freeSurface surface
  SDL.destroyTexture texture

  return (text_width, text_height)

-- | This function composes the whole scene
renderBoardSDL :: (MonadIO m, MonadReader Env m) => RenderState -> m ()
renderBoardSDL  (RenderState ar game_over current_score) = do
    (window, renderer, font) <- asks getGraphicDevices
    BoardInfo board_width board_height <- asks getBoardInfo
    SDL.clear renderer                                                -- initialize sdl's render backbuffer
    V2 window_width window_height <- SDL.get $ SDL.windowSize window  -- Get windows attributes

    -- Render the up panel
    (text_width, text_height) <- renderScore current_score game_over window_width renderer font

    -- Print the board. Notice we need to manually reduce windows height
    let xSize = window_width `quot` fromIntegral board_width                   -- Size of the squares of the grid. Essentially, divide the
        ySize = (window_height - text_height) `quot` fromIntegral board_height -- size of the window by the number of cells
    forM_ (assocs ar) $ \((a, b), cell) -> do
      let coordX = xSize * (fromIntegral b - 1)   -- TODO: x and y coordinates are swaped w.r.t. tui version
          coordY = ySize * (fromIntegral a - 1) + text_height --
          r = mkRect coordX coordY xSize ySize  -- Create a rectangle in the adecuate coordintates
      case cell of
          Empty -> do
              SDL.rendererDrawColor renderer $= white -- Set renderer color to White
              SDL.drawRect renderer (Just r)          -- Draw a non-filled rectangle
          SnakeHead -> drawCell blue  r renderer
          Snake     -> drawCell green r renderer
          Apple     -> drawCell red   r renderer

    SDL.rendererDrawColor renderer $= black  -- Set color to background. Notice that previous calls to rendererDrawColor are limited to some rectangles, but most of the screen is empty, hence we must call some background color
    SDL.present renderer                     -- draws sdl's render backbuffer

-- | This function renders the board if no gameover
renderSDL :: (MonadIO m, MonadReader Env m) => RenderState  -> m ()
renderSDL render_state@(RenderState _ game_over n) =
  if game_over
    then asks getBoardInfo >>= \bi -> renderBoardSDL (RenderState (emptyGrid bi) game_over n)
    else renderBoardSDL render_state


instance (Monad m, MonadIO m) => MonadRender (App m) where
  render = do
    rs     <- gets getRenderState
    renderSDL rs

{-
Run the process
-}

gameloop :: ( MonadIO m
            , MonadReader e m
            , MonadState AppState m
            , MonadRender m
            , HasEventQueue e
            , MonadSnake m
            , MonadQueue m) => m ()
gameloop = do
    -- Update speed. Notice that SDL.pollEvents must be run in the same 
    -- thread as the rendering process, that is different from the TUI version
    -- In which we initialize a concurrent writeInput process. the consequence
    -- is that threadDelay now affects the rendering speed, and the event polling.
    -- To for example, using threadDelay in the middle of SDL.pollEvents and gameStep
    -- causes a little bit of unresponsiveness. In general, snake is played fast enough 
    -- not be a problem, but if you test the application at low speed, in becomes buggy.

    new_speed <- setSpeedOnScore
    liftIO $ threadDelay new_speed

    -- Read sdl events and push them into the application queue
    sdl_events  <- SDL.pollEvents
    event_queue <- asks getEventQueue
    liftIO $ forkIO $ writeUserInput sdl_events event_queue -- write sdl into game queue

    -- check for "quit" event
    let eventIsQPress event =
          case SDL.eventPayload event of
            SDL.QuitEvent -> True
            _ -> False
    let qPressed = any eventIsQPress sdl_events 

    gameStep

    -- Quit on exit event
    unless qPressed gameloop


run :: Env -> AppState -> IO ()
run env app = runApp gameloop `runReaderT` env `evalStateT` app