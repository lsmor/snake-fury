{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module GUI where

import qualified SDL
import qualified SDL.Font as Font
import Data.Text (Text)
import qualified RenderState as R
import RenderState (BoardInfo (BoardInfo), emptyGrid)
import Linear ( V2(V2), V4(..) )
import SDL (($=))
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Data.Array (assocs)
import EventQueue (EventQueue(EventQueue))
import qualified Snake
import Control.Concurrent.BoundedChan (tryWriteChan)
import App (Config, HasConfig (getConfig), HasEventQueue (getQueue), AppT (runApp), AppState (renderState), MonadRender (render), updateQueueTime, gameStep)
import Control.Monad.Reader
    ( MonadIO(..),
      MonadReader,
      ReaderT(runReaderT),
      forM_,
      void,
      unless,
      asks )
import Control.Monad.State
    ( MonadState,
      StateT,
      gets,
      evalStateT )
import Control.Concurrent (threadDelay)
import qualified Data.Text as Text
import Data.Bifunctor (Bifunctor(bimap))

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
-- |- Environment -|
-- |---------------|

-- | The environment for GUI version of snake
data Env = Env Config EventQueue SDL.Window SDL.Renderer Font.Font

-- The instances necesary to work within the App Monad
instance HasConfig Env where
  getConfig (Env con _ _ _ _) = con

instance HasEventQueue Env where
  getQueue (Env _ q _ _ _) = q


getGraphicDevices :: Env -> (SDL.Window , SDL.Renderer, Font.Font)
getGraphicDevices (Env _ _ win ren font) = (win, ren, font)



-- |-------------|
-- |- Rendering -|
-- |-------------|

-- | This function render the top panel with the score
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
renderBoardSDL :: (MonadIO m, MonadReader Env m) => R.RenderState -> m ()
renderBoardSDL  (R.RenderState ar (BoardInfo board_height board_width) game_over current_score) = do
    (window, renderer, font) <- asks getGraphicDevices
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
          R.Empty -> do
              SDL.rendererDrawColor renderer $= white -- Set renderer color to White
              SDL.drawRect renderer (Just r)          -- Draw a non-filled rectangle
          R.SnakeHead -> drawCell blue  r renderer
          R.Snake     -> drawCell green r renderer
          R.Apple     -> drawCell red   r renderer

    SDL.rendererDrawColor renderer $= black  -- Set color to background. Notice that previous calls to rendererDrawColor are limited to some rectangles, but most of the screen is empty, hence we must call some background color
    SDL.present renderer                     -- draws sdl's render backbuffer

-- | This function renders the board if no gameover
renderSDL :: (MonadIO m, MonadReader Env m) => R.RenderState  -> m ()
renderSDL render_state@(R.RenderState _ bi game_over n) =
  if game_over
    then renderBoardSDL (R.RenderState (emptyGrid bi) bi game_over n)
    else renderBoardSDL render_state

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
writeUserInput sdl_events (EventQueue userqueue _) =
  forM_ sdl_events $ \e -> do
  case e of
    UpArrow -> void $ tryWriteChan userqueue Snake.North
    LeftArrow -> void $ tryWriteChan userqueue Snake.West
    RightArrow -> void $ tryWriteChan userqueue Snake.East
    DownArrow -> void $ tryWriteChan userqueue Snake.South
    _   -> return ()


-- -------------
-- |- GUI App -|
-- -------------

-- Defining TUI
newtype Gui a = Gui { runGui :: AppT Env (StateT AppState IO) a }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadState AppState)

-- How is renderer in the terminal.
instance MonadRender Gui where
  render = do
    rs     <- gets renderState
    renderSDL rs


gameloop :: ( MonadIO m
            , MonadReader e m
            , HasConfig e
            , MonadState AppState m
            , MonadRender m
            , HasEventQueue e) => m ()
gameloop = do
    event_queue <- asks getQueue

    -- Read sdl events
    sdl_events  <- SDL.pollEvents
    let eventIsQPress event =
          case SDL.eventPayload event of
            SDL.QuitEvent -> True
            _ -> False
    let qPressed = any eventIsQPress sdl_events -- check for "quit" event

    -- Update speed
    new_speed <- updateQueueTime

    liftIO $ writeUserInput sdl_events event_queue -- write sdl into game queue
    liftIO $ threadDelay new_speed

    gameStep

    -- Quit on exit event
    unless qPressed gameloop



-- | Given an initial AppState and an Env, it initializes the gameloop
run :: AppState -> Env -> IO ()
run initialState env = flip evalStateT initialState . flip runReaderT env . runApp . runGui $ gameloop
