{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module GUI where

import qualified SDL
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import qualified RenderState as R
import RenderState (BoardInfo (BoardInfo))
import Linear
import SDL (($=))
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Data.Foldable (forM_)
import Data.Array (assocs)
import EventQueue (EventQueue(EventQueue))
import qualified Snake
import Control.Concurrent.BoundedChan (tryWriteChan)
import App (Config, HasConfig (getConfig), HasEventQueue (getQueue))

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

mkRect :: a -> a -> a -> a-> SDL.Rectangle a
mkRect x y w h = SDL.Rectangle rec_origin rec_size
  where
    rec_origin = SDL.P (SDL.V2 x y)
    rec_size = SDL.V2 w h

withWindow :: MonadIO m => SDL.WindowConfig -> Text -> (SDL.Window -> m a) -> m ()
withWindow cfg title io = do
  w <- SDL.createWindow title cfg
  SDL.showWindow w
  void $ io w
  SDL.destroyWindow w

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
data Env = Env Config EventQueue

-- The instances necesary to work within the App Monad
instance HasConfig Env where
  getConfig (Env con _) = con

instance HasEventQueue Env where
  getQueue (Env _ q) = q



-- |-------------|
-- |- Rendering -|
-- |-------------|

renderBoardSDL :: MonadIO m => SDL.Window -> SDL.Renderer -> R.RenderState  -> m ()
renderBoardSDL window renderer (R.RenderState ar (BoardInfo i j) game_over current_score) = do
    SDL.clear renderer                  -- initialize sdl's render backbuffer
    V2 x y <- SDL.get $ SDL.windowSize window
    let xSize = x `quot` fromIntegral i -- Size of the squares of the grid. Essentially, divide the
        ySize = y `quot` fromIntegral j -- size of the window by the number of cells
    forM_ (assocs ar) $ \((a, b), cell) -> do
      let coordX = xSize * (fromIntegral a - 1)
          coordY = ySize * (fromIntegral b - 1)
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

-- |---------------|
-- |- User Inputs -|
-- |---------------|

-- This is just for the sake of readability
pattern UpArrow, DownArrow, LeftArrow, RightArrow :: SDL.Event
pattern UpArrow <- SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym SDL.ScancodeUp _ _ )))
pattern DownArrow <- SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym SDL.ScancodeDown _ _ )))
pattern LeftArrow <- SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym SDL.ScancodeLeft _ _ )))
pattern RightArrow <- SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym SDL.ScancodeRight _ _ )))


writeUserInput :: EventQueue -> IO ()
writeUserInput queue@(EventQueue userqueue _) = do
    e <- SDL.pollEvent
    case e of
      Just UpArrow -> tryWriteChan userqueue Snake.North >> writeUserInput queue
      Just LeftArrow -> tryWriteChan userqueue Snake.West  >> writeUserInput queue
      Just RightArrow -> tryWriteChan userqueue Snake.East  >> writeUserInput queue
      Just DownArrow -> tryWriteChan userqueue Snake.South >> writeUserInput queue
      _   -> writeUserInput queue

