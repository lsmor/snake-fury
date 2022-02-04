{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module GUI where

import qualified SDL
import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import qualified RenderState as R
import RenderState (emptyGrid, BoardInfo (BoardInfo))
import Linear
import SDL (($=))
import Control.Concurrent (threadDelay)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Data.Foldable (forM_)
import Data.Array (assocs)

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
mkRect x y w h = SDL.Rectangle o z
  where
    o = SDL.P (SDL.V2 x y)
    z = SDL.V2 w h

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


drawGrid :: MonadIO m => V4 Word8 -> SDL.Rectangle CInt -> SDL.Renderer -> m ()
drawGrid col r renderer  = do
  SDL.rendererDrawColor renderer $= col
  SDL.fillRect renderer (Just r)


renderBoardSDL :: MonadIO m => SDL.Window -> SDL.Renderer -> R.RenderState  -> m ()
renderBoardSDL window renderer (R.RenderState ar (BoardInfo i j) b n) = do
    SDL.clear renderer
    V2 x y <- SDL.get $ SDL.windowSize window
    let xSize = x `quot` fromIntegral i -- Size of the squares of the grid.
        ySize = y `quot` fromIntegral j
    forM_ (assocs ar) $ \((a, b), cell) -> do
      let coordX = xSize * (fromIntegral a - 1)
          coordY = ySize * (fromIntegral b - 1)
          r = mkRect coordX coordY xSize ySize
      case cell of
          R.Empty -> do
              SDL.rendererDrawColor renderer $= white
              SDL.drawRect renderer (Just r)
          R.SnakeHead -> drawGrid blue  r renderer
          R.Snake     -> drawGrid green r renderer
          R.Apple     -> drawGrid red   r renderer
      
    SDL.rendererDrawColor renderer $= black      
    SDL.present renderer