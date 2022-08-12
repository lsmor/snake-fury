{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Renderer where
import Brick (Widget, CursorLocation, BrickEvent, EventM, AttrMap)
import Brick.Main (App)
import GameState (GameState(..))
import RenderState (RenderState(..), BoardInfo (BoardInfo))
import EventQueue (EventQueue(..))


data AppState = AppState GameState RenderState
data Env = Env BoardInfo EventQueue

myAppDraw :: AppState -> [Widget n]
myAppDraw = undefined

myAppChooseCursor :: AppState -> [CursorLocation n] -> Maybe (CursorLocation n)
myAppChooseCursor = undefined

myAppHandleEvent :: BrickEvent n e -> EventM n s ()
myAppHandleEvent = undefined

myAppStartEvent :: EventM n AppState ()
myAppStartEvent = undefined

myAppAttrMap :: AppState -> AttrMap
myAppAttrMap = undefined