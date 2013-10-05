{-# LANGUAGE FlexibleContexts           #-}

module Yage.Core.Application.Event
    ( registerCallbacks
    , errorCallback, windowPositionCallback, windowSizeCallback, windowCloseCallback, windowRefreshCallback  
    , windowFocusCallback, windowIconifyCallback, framebufferSizeCallback, mouseButtonCallback 
    , mousePositionCallback, mouseEnterCallback, scrollCallback, keyCallback, charCallback
    ) where

import           Control.Concurrent.STM       (TQueue, atomically, writeTQueue)
import           Control.Monad.Exception

import qualified Graphics.UI.GLFW             as GLFW (Window)

import           Yage.Core.Application.Types
import           Yage.Core.Application.Exception
import           Yage.Core.GLFW.Callback
--------------------------------------------------------------------------------


errorCallback           :: TQueue Event -> Error -> String                  -> IO ()
windowPositionCallback  :: TQueue Event -> GLFW.Window -> Int -> Int        -> IO ()
windowSizeCallback      :: TQueue Event -> GLFW.Window -> Int -> Int        -> IO ()
windowCloseCallback     :: TQueue Event -> GLFW.Window                      -> IO ()
windowRefreshCallback   :: TQueue Event -> GLFW.Window                      -> IO ()
windowFocusCallback     :: TQueue Event -> GLFW.Window -> FocusState        -> IO ()
windowIconifyCallback   :: TQueue Event -> GLFW.Window -> IconifyState      -> IO ()
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int        -> IO ()
mouseButtonCallback     :: TQueue Event -> GLFW.Window -> MouseButton -> MouseButtonState -> ModifierKeys -> IO ()
mousePositionCallback   :: TQueue Event -> GLFW.Window -> Double -> Double  -> IO ()
mouseEnterCallback      :: TQueue Event -> GLFW.Window -> CursorState       -> IO ()
scrollCallback          :: TQueue Event -> GLFW.Window -> Double -> Double  -> IO ()
keyCallback             :: TQueue Event -> GLFW.Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
charCallback            :: TQueue Event -> GLFW.Window -> Char              -> IO ()

errorCallback           tc e s            = queueEvent tc $ EventError e s
windowPositionCallback  tc win x y        = queueEvent tc $ EventWindowPosition win x y
windowSizeCallback      tc win w h        = queueEvent tc $ EventWindowSize win w h
windowCloseCallback     tc win            = queueEvent tc $ EventWindowClose win
windowRefreshCallback   tc win            = queueEvent tc $ EventWindowRefresh win
windowFocusCallback     tc win fa         = queueEvent tc $ EventWindowFocus win fa
windowIconifyCallback   tc win ia         = queueEvent tc $ EventWindowIconify win ia
framebufferSizeCallback tc win w h        = queueEvent tc $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = queueEvent tc $ EventMouseButton win mb mba mk
mousePositionCallback   tc win x y        = queueEvent tc $ EventMousePosition win x y
mouseEnterCallback      tc win ca         = queueEvent tc $ EventMouseEnter win ca
scrollCallback          tc win x y        = queueEvent tc $ EventMouseScroll win x y
keyCallback             tc win k sc ka mk = queueEvent tc $ EventKey win k sc ka mk
charCallback            tc win c          = queueEvent tc $ EventChar win c

--------------------------------------------------------------------------------

queueEvent :: TQueue Event -> Event -> IO ()
queueEvent tc = atomically . (writeTQueue tc)

--

registerCallbacks :: (Throws InternalException l) => Window -> TQueue Event -> Application l ()
registerCallbacks win tq = do
    -- annoying setup
    setWindowPositionCallback   win $ Just $ windowPositionCallback tq
    setWindowSizeCallback       win $ Just $ windowSizeCallback tq
    setWindowCloseCallback      win $ Just $ windowCloseCallback tq
    setWindowRefreshCallback    win $ Just $ windowRefreshCallback tq
    setWindowFocusCallback      win $ Just $ windowFocusCallback tq
    setWindowIconifyCallback    win $ Just $ windowIconifyCallback tq
    setFramebufferSizeCallback  win $ Just $ framebufferSizeCallback tq
    
    setKeyCallback              win $ Just $ keyCallback tq
    setCharCallback             win $ Just $ charCallback tq
    
    setMouseButtonCallback      win $ Just $ mouseButtonCallback tq
    setMousePositionCallback    win $ Just $ mousePositionCallback tq
    setMouseEnterCallback       win $ Just $ mouseEnterCallback tq
    setScrollCallback           win $ Just $ scrollCallback tq