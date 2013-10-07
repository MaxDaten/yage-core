{-# LANGUAGE FlexibleContexts           #-}

module Yage.Core.Application.Event
    ( registerWindowCallbacks, registerGlobalErrorCallback
    , errorCallback, windowPositionCallback, windowSizeCallback, windowCloseCallback, windowRefreshCallback  
    , windowFocusCallback, windowIconifyCallback, framebufferSizeCallback, mouseButtonCallback 
    , mousePositionCallback, mouseEnterCallback, scrollCallback, keyCallback, charCallback
    ) where

import           Control.Concurrent.STM         (TQueue, atomically, writeTQueue)
import           Control.Monad.Exception
import           Control.Monad.Reader           (asks)

import qualified Graphics.UI.GLFW               as GLFW (Window)

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

errorCallback           tc e s            = queueEvent tc $ Event'Error e s
windowPositionCallback  tc win x y        = queueEvent tc $ Event'WindowPosition win x y
windowSizeCallback      tc win w h        = queueEvent tc $ Event'WindowSize win w h
windowCloseCallback     tc win            = queueEvent tc $ Event'WindowClose win
windowRefreshCallback   tc win            = queueEvent tc $ Event'WindowRefresh win
windowFocusCallback     tc win fa         = queueEvent tc $ Event'WindowFocus win fa
windowIconifyCallback   tc win ia         = queueEvent tc $ Event'WindowIconify win ia
framebufferSizeCallback tc win w h        = queueEvent tc $ Event'FramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = queueEvent tc $ Event'MouseButton win mb mba mk
mousePositionCallback   tc win x y        = queueEvent tc $ Event'MousePosition win x y
mouseEnterCallback      tc win ca         = queueEvent tc $ Event'MouseEnter win ca
scrollCallback          tc win x y        = queueEvent tc $ Event'MouseScroll win x y
keyCallback             tc win k sc ka mk = queueEvent tc $ Event'Key win k sc ka mk
charCallback            tc win c          = queueEvent tc $ Event'Char win c

--------------------------------------------------------------------------------

queueEvent :: TQueue Event -> Event -> IO ()
queueEvent tc = atomically . (writeTQueue tc)

--------------------------------------------------------------------------------

registerGlobalErrorCallback :: (Throws InternalException l) => Application l ()
registerGlobalErrorCallback = do
    eventQ <- asks app'eventQ
    setErrorCallback $ Just $ errorCallback eventQ

registerWindowCallbacks :: (Throws InternalException l) => Window -> TQueue Event -> Application l ()
registerWindowCallbacks win tq = do
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