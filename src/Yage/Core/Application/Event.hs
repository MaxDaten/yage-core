{-# LANGUAGE FlexibleContexts           #-}

module Yage.Core.Application.Event
    ( registerCallbacks
    , errorCallback, windowPositionCallback, windowSizeCallback, windowCloseCallback, windowRefreshCallback  
    , windowFocusCallback, windowIconifyCallback, framebufferSizeCallback, mouseButtonCallback 
    , mousePositionCallback, mouseEnterCallback, scrollCallback, keyCallback, charCallback
    ) where

import           Control.Concurrent.STM       (TQueue, atomically, writeTQueue)
import           Control.Monad.Exception

import qualified Graphics.UI.GLFW             as GLFW

import           Yage.Core.Application.Types
import           Yage.Core.Application.Exception
import           Yage.Core.GLFW.Callback
--------------------------------------------------------------------------------


errorCallback :: TQueue Event -> Error -> String -> IO ()
errorCallback tc e s = queueEvent tc $ EventError e s

windowPositionCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
windowPositionCallback tc win x y = queueEvent tc $ EventWindowPosition win x y

windowSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
windowSizeCallback tc win w h = queueEvent tc $ EventWindowSize win w h

windowCloseCallback :: TQueue Event -> GLFW.Window -> IO ()
windowCloseCallback tc win = queueEvent tc $ EventWindowClose win

windowRefreshCallback :: TQueue Event -> GLFW.Window -> IO ()
windowRefreshCallback tc win = queueEvent tc $ EventWindowRefresh win

windowFocusCallback :: TQueue Event -> GLFW.Window -> GLFW.FocusState -> IO ()
windowFocusCallback tc win fa = queueEvent tc $ EventWindowFocus win fa

windowIconifyCallback :: TQueue Event -> GLFW.Window -> GLFW.IconifyState -> IO ()
windowIconifyCallback tc win ia = queueEvent tc $ EventWindowIconify win ia

framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
framebufferSizeCallback tc win w h = queueEvent tc $ EventFramebufferSize win w h

mouseButtonCallback :: TQueue Event -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
mouseButtonCallback tc win mb mba mk = queueEvent tc $ EventMouseButton win mb mba mk

mousePositionCallback :: TQueue Event -> GLFW.Window -> Double -> Double -> IO ()
mousePositionCallback tc win x y = queueEvent tc $ EventMousePosition win x y

mouseEnterCallback :: TQueue Event -> GLFW.Window -> GLFW.CursorState -> IO ()
mouseEnterCallback tc win ca = queueEvent tc $ EventMouseEnter win ca

scrollCallback :: TQueue Event -> GLFW.Window -> Double -> Double -> IO ()
scrollCallback tc win x y = queueEvent tc $ EventMouseScroll win x y

keyCallback :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback tc win k sc ka mk = queueEvent tc $ EventKey win k sc ka mk

charCallback :: TQueue Event -> GLFW.Window -> Char -> IO ()
charCallback tc win c = queueEvent tc $ EventChar win c

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