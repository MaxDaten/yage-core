{-# LANGUAGE FlexibleContexts           #-}

module Yage.Core.Application.Event
    ( registerWindowCallbacks, registerGlobalErrorCallback
    , errorCallback, windowPositionCallback, windowSizeCallback, windowCloseCallback, windowRefreshCallback
    , windowFocusCallback, windowIconifyCallback, framebufferSizeCallback, mouseButtonCallback
    , mousePositionCallback, mouseEnterCallback, scrollCallback, keyCallback, charCallback
    ) where

import           Yage.Prelude

import           Control.Concurrent.STM         (TQueue, atomically, writeTQueue)
import           Control.Monad.Exception
import           Control.Monad.Reader           (asks)

import qualified Graphics.UI.GLFW               as GLFW (Window)

import           Yage.Core.Application.Types
import           Yage.Core.Application.Exception
import           Yage.Core.Application.Logging
import           Yage.Core.GLFW.Callback
--------------------------------------------------------------------------------


errorCallback           :: TQueue Event -> Maybe Logger -> Error -> String                  -> IO ()
windowPositionCallback  :: TQueue Event -> Maybe Logger -> GLFW.Window -> Int -> Int        -> IO ()
windowSizeCallback      :: TQueue Event -> Maybe Logger -> GLFW.Window -> Int -> Int        -> IO ()
windowCloseCallback     :: TQueue Event -> Maybe Logger -> GLFW.Window                      -> IO ()
windowRefreshCallback   :: TQueue Event -> Maybe Logger -> GLFW.Window                      -> IO ()
windowFocusCallback     :: TQueue Event -> Maybe Logger -> GLFW.Window -> FocusState        -> IO ()
windowIconifyCallback   :: TQueue Event -> Maybe Logger -> GLFW.Window -> IconifyState      -> IO ()
framebufferSizeCallback :: TQueue Event -> Maybe Logger -> GLFW.Window -> Int -> Int        -> IO ()
mouseButtonCallback     :: TQueue Event -> Maybe Logger -> GLFW.Window -> MouseButton -> MouseButtonState -> ModifierKeys -> IO ()
mousePositionCallback   :: TQueue Event -> Maybe Logger -> GLFW.Window -> Double -> Double  -> IO ()
mouseEnterCallback      :: TQueue Event -> Maybe Logger -> GLFW.Window -> CursorState       -> IO ()
scrollCallback          :: TQueue Event -> Maybe Logger -> GLFW.Window -> Double -> Double  -> IO ()
keyCallback             :: TQueue Event -> Maybe Logger -> GLFW.Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
charCallback            :: TQueue Event -> Maybe Logger -> GLFW.Window -> Char              -> IO ()

errorCallback           tc ml e s            = queueEvent tc ml $ Event'Error e s
windowPositionCallback  tc ml win x y        = queueEvent tc ml $ Event'WindowPosition win x y
windowSizeCallback      tc ml win w h        = queueEvent tc ml $ Event'WindowSize win w h
windowCloseCallback     tc ml win            = queueEvent tc ml $ Event'WindowClose win
windowRefreshCallback   tc ml win            = queueEvent tc ml $ Event'WindowRefresh win
windowFocusCallback     tc ml win fa         = queueEvent tc ml $ Event'WindowFocus win fa
windowIconifyCallback   tc ml win ia         = queueEvent tc ml $ Event'WindowIconify win ia
framebufferSizeCallback tc ml win w h        = queueEvent tc ml $ Event'FramebufferSize win w h
mouseButtonCallback     tc ml win mb mba mk  = queueEvent tc ml $ Event'MouseButton win mb mba mk
mousePositionCallback   tc ml win x y        = queueEvent tc ml $ Event'MousePosition win x y
mouseEnterCallback      tc ml win ca         = queueEvent tc ml $ Event'MouseEnter win ca
scrollCallback          tc ml win x y        = queueEvent tc ml $ Event'MouseScroll win x y
keyCallback             tc ml win k sc ka mk = queueEvent tc ml $ Event'Key win k sc ka mk
charCallback            tc ml win c          = queueEvent tc ml $ Event'Char win c

--------------------------------------------------------------------------------

queueEvent :: TQueue Event -> Maybe Logger -> Event -> IO ()
queueEvent tc ml e = do
    logEvent ml e
    atomically $ writeTQueue tc e
    where
        -- TODO Pattern-Matching and/or multiplexing
        logEvent :: Maybe Logger -> Event -> IO ()
        logEvent (Just l) e = logL l NOTICE (show e)
        logEvent Nothing _ = return ()

--------------------------------------------------------------------------------

registerGlobalErrorCallback :: (Throws InternalException l) => Maybe Logger -> Application l ()
registerGlobalErrorCallback ml = do
    eventQ <- asks app'eventQ
    setErrorCallback $ Just $ errorCallback eventQ ml

registerWindowCallbacks :: (Throws InternalException l) => Window -> TQueue Event -> Maybe Logger -> Application l ()
registerWindowCallbacks win tq ml = do
    -- annoying setup
    setWindowPositionCallback   win $ Just $ windowPositionCallback tq ml
    setWindowSizeCallback       win $ Just $ windowSizeCallback tq ml
    setWindowCloseCallback      win $ Just $ windowCloseCallback tq ml
    setWindowRefreshCallback    win $ Just $ windowRefreshCallback tq ml
    setWindowFocusCallback      win $ Just $ windowFocusCallback tq ml
    setWindowIconifyCallback    win $ Just $ windowIconifyCallback tq ml
    setFramebufferSizeCallback  win $ Just $ framebufferSizeCallback tq ml

    setKeyCallback              win $ Just $ keyCallback tq ml
    setCharCallback             win $ Just $ charCallback tq ml

    setMouseButtonCallback      win $ Just $ mouseButtonCallback tq ml
    setMousePositionCallback    win $ Just $ mousePositionCallback tq ml
    setMouseEnterCallback       win $ Just $ mouseEnterCallback tq ml
    setScrollCallback           win $ Just $ scrollCallback tq ml
