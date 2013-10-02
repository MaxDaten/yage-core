{-# LANGUAGE FlexibleContexts           #-}
module Yage.Core.GLFW.Callback
    ( GLFW.WindowPosCallback, setWindowPosCallback
    , GLFW.WindowSizeCallback, setWindowSizeCallback
    , GLFW.WindowCloseCallback, setWindowCloseCallback
    , GLFW.WindowRefreshCallback, setWindowRefreshCallback
    , GLFW.WindowFocusCallback, setWindowFocusCallback
    , GLFW.WindowIconifyCallback, setWindowIconifyCallback
    , GLFW.FramebufferSizeCallback, setFramebufferSizeCallback

    , GLFW.KeyCallback, setKeyCallback
    , GLFW.CharCallback, setCharCallback
    , GLFW.MouseButtonCallback, setMouseButtonCallback
    , GLFW.CursorPosCallback, setCursorPosCallback
    , GLFW.CursorEnterCallback, setCursorEnterCallback
    , GLFW.ScrollCallback, setScrollCallback
    ) where

import           Control.Monad.Exception

import qualified Graphics.UI.GLFW as GLFW

import           Yage.Core.Application.Types
import           Yage.Core.Application.Exception
import           Yage.Core.GLFW.Base


--------------------------------------------------------------------------------
-- window callbacks

setWindowPosCallback :: (Throws InternalException l) => Window -> Maybe GLFW.WindowPosCallback -> Application l ()
setWindowPosCallback = windowCallbackMapping GLFW.setWindowPosCallback


setWindowSizeCallback :: (Throws InternalException l) => Window -> Maybe GLFW.WindowSizeCallback -> Application l ()
setWindowSizeCallback = windowCallbackMapping GLFW.setWindowSizeCallback


setWindowCloseCallback :: (Throws InternalException l) => Window -> Maybe GLFW.WindowCloseCallback -> Application l ()
setWindowCloseCallback = windowCallbackMapping GLFW.setWindowCloseCallback

setWindowRefreshCallback :: (Throws InternalException l) => Window -> Maybe GLFW.WindowRefreshCallback -> Application l ()
setWindowRefreshCallback = windowCallbackMapping GLFW.setWindowRefreshCallback


setWindowFocusCallback :: (Throws InternalException l) => Window -> Maybe GLFW.WindowFocusCallback -> Application l ()
setWindowFocusCallback = windowCallbackMapping GLFW.setWindowFocusCallback

setWindowIconifyCallback :: (Throws InternalException l) => Window -> Maybe GLFW.WindowIconifyCallback -> Application l ()
setWindowIconifyCallback = windowCallbackMapping GLFW.setWindowIconifyCallback

setFramebufferSizeCallback :: (Throws InternalException l) => Window -> Maybe GLFW.FramebufferSizeCallback -> Application l ()
setFramebufferSizeCallback = windowCallbackMapping GLFW.setFramebufferSizeCallback


windowCallbackMapping :: (Throws InternalException l) => (GLFW.Window -> t -> IO a) -> Window -> t -> Application l a
windowCallbackMapping cbf win cb = glfw . (\w -> cbf w cb) . winHandle $ win

--------------------------------------------------------------------------------
-- input callbacks

setKeyCallback :: (Throws InternalException l) => Window -> Maybe GLFW.KeyCallback -> Application l ()
setKeyCallback = windowCallbackMapping GLFW.setKeyCallback

setCharCallback :: (Throws InternalException l) => Window -> Maybe GLFW.CharCallback -> Application l ()
setCharCallback = windowCallbackMapping GLFW.setCharCallback

setMouseButtonCallback :: (Throws InternalException l) => Window -> Maybe GLFW.MouseButtonCallback -> Application l ()
setMouseButtonCallback = windowCallbackMapping GLFW.setMouseButtonCallback

setCursorPosCallback :: (Throws InternalException l) =>  Window -> Maybe GLFW.CursorPosCallback -> Application l ()
setCursorPosCallback = windowCallbackMapping GLFW.setCursorPosCallback

setCursorEnterCallback :: (Throws InternalException l) => Window -> Maybe GLFW.CursorEnterCallback -> Application l ()
setCursorEnterCallback = windowCallbackMapping GLFW.setCursorEnterCallback

setScrollCallback :: (Throws InternalException l) => Window -> Maybe GLFW.ScrollCallback -> Application l ()
setScrollCallback = windowCallbackMapping GLFW.setScrollCallback

