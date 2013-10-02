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

{-# INLINE setWindowPosCallback #-}
setWindowPosCallback :: (Throws InternalException l) => Window -> Maybe GLFW.WindowPosCallback -> Application l ()
setWindowPosCallback = windowCallbackMapping GLFW.setWindowPosCallback

{-# INLINE setWindowSizeCallback #-}
setWindowSizeCallback :: (Throws InternalException l) => Window -> Maybe GLFW.WindowSizeCallback -> Application l ()
setWindowSizeCallback = windowCallbackMapping GLFW.setWindowSizeCallback

{-# INLINE setWindowCloseCallback #-}
setWindowCloseCallback :: (Throws InternalException l) => Window -> Maybe GLFW.WindowCloseCallback -> Application l ()
setWindowCloseCallback = windowCallbackMapping GLFW.setWindowCloseCallback

{-# INLINE setWindowRefreshCallback #-}
setWindowRefreshCallback :: (Throws InternalException l) => Window -> Maybe GLFW.WindowRefreshCallback -> Application l ()
setWindowRefreshCallback = windowCallbackMapping GLFW.setWindowRefreshCallback

{-# INLINE setWindowFocusCallback #-}
setWindowFocusCallback :: (Throws InternalException l) => Window -> Maybe GLFW.WindowFocusCallback -> Application l ()
setWindowFocusCallback = windowCallbackMapping GLFW.setWindowFocusCallback

{-# INLINE setWindowIconifyCallback #-}
setWindowIconifyCallback :: (Throws InternalException l) => Window -> Maybe GLFW.WindowIconifyCallback -> Application l ()
setWindowIconifyCallback = windowCallbackMapping GLFW.setWindowIconifyCallback

{-# INLINE setFramebufferSizeCallback #-}
setFramebufferSizeCallback :: (Throws InternalException l) => Window -> Maybe GLFW.FramebufferSizeCallback -> Application l ()
setFramebufferSizeCallback = windowCallbackMapping GLFW.setFramebufferSizeCallback

--------------------------------------------------------------------------------
-- input callbacks
{-# INLINE setKeyCallback #-}
setKeyCallback :: (Throws InternalException l) => Window -> Maybe GLFW.KeyCallback -> Application l ()
setKeyCallback = windowCallbackMapping GLFW.setKeyCallback

{-# INLINE setCharCallback #-}
setCharCallback :: (Throws InternalException l) => Window -> Maybe GLFW.CharCallback -> Application l ()
setCharCallback = windowCallbackMapping GLFW.setCharCallback

{-# INLINE setMouseButtonCallback #-}
setMouseButtonCallback :: (Throws InternalException l) => Window -> Maybe GLFW.MouseButtonCallback -> Application l ()
setMouseButtonCallback = windowCallbackMapping GLFW.setMouseButtonCallback

{-# INLINE setCursorPosCallback #-}
setCursorPosCallback :: (Throws InternalException l) =>  Window -> Maybe GLFW.CursorPosCallback -> Application l ()
setCursorPosCallback = windowCallbackMapping GLFW.setCursorPosCallback

{-# INLINE setCursorEnterCallback #-}
setCursorEnterCallback :: (Throws InternalException l) => Window -> Maybe GLFW.CursorEnterCallback -> Application l ()
setCursorEnterCallback = windowCallbackMapping GLFW.setCursorEnterCallback

{-# INLINE setScrollCallback #-}
setScrollCallback :: (Throws InternalException l) => Window -> Maybe GLFW.ScrollCallback -> Application l ()
setScrollCallback = windowCallbackMapping GLFW.setScrollCallback

--------------------------------------------------------------------------------
-- helper

{-# INLINE windowCallbackMapping #-}
windowCallbackMapping :: (Throws InternalException l) => (GLFW.Window -> t -> IO a) -> Window -> t -> Application l a
windowCallbackMapping cbf win cb = glfw . (\w -> cbf w cb) . winHandle $ win
