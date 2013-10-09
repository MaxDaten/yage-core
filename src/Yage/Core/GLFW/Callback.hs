{-# LANGUAGE FlexibleContexts           #-}
module Yage.Core.GLFW.Callback
    ( setWindowPositionCallback
    , setWindowSizeCallback
    , setWindowCloseCallback
    , setWindowRefreshCallback
    , setWindowFocusCallback
    , setWindowIconifyCallback
    , setFramebufferSizeCallback

    , setKeyCallback
    , setCharCallback
    , setMouseButtonCallback
    , setMousePositionCallback
    , setMouseEnterCallback
    , setScrollCallback

    , setErrorCallback
    , WindowPositionCallback, GLFW.WindowSizeCallback, GLFW.WindowCloseCallback, GLFW.WindowRefreshCallback
    , GLFW.WindowFocusCallback, GLFW.WindowIconifyCallback, GLFW.FramebufferSizeCallback
    , GLFW.KeyCallback, GLFW.CharCallback
    , GLFW.MouseButtonCallback, MousePositionCallback, MouseEnterCallback, GLFW.ScrollCallback
    , GLFW.ErrorCallback
    , GLFW.FocusState, GLFW.IconifyState, GLFW.MouseButton, GLFW.MouseButtonState, GLFW.ModifierKeys
    , GLFW.CursorState, GLFW.Key, GLFW.KeyState
    ) where

import Yage.Prelude

import           Control.Monad.Exception
import qualified Graphics.UI.GLFW as GLFW

import           Yage.Core.Application.Types
import           Yage.Core.Application.Exception
import           Yage.Core.GLFW.Base


--------------------------------------------------------------------------------
-- window callbacks

{-# INLINE setWindowPositionCallback  #-}
{-# INLINE setWindowSizeCallback      #-}
{-# INLINE setWindowCloseCallback     #-}
{-# INLINE setWindowRefreshCallback   #-}
{-# INLINE setWindowFocusCallback     #-}
{-# INLINE setWindowIconifyCallback   #-}
{-# INLINE setFramebufferSizeCallback #-}
setWindowPositionCallback  :: (Throws InternalException l) => Window -> Maybe WindowPositionCallback       -> Application l ()
setWindowSizeCallback      :: (Throws InternalException l) => Window -> Maybe GLFW.WindowSizeCallback      -> Application l ()
setWindowCloseCallback     :: (Throws InternalException l) => Window -> Maybe GLFW.WindowCloseCallback     -> Application l ()
setWindowRefreshCallback   :: (Throws InternalException l) => Window -> Maybe GLFW.WindowRefreshCallback   -> Application l ()
setWindowFocusCallback     :: (Throws InternalException l) => Window -> Maybe GLFW.WindowFocusCallback     -> Application l ()
setWindowIconifyCallback   :: (Throws InternalException l) => Window -> Maybe GLFW.WindowIconifyCallback   -> Application l ()
setFramebufferSizeCallback :: (Throws InternalException l) => Window -> Maybe GLFW.FramebufferSizeCallback -> Application l ()

setWindowPositionCallback  = windowCallbackMapping GLFW.setWindowPosCallback
setWindowSizeCallback      = windowCallbackMapping GLFW.setWindowSizeCallback
setWindowCloseCallback     = windowCallbackMapping GLFW.setWindowCloseCallback
setWindowRefreshCallback   = windowCallbackMapping GLFW.setWindowRefreshCallback
setWindowFocusCallback     = windowCallbackMapping GLFW.setWindowFocusCallback
setWindowIconifyCallback   = windowCallbackMapping GLFW.setWindowIconifyCallback
setFramebufferSizeCallback = windowCallbackMapping GLFW.setFramebufferSizeCallback

--------------------------------------------------------------------------------
-- input callbacks

{-# INLINE setKeyCallback           #-}
{-# INLINE setCharCallback          #-}
{-# INLINE setMouseButtonCallback   #-}
{-# INLINE setMousePositionCallback #-}
{-# INLINE setMouseEnterCallback    #-}
{-# INLINE setScrollCallback        #-}
setKeyCallback           :: (Throws InternalException l) => Window -> Maybe GLFW.KeyCallback         -> Application l ()
setCharCallback          :: (Throws InternalException l) => Window -> Maybe GLFW.CharCallback        -> Application l ()
setMouseButtonCallback   :: (Throws InternalException l) => Window -> Maybe GLFW.MouseButtonCallback -> Application l ()
setMousePositionCallback :: (Throws InternalException l) => Window -> Maybe MousePositionCallback    -> Application l ()
setMouseEnterCallback    :: (Throws InternalException l) => Window -> Maybe MouseEnterCallback       -> Application l ()
setScrollCallback        :: (Throws InternalException l) => Window -> Maybe GLFW.ScrollCallback      -> Application l ()

setKeyCallback           = windowCallbackMapping GLFW.setKeyCallback
setCharCallback          = windowCallbackMapping GLFW.setCharCallback
setMouseButtonCallback   = windowCallbackMapping GLFW.setMouseButtonCallback
setMousePositionCallback = windowCallbackMapping GLFW.setCursorPosCallback
setMouseEnterCallback    = windowCallbackMapping GLFW.setCursorEnterCallback
setScrollCallback        = windowCallbackMapping GLFW.setScrollCallback

--------------------------------------------------------------------------------

{-# INLINE setErrorCallback #-}
setErrorCallback :: (Throws InternalException l) => Maybe GLFW.ErrorCallback -> Application l ()
setErrorCallback = glfw . GLFW.setErrorCallback

--------------------------------------------------------------------------------
-- helper

{-# INLINE windowCallbackMapping #-}
windowCallbackMapping :: (Throws InternalException l) => (GLFW.Window -> t -> IO a) -> Window -> t -> Application l a
windowCallbackMapping cbf win cb = glfw . (\w -> cbf w cb) . win'handle $ win

-- type renaming
type MousePositionCallback  = GLFW.CursorPosCallback
type MouseEnterCallback     = GLFW.CursorEnterCallback
type WindowPositionCallback = GLFW.WindowPosCallback

