{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}

module Yage.Core.Application.Internal.Event
    ( module Yage.Core.Application.Internal.Event
    , module Polling
    , module Types
    ) where

import           Yage.Prelude

import qualified Data.Set                       as Set
import           Data.Monoid

import           Control.Concurrent.STM         (TVar, modifyTVar', atomically)

import           Yage.Core.GLFW.Callback
import           Yage.Core.GLFW.Event           as Polling (pollEvents, waitEvents)

import           Yage.Core.Application.Types    as Types
import           Yage.Core.GLFW.Callback        as Types ( FocusState(..), IconifyState(..), MouseButton(..), MouseButtonState(..), ModifierKeys(..)
                                                         , CursorState(..), Key(..), KeyState(..))
import           Yage.Core.Application.Logging
import           Yage.Core.Application.Exception

import           Linear


registerGlobalErrorCallback :: (Throws InternalException l)
                            => Logger -> Application l ()
registerGlobalErrorCallback logger = do
    --eventQ <- asks appEventQ
    setErrorCallback $ Just $ \err msg -> logL logger CRITICAL (format "GLFWError {0} - {1}" [show err, msg])


--------------------------------------------------------------------------------


windowPositionCallback  :: TVar WindowState -> WindowHandle -> Int -> Int        -> IO ()
windowSizeCallback      :: TVar WindowState -> WindowHandle -> Int -> Int        -> IO ()
windowCloseCallback     :: TVar WindowState -> WindowHandle                      -> IO ()
windowRefreshCallback   :: TVar WindowState -> WindowHandle                      -> IO ()
windowFocusCallback     :: TVar WindowState -> WindowHandle -> FocusState        -> IO ()
windowIconifyCallback   :: TVar WindowState -> WindowHandle -> IconifyState      -> IO ()
windowMouseEnterCallback:: TVar WindowState -> WindowHandle -> CursorState       -> IO ()
--framebufferSizeCallback :: TVar WindowState -> WindowHandle -> Int -> Int        -> IO ()
mouseButtonCallback     :: TVar InputState -> WindowHandle -> MouseButton -> MouseButtonState -> ModifierKeys -> IO ()
mousePositionCallback   :: TVar InputState -> WindowHandle -> Double -> Double  -> IO ()
scrollCallback          :: TVar InputState -> WindowHandle -> Double -> Double  -> IO ()
--keyCallback             :: TVar InputState -> WindowHandle -> Key -> Int -> GLFW.KeyState -> ModifierKeys -> IO ()
--charCallback            :: TVar InputState -> WindowHandle -> Char              -> IO ()

windowPositionCallback winVar _ x y =
    atomically $ modifyTVar' winVar $ winPosition .~ V2 x y

windowSizeCallback winVar _ w h =
    atomically $ modifyTVar' winVar $ winSize .~ V2 w h

windowCloseCallback winVar _ =
    atomically $ modifyTVar' winVar $ winShouldClose .~ True

windowRefreshCallback winVar _ =
    atomically $ modifyTVar' winVar $ winRefresh .~ True

windowFocusCallback winVar _ focusState =
    atomically $ modifyTVar' winVar $ winFocus .~ (focusState == FocusState'Focused)

windowIconifyCallback winVar _ iconifyState =
    atomically $ modifyTVar' winVar $ winIconified .~ (iconifyState == IconifyState'Iconified)

windowMouseEnterCallback winVar _ cursorState =
    atomically $ modifyTVar' winVar $ winMouseIn .~ (cursorState == CursorState'InWindow)
-- framebufferSizeCallback winVar   win w h        = undefined

-- TODO: respect modifier
mouseButtonCallback inputVar _ button MouseButtonState'Pressed _modifier  =
    atomically $ modifyTVar' inputVar $ mouse.mbPressed %~ Set.insert (fromEnum button)
mouseButtonCallback inputVar _ button MouseButtonState'Released _modifier  =
    atomically $ modifyTVar' inputVar $ mouse.mbPressed %~ Set.delete (fromEnum button)

mousePositionCallback inputVar _ x y =
    atomically $ modifyTVar' inputVar $ mouse.mousePosition .~ V2 x y

scrollCallback inputVar _ offX offY        =
    atomically $ modifyTVar' inputVar $ mouse.mouseScroll +~ V2 offX offY

keyCallback inputVar win key _charcode KeyState'Released _modifier =
    atomically $ modifyTVar' inputVar $ keyboard.keyMap %~ Set.delete key


keyCallback inputVar win key _charcode keystate _modifier =
    atomically $ modifyTVar' inputVar $ keyboard.keyMap %~ Set.insert key
--charCallback            inputVar win c          = undefined

--------------------------------------------------------------------------------



registerWindowCallbacks :: (Throws InternalException l) => Window -> Maybe Logger -> Application l ()
registerWindowCallbacks win ml =
    let stateVar = winState win
        inputVar = inputState win
    in do
    -- annoying setup
    setWindowPositionCallback   win $ Just $ windowPositionCallback stateVar
    setWindowSizeCallback       win $ Just $ windowSizeCallback stateVar
    setWindowCloseCallback      win $ Just $ windowCloseCallback stateVar
    setWindowRefreshCallback    win $ Just $ windowRefreshCallback stateVar
    setWindowFocusCallback      win $ Just $ windowFocusCallback stateVar
    setWindowIconifyCallback    win $ Just $ windowIconifyCallback stateVar
    setMouseEnterCallback       win $ Just $ windowMouseEnterCallback stateVar
    --setFramebufferSizeCallback  win $ Just $ framebufferSizeCallback stateVar

    setKeyCallback              win $ Just $ keyCallback inputVar
    --setCharCallback             win $ Just $ charCallback inputVar

    setMouseButtonCallback      win $ Just $ mouseButtonCallback inputVar
    setMousePositionCallback    win $ Just $ mousePositionCallback inputVar
    setScrollCallback           win $ Just $ scrollCallback inputVar

--------------------------------------------------------------------------------
