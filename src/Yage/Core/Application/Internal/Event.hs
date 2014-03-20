{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}

module Yage.Core.Application.Internal.Event
    ( module Yage.Core.Application.Internal.Event
    , module Polling
    , module Types
    , module GLFW
    ) where

import           Yage.Prelude
import           Yage.Lens

import           Control.Monad                  (msum)
import           Control.Concurrent.STM         (TVar, modifyTVar', atomically)
import           Linear                         (V2(..))

import           Yage.Core.GLFW.Callback
import           Yage.Core.GLFW.Event           as Polling (pollEvents, waitEvents)

import           Yage.Core.Application.Types    as Types
import           Yage.Core.GLFW.Callback        as GLFW ( FocusState(..), IconifyState(..), MouseButton(..), MouseButtonState(..), ModifierKeys(..)
                                                         , CursorState(..), Key(..), KeyState(..), JoystickButtonState(..))
import           Yage.Core.Application.Logging
import           Yage.Core.Application.Exception



registerGlobalErrorCallback :: (Throws InternalException l)
                            => Logger -> Application l ()
registerGlobalErrorCallback logger = do
    --eventQ <- asks appEventQ
    setErrorCallback $ Just $ \err msg -> logL logger CRITICAL (format "GLFWError {0} - {1}" [show err, msg])


--------------------------------------------------------------------------------


internalWindowPositionCallback  :: TVar WindowState -> Maybe (WindowHandle -> Int -> Int -> IO ())
internalWindowPositionCallback winVar = return $ \_ x y ->
    atomically $ modifyTVar' winVar $ winPosition .~ V2 x y

internalWindowSizeCallback      :: TVar WindowState -> Maybe (WindowHandle -> Int -> Int -> IO ())
internalWindowSizeCallback winVar = return $ \_ w h ->
    atomically $ modifyTVar' winVar $ winSize .~ V2 w h

internalWindowCloseCallback     :: TVar WindowState -> Maybe (WindowHandle -> IO ())
internalWindowCloseCallback winVar = return $ \_ ->
    atomically $ modifyTVar' winVar $ winShouldClose .~ True

internalWindowRefreshCallback   :: TVar WindowState -> Maybe (WindowHandle -> IO ())
internalWindowRefreshCallback winVar = return $ \_ ->
    atomically $ modifyTVar' winVar $ winRefresh .~ True

internalWindowFocusCallback     :: TVar WindowState -> Maybe (WindowHandle -> FocusState -> IO ())
internalWindowFocusCallback winVar = return $ \_ focusState ->
    atomically $ modifyTVar' winVar $ winFocus .~ (focusState == FocusState'Focused)

internalWindowIconifyCallback   :: TVar WindowState -> Maybe (WindowHandle -> IconifyState      -> IO ())
internalWindowIconifyCallback winVar = return $ \_ iconifyState ->
    atomically $ modifyTVar' winVar $ winIconified .~ (iconifyState == IconifyState'Iconified)

internalWindowCursorEnterCallback:: TVar WindowState -> Maybe (WindowHandle -> CursorState       -> IO ())
internalWindowCursorEnterCallback winVar = return $ \_ cursorState ->
    atomically $ modifyTVar' winVar $ winMouseIn .~ (cursorState == CursorState'InWindow)

internalFramebufferSizeCallback :: TVar WindowState -> Maybe (WindowHandle -> Int -> Int -> IO ())
internalFramebufferSizeCallback winVar = return $ \_ w h ->
    atomically $ modifyTVar' winVar $ fbSize .~ V2 w h
{--

--framebufferSizeCallback :: TVar WindowState -> WindowHandle -> Int -> Int        -> IO ()
-- framebufferSizeCallback winVar   win w h        = undefined

-- TODO: respect modifier
mouseButtonCallback     :: TVar InputState -> WindowHandle -> GLFW.MouseButton -> MouseButtonState -> ModifierKeys -> IO ()
mouseButtonCallback inputVar _ button MouseButtonState'Pressed _modifier  =
    atomically $ modifyTVar' inputVar $ mouse.mbPressed %~ Set.insert (fromEnum button)
mouseButtonCallback inputVar _ button MouseButtonState'Released _modifier  =
    atomically $ modifyTVar' inputVar $ mouse.mbPressed %~ Set.delete (fromEnum button)

mousePositionCallback   :: TVar InputState -> WindowHandle -> Double -> Double  -> IO ()
mousePositionCallback inputVar _ x y =
    atomically $ modifyTVar' inputVar $ mouse.mousePosition .~ V2 x y

scrollCallback          :: TVar InputState -> WindowHandle -> Double -> Double  -> IO ()
scrollCallback inputVar _ offX offY        =
    atomically $ modifyTVar' inputVar $ mouse.mouseScroll +~ V2 offX offY

--keyCallback             :: TVar InputState -> WindowHandle -> Key -> Int -> GLFW.KeyState -> ModifierKeys -> IO ()
keyCallback inputVar win key _charcode KeyState'Released _modifier =
    atomically $ modifyTVar' inputVar $ keyboard.keyMap %~ Set.delete key
keyCallback inputVar win key _charcode keystate _modifier =
    atomically $ modifyTVar' inputVar $ keyboard.keyMap %~ Set.insert key

--}
--charCallback            :: TVar InputState -> WindowHandle -> Char              -> IO ()
--charCallback            inputVar win c          = undefined

--------------------------------------------------------------------------------


registerWindowCallbacks :: (Throws InternalException l, EventCtr ectr) => Window -> ectr -> Application l ()
registerWindowCallbacks win@Window{winState} inputCtr = do
    -- annoying setup
    setWindowPositionCallback   win $ msum [ internalWindowPositionCallback winState,    windowPositionCallback inputCtr    ]
    setWindowSizeCallback       win $ msum [ internalWindowSizeCallback winState,        windowSizeCallback inputCtr        ]
    setWindowCloseCallback      win $ msum [ internalWindowCloseCallback winState,       windowCloseCallback inputCtr       ]
    setWindowRefreshCallback    win $ msum [ internalWindowRefreshCallback winState,     windowRefreshCallback inputCtr     ]
    setWindowFocusCallback      win $ msum [ internalWindowFocusCallback winState,       windowFocusCallback inputCtr       ]
    setWindowIconifyCallback    win $ msum [ internalWindowIconifyCallback winState,     windowIconifyCallback inputCtr     ]
    setCursorEnterCallback      win $ msum [ internalWindowCursorEnterCallback winState, cursorEnterCallback inputCtr       ]
    setFramebufferSizeCallback  win $ msum [ internalFramebufferSizeCallback winState,   framebufferSizeCallback inputCtr   ]

    setKeyCallback              win $ keyCallback inputCtr
    --setCharCallback             win $ Just $ charCallback inputVar

    setMouseButtonCallback      win $ mouseButtonCallback inputCtr
    setCursorPositionCallback   win $ cursorPositionCallback inputCtr
    setScrollCallback           win $ scrollCallback inputCtr

--------------------------------------------------------------------------------
