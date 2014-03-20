{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleInstances  #-}
module Yage.Core.Application.Internal.Types where

import           Yage.Prelude
import           Yage.Lens

import Linear (V2)
import qualified Graphics.UI.GLFW as GLFW


class EventCtr c where
    windowPositionCallback :: c -> Maybe GLFW.WindowPosCallback
    windowPositionCallback _ = Nothing

    windowSizeCallback     :: c -> Maybe GLFW.WindowSizeCallback
    windowSizeCallback     _ = Nothing

    windowCloseCallback    :: c -> Maybe GLFW.WindowCloseCallback
    windowCloseCallback    _ = Nothing

    windowRefreshCallback  :: c -> Maybe GLFW.WindowRefreshCallback
    windowRefreshCallback  _ = Nothing

    windowFocusCallback    :: c -> Maybe GLFW.WindowFocusCallback
    windowFocusCallback    _ = Nothing

    windowIconifyCallback  :: c -> Maybe GLFW.WindowIconifyCallback
    windowIconifyCallback  _ = Nothing

    cursorEnterCallback    :: c -> Maybe GLFW.CursorEnterCallback
    cursorEnterCallback    _ = Nothing

    cursorPositionCallback :: c -> Maybe GLFW.CursorPosCallback
    cursorPositionCallback _ = Nothing

    keyCallback            :: c -> Maybe GLFW.KeyCallback
    keyCallback            _ = Nothing

    mouseButtonCallback    :: c -> Maybe GLFW.MouseButtonCallback
    mouseButtonCallback    _ = Nothing

    scrollCallback         :: c -> Maybe GLFW.ScrollCallback
    scrollCallback         _ = Nothing

    framebufferSizeCallback :: c -> Maybe GLFW.FramebufferSizeCallback
    framebufferSizeCallback _ = Nothing


newtype NullEventController = NullEventController ()
instance EventCtr NullEventController where


data WindowState = WindowState
    { _winPosition    :: !(V2 Int)
    , _winSize        :: !(V2 Int)
    , _winFocus       :: !Bool
    , _winIconified   :: !Bool
    , _winShouldClose :: !Bool
    , _winRefresh     :: !Bool
    , _winMouseIn     :: !Bool
    , _fbSize         :: !(V2 Int)
    }

makeLenses ''WindowState

