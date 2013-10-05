{-# LANGUAGE DeriveDataTypeable #-}

module Yage.Core.Application.Types
    ( Application, Window(..), ApplicationState(..), ApplicationEnv(..), Event(..)
    , GLFW.FocusState
    , GLFW.IconifyState
    , GLFW.MouseButton, GLFW.MouseButtonState
    , GLFW.CursorState
    , GLFW.Key, GLFW.KeyState, GLFW.ModifierKeys
    , GLFW.Error
    ) where

import           Control.Monad.Exception
import           Control.Concurrent.STM       (TQueue)
import           Control.Monad.RWS.Strict     (RWST)
import           Data.Trie                    (Trie)

import qualified Graphics.UI.GLFW             as GLFW


type Application l a = EMT l (RWST ApplicationEnv () ApplicationState IO) a


data Window = Window
    { winTitle  :: !String
    , winSize   :: !(Int, Int)
    , winHandle :: !GLFW.Window
    }
    deriving (Show)


data ApplicationState = ApplicationState
    { appTitle   :: !String
    , appWindows :: Trie (Window)
    }
    deriving (Show)

data ApplicationEnv = ApplicationEnv
    { appEventQ :: TQueue Event
    }


-- mainly inspired by glfw-b-demo
data Event = EventError             !GLFW.Error !String
           | EventWindowPosition    !GLFW.Window !Int !Int
           | EventWindowSize        !GLFW.Window !Int !Int
           | EventWindowClose       !GLFW.Window
           | EventWindowRefresh     !GLFW.Window
           | EventWindowFocus       !GLFW.Window !GLFW.FocusState
           | EventWindowIconify     !GLFW.Window !GLFW.IconifyState
           | EventFramebufferSize   !GLFW.Window !Int !Int
           | EventMousePosition     !GLFW.Window !Double !Double
           | EventMouseEnter        !GLFW.Window !GLFW.CursorState
           | EventMouseButton       !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
           | EventMouseScroll       !GLFW.Window !Double !Double
           | EventKey               !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
           | EventChar              !GLFW.Window !Char
           deriving (Typeable, Show)

