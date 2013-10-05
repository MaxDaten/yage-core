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
data Event = Event'Error             !GLFW.Error !String
           | Event'WindowPosition    !GLFW.Window !Int !Int
           | Event'WindowSize        !GLFW.Window !Int !Int
           | Event'WindowClose       !GLFW.Window
           | Event'WindowRefresh     !GLFW.Window
           | Event'WindowFocus       !GLFW.Window !GLFW.FocusState
           | Event'WindowIconify     !GLFW.Window !GLFW.IconifyState
           | Event'FramebufferSize   !GLFW.Window !Int !Int
           | Event'MousePosition     !GLFW.Window !Double !Double
           | Event'MouseEnter        !GLFW.Window !GLFW.CursorState
           | Event'MouseButton       !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
           | Event'MouseScroll       !GLFW.Window !Double !Double
           | Event'Key               !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
           | Event'Char              !GLFW.Window !Char
           deriving (Typeable, Show)

