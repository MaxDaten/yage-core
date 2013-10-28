{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DeriveDataTypeable     #-}
module Yage.Core.Application.EventTypes
    ( module Yage.Core.Application.EventTypes
    , module GLFWEventTypes
    ) where


import Yage.Prelude
import Data.Data

import Control.Lens

import qualified Graphics.UI.GLFW             as GLFW (Window)
import           Graphics.UI.GLFW             as GLFWEventTypes ( FocusState(..), IconifyState, MouseButton, MouseButtonState
                                              , CursorState, KeyState, ModifierKeys, Error, Key(..))



data EWindowPosition = EWindowPosition
    { __winX             :: !Int
    , __winY             :: !Int
    } deriving (Show, Ord, Eq, Typeable)
makeLenses ''EWindowPosition

data EWindowSize = EWindowSize
    { __winWidth         :: !Int
    , __winHeight        :: !Int
    } deriving (Show, Ord, Eq, Typeable, Data)
makeLenses ''EWindowSize

data EWindowFocus = EWindowFocus
    { __focusState       :: !FocusState }
    deriving (Show, Ord, Eq, Typeable, Data)
makeLenses ''EWindowFocus

data EWindowIconify = EWindowIconify
    { __iconifyState     :: !IconifyState }
    deriving (Show, Ord, Eq, Typeable, Data)
makeLenses ''EWindowIconify

data EFramebufferSize = EFramebufferSize
    { __fbWidth          :: !Int
    , __fbHeight         :: !Int
    } deriving (Show, Ord, Eq, Typeable, Data)
makeLenses ''EFramebufferSize

data EMousePosition = EMousePosition
    { __mousePosX        :: !Double
    , __mousePosY        :: !Double
    } deriving (Show, Ord, Eq, Typeable, Data)
makeLenses ''EMousePosition

data EMouseEnter = EMouseEnter
    { __cursorState      :: !CursorState
    } deriving (Show, Ord, Eq, Typeable, Data)
makeLenses ''EMouseEnter

data EMouseButton = EMouseButton
    { __mouseButton      :: !MouseButton
    , __mouseButtonState :: !MouseButtonState
    , __mouseModifier    :: !ModifierKeys
    } deriving (Show, Ord, Eq, Typeable, Data)
makeLenses ''EMouseButton

data EMouseScroll = EMouseScroll
    { __mouseScrollX    :: !Double
    , __mouseScrollY    :: !Double
    } deriving (Show, Ord, Eq, Typeable, Data)
makeLenses ''EMouseScroll

data EKey = EKey
    { __key             :: !Key
    , __code            :: !Int
    , __keyState        :: !KeyState
    , __keyModifier     :: !ModifierKeys
    } deriving (Show, Ord, Eq, Typeable, Data)
makeLenses ''EKey

data EChar = EChar
    { __char            :: Char }
    deriving (Show, Ord, Eq, Typeable, Data)
makeLenses ''EChar

--------------------------------------------------------------------------------

-- mainly inspired by glfw-b-demo
data Event = EventError             !Error !String
           | EventWindowPosition    !GLFW.Window !EWindowPosition
           | EventWindowSize        !GLFW.Window !EWindowSize
           | EventWindowClose       !GLFW.Window
           | EventWindowRefresh     !GLFW.Window
           | EventWindowFocus       !GLFW.Window !EWindowFocus
           | EventWindowIconify     !GLFW.Window !EWindowIconify
           | EventFramebufferSize   !GLFW.Window !EFramebufferSize
           | EventMousePosition     !GLFW.Window !EMousePosition
           | EventMouseEnter        !GLFW.Window !EMouseEnter
           | EventMouseButton       !GLFW.Window !EMouseButton
           | EventMouseScroll       !GLFW.Window !EMouseScroll
           | EventKey               !GLFW.Window !EKey
           | EventChar              !GLFW.Window !EChar
           deriving (Typeable, Show, Ord, Eq)
