{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# OPTIONS_GHC -pgmPcpphs -optP--cpp -optP-ansi #-}
module Yage.Core.Application.EventTypes
    ( module Yage.Core.Application.EventTypes
    , module GLFWEventTypes
    ) where


import Yage.Prelude
import Data.Data

import Control.Lens

import qualified Graphics.UI.GLFW             as GLFW (Window)
import           Graphics.UI.GLFW             as GLFWEventTypes ( FocusState(..), IconifyState(..), MouseButton(..), MouseButtonState(..)
                                              , CursorState(..), KeyState(..), ModifierKeys(..), Error(..), Key(..))



data EWindowPosition = EWindowPosition
    { _winX             :: !Int
    , _winY             :: !Int
    } deriving (Show, Ord, Eq, Typeable)
makeLenses ''EWindowPosition

data EWindowSize = EWindowSize
    { _winWidth         :: !Int
    , _winHeight        :: !Int
    } deriving (Show, Ord, Eq, Typeable, Data)
makeLenses ''EWindowSize

data EWindowFocus = EWindowFocus
    { _focusState       :: !FocusState }
    deriving (Show, Ord, Eq, Typeable, Data)
makeLenses ''EWindowFocus

data EWindowIconify = EWindowIconify
    { _iconifyState     :: !IconifyState }
    deriving (Show, Ord, Eq, Typeable, Data)
makeLenses ''EWindowIconify

data EFramebufferSize = EFramebufferSize
    { _fbWidth          :: !Int
    , _fbHeight         :: !Int
    } deriving (Show, Ord, Eq, Typeable, Data)
makeLenses ''EFramebufferSize

data EMousePosition = EMousePosition
    { _mousePosX        :: !Double
    , _mousePosY        :: !Double
    } deriving (Show, Ord, Eq, Typeable, Data)
makeLenses ''EMousePosition

data EMouseEnter = EMouseEnter
    { _cursorState      :: !CursorState
    } deriving (Show, Ord, Eq, Typeable, Data)
makeLenses ''EMouseEnter

data EMouseButton = EMouseButton
    { _mouseButton      :: !MouseButton
    , _mouseButtonState :: !MouseButtonState
    , _mouseModifier    :: !ModifierKeys
    } deriving (Show, Ord, Eq, Typeable, Data)
makeLenses ''EMouseButton

data EMouseScroll = EMouseScroll
    { _mouseScrollX    :: !Double
    , _mouseScrollY    :: !Double
    } deriving (Show, Ord, Eq, Typeable, Data)
makeLenses ''EMouseScroll

data EKey = EKey
    { _key             :: !Key
    , _code            :: !Int
    , _keyState        :: !KeyState
    , _keyModifier     :: !ModifierKeys
    } deriving (Show, Ord, Eq, Typeable, Data)
makeLenses ''EKey

data EChar = EChar
    { _char            :: Char }
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
--------------------------------------------------------------------------------

-- some predicates

#define DO(ISEVENT) \
    ISEVENT(EventError) \
    ISEVENT(EventWindowPosition) \
    ISEVENT(EventWindowSize) \
    ISEVENT(EventWindowClose) \
    ISEVENT(EventWindowFocus) \
    ISEVENT(EventWindowIconify) \
    ISEVENT(EventFramebufferSize) \
    ISEVENT(EventMousePosition) \
    ISEVENT(EventMouseEnter) \
    ISEVENT(EventMouseButton) \
    ISEVENT(EventMouseScroll) \
    ISEVENT(EventKey) \
    ISEVENT(EventChar)

#define ISEVENT(eventname) \
    is##eventname :: Event -> Bool; \
    is##eventname eventname##{} = True; \
    is##eventname _ = False; \

DO(ISEVENT)
#undef ISEVENT
#undef DO

