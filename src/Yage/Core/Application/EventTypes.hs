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

isEventError, isEventWindowPosition, isEventWindowSize, isEventWindowClose  :: Event -> Bool
isEventWindowRefresh, isEventWindowFocus, isEventWindowIconify, isEventFramebufferSize  :: Event -> Bool 
isEventMousePosition, isEventMouseEnter, isEventMouseButton, isEventMouseScroll :: Event -> Bool
isEventKey, isEventChar :: Event -> Bool

isEventError            EventError{}           = True
isEventError _                                 = False

isEventWindowPosition   EventWindowPosition{}  = True
isEventWindowPosition _                        = False

isEventWindowIconify    EventWindowIconify{}   = True
isEventWindowIconify _                         = False

isEventWindowSize       EventWindowSize{}      = True
isEventWindowSize _                            = False

isEventWindowClose      EventWindowClose{}     = True
isEventWindowClose _                           = False

isEventWindowRefresh    EventWindowRefresh{}   = True
isEventWindowRefresh _                         = False

isEventWindowFocus      EventWindowFocus{}     = True
isEventWindowFocus _                           = False

isEventFramebufferSize  EventFramebufferSize{} = True
isEventFramebufferSize _                       = False

isEventMousePosition    EventMousePosition{}   = True
isEventMousePosition _                         = False

isEventMouseEnter       EventMousePosition{}   = True
isEventMouseEnter _                            = False

isEventMouseButton      EventMouseButton{}     = True
isEventMouseButton _                           = False

isEventMouseScroll      EventMouseScroll{}     = True
isEventMouseScroll _                           = False

isEventKey              EventKey{}             = True
isEventKey _                                   = False

isEventChar             EventChar{}            = True
isEventChar _                                  = False
