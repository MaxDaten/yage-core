{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveFunctor      #-}
module Yage.Core.Application.Internal.Types where

import           Yage.Prelude

import Linear (V2)
import qualified Graphics.UI.GLFW as GLFW


data WindowState = WindowState
    { _winPosition    :: V2 Int
    , _winSize        :: V2 Int
    , _winFocus       :: Bool
    , _winIconified   :: Bool
    , _winShouldClose :: Bool
    , _winRefresh     :: Bool
    , _winMouseIn     :: Bool
    }

makeLenses ''WindowState

---------------------------------------------------------------------------------------------------

data MouseState = MouseState
    { _mousePosition :: V2 Double -- | screen coords relative to upper left corner
    , _mbPressed     :: Set Int
    , _mouseScroll   :: V2 Double
    }
    deriving (Show, Typeable)

makeLenses ''MouseState

---------------------------------------------------------------------------------------------------

data KeyboardState = KeyboardState
    { _keyMap :: Set GLFW.Key
    } deriving (Show, Typeable)

makeLenses ''KeyboardState
---------------------------------------------------------------------------------------------------

type Axis = Double
data JoystickState = JoystickState
    { _joyButtons :: Set Int -- TODO
    , _joyAxes    :: [Axis]
    }
    deriving (Show, Typeable)

makeLenses ''JoystickState

---------------------------------------------------------------------------------------------------

data InputState = InputState
    { _keyboard :: KeyboardState        -- | current pressed keys
    , _mouse    :: MouseState           -- | current pressed buttons and mouse position
    , _joystick :: Maybe JoystickState  -- | current pressed buttons and axes
    }
    deriving (Show, Typeable)

makeLenses ''InputState

initialInputState :: InputState
initialInputState = InputState
    { _keyboard  = mempty
    , _mouse     = mempty
    , _joystick  = Nothing --- JoystickState empty []
    }

---------------------------------------------------------------------------------------------------

instance Monoid InputState where
    mempty = initialInputState
    mappend a b =
        InputState
            { _keyboard = a^.keyboard <> b^.keyboard
            , _mouse    = a^.mouse    <> b^.mouse
            , _joystick = a^.joystick <> b^.joystick
            }


instance Monoid MouseState where
    mempty = MouseState 0 mempty 0
    mappend a b =
        MouseState
            (a^.mousePosition +  b^.mousePosition)
            (a^.mbPressed     <> b^.mbPressed)
            (a^.mouseScroll   +  b^.mouseScroll)

instance Monoid JoystickState where
    mempty = JoystickState mempty mempty
    mappend a b =
        JoystickState
            { _joyButtons = a^.joyButtons <> b^.joyButtons
            , _joyAxes    = a^.joyAxes    <> b^.joyAxes
            }


instance Monoid KeyboardState where
    mempty = KeyboardState mempty
    mappend (KeyboardState a) (KeyboardState b) = KeyboardState $ mappend a b
