{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
module Yage.Core.Application.Event.Types where

import           Yage.Prelude

import qualified Data.Map                                   as Map
import qualified Data.Set                                   as Set

import           Yage.Core.Application.Internal.Event.Types


-- type WindowEvents = [Event]
type WindowEvents = [Event]


data MouseState = MouseState
    { _mousePosition :: (Double, Double) -- | screen coords relative to upper left corner
    , _mouseButtons  :: Set Event
    }
    deriving (Show, Typeable)

makeLenses ''MouseState

type KeyboardState = Map Key Event

type Axis = Double
data JoystickState = JoystickState
    { _joyButtons :: Set Event
    , _joyAxes    :: [Axis]
    }
    deriving (Show, Typeable)

makeLenses ''JoystickState

data InputState = InputState
    { _keyboard :: KeyboardState        -- | current pressed keys
    , _mouse    :: MouseState           -- | current pressed buttons and mouse position
    , _joystick :: Maybe JoystickState  -- | current pressed buttons and axes
    }
    deriving (Show, Typeable)

makeLenses ''InputState

initialInputState :: InputState
initialInputState = InputState
    { _keyboard  = Map.empty
    , _mouse     = MouseState (0,0) Set.empty
    , _joystick  = Nothing --- JoystickState empty []
    }

instance Monoid InputState where
    mempty = initialInputState
    mappend a b =
        InputState
            { _keyboard = a^.keyboard <> b^.keyboard
            , _mouse    = a^.mouse    <> b^.mouse
            , _joystick = a^.joystick <> b^.joystick
            }


instance Monoid MouseState where
    mempty = MouseState (0,0) mempty
    mappend a b =
        MouseState
            { _mousePosition = a^.mousePosition <> b^.mousePosition
            , _mouseButtons = a^.mouseButtons <> b^.mouseButtons
            }

instance Monoid JoystickState where
    mempty = JoystickState mempty mempty
    mappend a b =
        JoystickState
            { _joyButtons = a^.joyButtons <> b^.joyButtons
            , _joyAxes    = a^.joyAxes    <> b^.joyAxes
            }

instance Monoid Double where
    mempty = 0
    mappend = (+)
