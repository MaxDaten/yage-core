{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DeriveDataTypeable     #-}
module Yage.Core.Application.Event.Types where

import Yage.Prelude

import Control.Lens

import qualified Data.Map as Map
import qualified Data.Set as Set

import Yage.Core.Application.Internal.Event.Types

data MouseState = MouseState
    { _mousePosition :: (Double, Double) -- | screen coords relative to upper left corner
    , _mouseButtons  :: Set Event
    }
    deriving (Show, Typeable)

makeLenses ''MouseState

type KeyboardState = Map Key Event

type Axis = Double
data JoystickState = JoystickState
    { _joyButtons   :: Set Event
    , _joyAxes      :: [Axis]
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
