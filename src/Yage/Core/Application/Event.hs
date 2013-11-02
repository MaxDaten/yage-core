{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}

module Yage.Core.Application.Event where

import           Yage.Prelude

import           Control.Monad                  (liftM, sequence, join)
import           Control.Monad.Reader           (asks)
import           Control.Concurrent.STM         (TQueue, atomically, tryReadTQueue)

import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set
import           Data.List                      (map, foldr)
import           Data.Maybe                     ()

import           Control.Lens

import           Yage.Core.Application.Types
import           Yage.Core.Application.Utils
import           Yage.Core.Application.Internal.Event
import           Yage.Core.Application.Exception
import           Yage.Core.Application.Logging
import           Yage.Core.GLFW.Callback        hiding (Key)
import           Yage.Core.GLFW.Event


--------------------------------------------------------------------------------

class EventHandler t a where
    handleEvent :: t -> EventHandling a

type EventHandling a = forall l. (Throws InternalException l, Throws ApplicationException l) => Event -> Application l a
--------------------------------------------------------------------------------


pollOneEvent :: (Throws InternalException l) => Application l (Maybe Event)
pollOneEvent = do
    pollEvents
    queue <- asks appEventQ
    ioe $ atomically $ tryReadTQueue queue



handleEventsWith :: (Throws InternalException l, Throws ApplicationException l)
                  => EventHandling a -> Application l ([a])
handleEventsWith handler = pollOneEvent >>= processEvent' []
    where
        processEvent' as Nothing = return as
        processEvent' as (Just e) = do
            internalEventHandling e
            a <- handler e
            processEvent' (a:as) =<< pollOneEvent



multiplexEvents :: (Throws InternalException l, Throws ApplicationException l, EventHandler h b)
                => [h] -> Application l [b]
multiplexEvents hs = liftM join $ handleEventsWith $ flip multiplex hs
    where
        multiplex :: (Throws InternalException l, Throws ApplicationException l, EventHandler h b) => Event -> [h] -> Application l [b]
        multiplex e = sequence . map (`handleEvent` e)



collectEvents :: (Throws InternalException l, Throws ApplicationException l)
              => Application l [Event]
collectEvents = handleEventsWith return



internalEventHandling :: EventHandling ()
internalEventHandling e = debugM $ show e



registerGlobalErrorCallback :: (Throws InternalException l) => Maybe Logger -> Application l ()
registerGlobalErrorCallback ml = do
    eventQ <- asks appEventQ
    setErrorCallback $ Just $ errorCallback eventQ ml



registerWindowCallbacks :: (Throws InternalException l) => Window -> TQueue Event -> Maybe Logger -> Application l ()
registerWindowCallbacks win tq ml = do
    -- annoying setup
    setWindowPositionCallback   win $ Just $ windowPositionCallback tq ml
    setWindowSizeCallback       win $ Just $ windowSizeCallback tq ml
    setWindowCloseCallback      win $ Just $ windowCloseCallback tq ml
    setWindowRefreshCallback    win $ Just $ windowRefreshCallback tq ml
    setWindowFocusCallback      win $ Just $ windowFocusCallback tq ml
    setWindowIconifyCallback    win $ Just $ windowIconifyCallback tq ml
    setFramebufferSizeCallback  win $ Just $ framebufferSizeCallback tq ml

    setKeyCallback              win $ Just $ keyCallback tq ml
    setCharCallback             win $ Just $ charCallback tq ml

    setMouseButtonCallback      win $ Just $ mouseButtonCallback tq ml
    setMousePositionCallback    win $ Just $ mousePositionCallback tq ml
    setMouseEnterCallback       win $ Just $ mouseEnterCallback tq ml
    setScrollCallback           win $ Just $ scrollCallback tq ml

--------------------------------------------------------------------------------



data MouseState = MouseState
    { _mousePosition :: (Double, Double) -- | screen coords relative to upper left corner
    , _mouseButtons  :: Set Event
    } deriving Show

makeLenses ''MouseState

type KeyboardState = Map Key Event

type Axis = Double
data JoystickState = JoystickState
    { _joyButtons   :: Set Event
    , _joyAxes      :: [Axis]
    } deriving Show

makeLenses ''JoystickState

data InputState = InputState
    { _keyboard :: KeyboardState        -- | current pressed keys
    , _mouse    :: MouseState           -- | current pressed buttons and mouse position
    , _joystick :: Maybe JoystickState  -- | current pressed buttons and axes
    } deriving Show

makeLenses ''InputState

initialInputState :: InputState
initialInputState = InputState
    { _keyboard  = Map.empty
    , _mouse     = MouseState (0,0) Set.empty
    , _joystick  = Nothing --- JoystickState empty []
    }


updateInputState :: InputState -> [Event] -> InputState
updateInputState = foldr insertIntoInputState


-- | TODO repsect windows
insertIntoInputState :: Event -> InputState -> InputState
insertIntoInputState   (EventMousePosition _win d) = mouse.mousePosition .~ (d^.mousePosX, d^.mousePosY)
insertIntoInputState e@(EventMouseButton   _win d) = mouse.mouseButtons  %~ (contains e .~ (d^.mouseButtonState == MouseButtonState'Pressed)) -- | so fucking awesome !
insertIntoInputState e@(EventKey           _win d) = keyboard            %~ at (d^.key) .~ justPressed e
insertIntoInputState _ = id

justPressed :: Event -> Maybe Event
justPressed e@(EventKey _win d)         = if d^.keyState == KeyState'Released        then Nothing else Just e
justPressed e@(EventMouseButton _win d) = if d^.mouseButtonState == MouseButtonState'Pressed then Just e else Nothing
justPressed _ = Nothing

currentKeyState :: InputState -> Key -> Maybe Event
currentKeyState inputState k = inputState^.keyboard.at k

isPressed :: InputState -> Key -> Bool
isPressed inputState k = isJust $ currentKeyState inputState k
