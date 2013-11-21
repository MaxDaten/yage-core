{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Yage.Core.Application.Event
    ( module Yage.Core.Application.Event
    , module EventType
    , module InternalEvent
    ) where

import           Yage.Prelude

import           Data.Maybe                     ()
import           Data.List                      (foldr, any)

import           Yage.Core.Application.Types
import           Yage.Core.Application.Internal.Event as InternalEvent
import           Yage.Core.Application.Event.Types    as EventType



updateInputState :: InputState -> [Event] -> InputState
updateInputState = foldr insertIntoInputState

isWindowEvent :: Event -> Bool
isWindowEvent event = any (flip ($) event)
                         [ isEventWindowPosition
                         , isEventWindowSize
                         , isEventWindowClose
                         , isEventWindowFocus
                         , isEventWindowIconify
                         ]

-- | TODO respect windows
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


justResizedTo :: WindowEvents -> Maybe (Int, Int)
justResizedTo es = extractSize <$> firstOf (traverse.filtered isEventWindowSize) es
    where
        extractSize (EventWindowSize _win e) = (e^.winWidth, e^.winHeight)
        extractSize _ = error "the impossible happend"
