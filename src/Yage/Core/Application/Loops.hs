{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Yage.Core.Application.Loops where

import           Yage.Prelude hiding (catch)

import           Data.List                       (filter)
import           Control.Monad.State             (gets)


import           Yage.Core.Application
import           Yage.Core.Application.Exception

basicWindowLoop :: (Throws ApplicationException l, Throws InternalException l)
                => WindowConfig                                                    -- | window size and hints
                -> b                                                               -- | initial value for app calc
                -> (Window -> (InputState, WindowEvents) -> b -> Application l b)  -- | the application
                -> Application l b
basicWindowLoop (WindowConfig (width, height) hints) initial loop = do
    win <- createWindowWithHints hints width height =<< gets appTitle
    appLoopStep win initial (initialInputState, []) loop


appLoopStep :: (Throws ApplicationException l, Throws InternalException l)
        => Window
        -> b
        -> (InputState, WindowEvents)
        -> (Window -> (InputState, WindowEvents) -> b -> Application l b)
        -> Application l b
appLoopStep win' b' (inputState', _consumedWinEvents') app = do
    (x, i) <- withWindowAsCurrent win' $ \win -> do
                allEvents   <- collectEvents
                let inputState = updateInputState inputState' allEvents
                    winEvents  = filter isWindowEvent allEvents

                result      <- app win (inputState, winEvents) b'

                swapBuffers win
                return (result, (inputState, winEvents))
    quit <- windowShouldClose win'
    if quit then return x else (appLoopStep win' x i app)
