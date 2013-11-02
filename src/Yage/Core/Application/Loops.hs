{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Yage.Core.Application.Loops where

import           Yage.Prelude hiding (catch)

import           Control.Monad (liftM)
import           Control.Monad.State             (gets)


import           Yage.Core.Application
import           Yage.Core.Application.Event
import           Yage.Core.Application.Exception


basicWindowLoop :: (Throws ApplicationException l, Throws InternalException l)
                => (Window -> b -> InputState -> Application l b)              -- | the application
                -> (Int, Int)                                                  -- | window width and height
                -> [WindowHint]                                                -- | window hints
                -> b                                                           -- | initial value for app calc
                -> Application l b
basicWindowLoop app (width, height) hints initial = do
    win <- createWindowWithHints hints width height =<< gets appTitle
    appLoopStep win app initial initialInputState


appLoopStep :: (Throws ApplicationException l, Throws InternalException l)
        => Window
        -> (Window -> b -> InputState -> Application l b)
        -> b
        -> InputState
        -> Application l b
appLoopStep win' app initial' inputState' = withWindowAsCurrent win' $ \win -> do
    inputState <- liftM (updateInputState inputState') collectEvents
    r <- app win initial' inputState

    swapBuffers win
    quit <- windowShouldClose win
    if quit then return r else (appLoopStep win app r inputState)
