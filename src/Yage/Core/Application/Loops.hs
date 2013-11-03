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
                => (Int, Int)                                                  -- | window width and height
                -> [WindowHint]                                                -- | window hints
                -> b                                                           -- | initial value for app calc
                -> (Window -> b -> InputState -> Application l b)              -- | the application
                -> Application l b
basicWindowLoop (width, height) hints initial loop = do
    win <- createWindowWithHints hints width height =<< gets appTitle
    appLoopStep win initial initialInputState loop


appLoopStep :: (Throws ApplicationException l, Throws InternalException l)
        => Window
        -> b
        -> InputState
        -> (Window -> b -> InputState -> Application l b)
        -> Application l b
appLoopStep win' initial' inputState' app = do
    (x, i) <- withWindowAsCurrent win' $ \win -> do
                result      <- app win initial' inputState'
                inputState  <- liftM (updateInputState inputState') collectEvents
                swapBuffers win
                return (result, inputState)
    quit <- windowShouldClose win'
    if quit then return x else (appLoopStep win' x i app)
