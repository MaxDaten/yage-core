{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Yage.Core.Application.Loops where

import           Yage.Prelude hiding (catch)

import           Control.Monad.State             (gets)


import           Yage.Core.Application
import           Yage.Core.Application.Exception

basicWindowLoop :: (Throws ApplicationException l, Throws InternalException l)
                => WindowConfig                                                    -- | window size and hints
                -> b                                                               -- | initial value for app calc
                -> (Window -> b -> Application l b)  -- | the application
                -> Application l b
basicWindowLoop (WindowConfig (width, height) hints) initial loop = do
    win <- createWindowWithHints hints width height =<< gets appTitle
    appLoopStep win initial loop


appLoopStep :: (Throws ApplicationException l, Throws InternalException l)
        => Window
        -> b
        -> (Window -> b -> Application l b)
        -> Application l b
appLoopStep win' b' app = do
    x <- withWindowAsCurrent win' $ \win -> do
            pollEvents

            result      <- app win b'

            swapBuffers win
            return result
    quit <- windowShouldClose win'
    if quit then return x else (appLoopStep win' x app)
