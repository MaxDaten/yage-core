{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Yage.Core.Application.Loops where

import           Yage.Prelude hiding (catch)

import           Control.Monad.State             (gets, modify)
import           System.Mem

import           Yage.Core.Application

basicWindowLoop :: (Throws ApplicationException l, Throws InternalException l, EventCtr ectr)
                => WindowConfig                                                    -- | window size and hints
                -> ectr                                                            -- | event controller for the window
                -> (Window -> ectr -> Application l ectr)                          -- | the application
                -> Application l ectr
basicWindowLoop (WindowConfig (width, height) hints) ectr loop = do
    title <- gets appTitle
    win   <- createWindowWithHints hints width height title
    registerWindowCallbacks win ectr
    appLoopStep win ectr loop


appLoopStep :: (Throws ApplicationException l, Throws InternalException l)
        => Window
        -> b
        -> (Window -> b -> Application l b)
        -> Application l b
appLoopStep win' b' app = do
    x <- withWindowAsCurrent win' $ \win -> do
            pollEvents
-- TODO EVENTS
-- update input controller (get new ictr and re-register window callbacks? (think about the atomization))
-- retain state from input-ctr (b param?) or put the input ctr into the app ? !!! thats it

            result      <- app win b'

            swapBuffers win
            return result
    quit <- windowShouldClose win'
    gcTime <- ioe $ ioTime $ performGC
    modify (\st -> st{ appGCTime = snd gcTime } )
    if quit then return x else (appLoopStep win' x app)
