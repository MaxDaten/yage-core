{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Exception
import Yage.Core.Application
import Yage.Core.Application.Logging

import Control.Monad (unless, when)
import Data.Maybe (isJust)


main :: IO ()
main = do
    let conf = defaultAppConfig{ logPriority = DEBUG }
    r <- execApplication "simple test app" conf app
    print r
    where
        app :: Application AnyException Int
        app = do
            win <- createWindow 800 600 "test window"
            io $ print win

            loop win
            --forever (return ())
            return 42
        loop win = do
            processEvents
            swapBuffers win

            quit <- windowShouldClose win
            unless quit (loop win)

        processEvents = do
            me <- pollOneEvent
            processEvent me
            when (isJust me) processEvents

        processEvent Nothing = return ()
        processEvent (Just e) = do
            debugM $ show e
