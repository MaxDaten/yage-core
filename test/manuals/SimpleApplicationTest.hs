{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Exception
import Yage.Core.Application

import Control.Monad (unless, when)
import Data.Maybe (isJust)


main :: IO ()
main = do
    r <- execApplication "simple test app" app
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
            me <- pollEvent
            processEvent me
            when (isJust me) processEvents

        processEvent Nothing = return ()
        processEvent (Just e) = io $ print e
