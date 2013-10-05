{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Exception
import Control.Applicative ((<$>))
import Control.Monad (void)
import Yage.Core.Application
import Yage.Core.Application.Exception

import Control.Monad (unless, when)
import Data.Maybe (isJust)


main :: IO ()
main = do
    r <- execApplication "simple test app" app
    print r
    where
        -- app :: (Throws ApplicationException l, Throws InternalException l) => Application l Int
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

        --processEvent :: (Throws ApplicationException l) => Maybe Event -> Application l ()
        processEvents = do
            me <- pollEvent
            processEvent me
            when (isJust me) processEvents

        processEvent Nothing = return ()
        processEvent (Just e) = io $ print e
