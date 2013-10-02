{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Yage.Core.Application
import Yage.Core.Application.Exception
import Control.Monad.Exception

import Control.Monad (unless)


main :: IO ()
main = do
    r <- execApplication "simple test app" app
    print r
    where
        app :: (Throws ApplicationException l, Throws InternalException l) => Application l Int
        app = do
            win <- createWindow 800 600 "test window"
            io $ print win

            loop win
            --forever (return ())
            return 42
        loop win = do
            quit <- windowShouldClose win
            pollEvents
            swapBuffers win
            unless quit (loop win)
