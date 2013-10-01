{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Yage.Core.Application
import Control.Monad (forever)
import Control.Monad.Exception
import Control.Monad.Trans (lift)
import           Control.Monad.State


main :: IO ()
main = do
    r <- execApplication "simple test app" app
    print r
    where
        app :: (Throws ApplicationException l) => Application l Int
        app = do
            io $ print "xyz"
            st <- get
            io $ print "xyz"
            win <- createWindow 800 600 "test window"
            io $ print "xyz2"
            io $ print win
            --forever (return ())
            return 42
