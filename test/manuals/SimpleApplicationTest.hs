{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Yage.Core.Application
import Control.Monad (forever)
import Control.Monad.Exception
import Control.Monad.Trans (lift)

main :: IO ()
main = do
    r <- execApplication "simple test app" app
    print r
    where
        app :: (Throws SomeException l) => Application l Int
        app = do
            mwin <- createWindow 800 600 "test window"
            io $ print mwin
            forever (return ())
            return 42
