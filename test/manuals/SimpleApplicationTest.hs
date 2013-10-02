{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Yage.Core.Application
import Yage.Core.Application.Exception
import Control.Monad.Exception


main :: IO ()
main = do
    r <- execApplication "simple test app" app
    print r
    where
        app :: (Throws ApplicationException l, Throws InternalException l) => Application l Int
        app = do
            win <- createWindow 800 600 "test window"
            io $ print win
            --forever (return ())
            return 42
