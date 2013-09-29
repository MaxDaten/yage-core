module Main where

import Yage.Core.Application


main :: IO ()
main = do
    e <- execApplication "simple test app" app
    case e of
        Right r -> print r
        Left e -> print e

    where
        app :: Application Int
        app = do
            mwin <- createWindow 800 600 "test window"
            io $ print mwin
            return 42
