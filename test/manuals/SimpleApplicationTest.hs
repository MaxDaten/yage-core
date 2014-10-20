{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad (void)

import Yage.Core.Application
import Yage.Core.Application.Loops
import Yage.Core.Application.Logging

data KeyLogger = KeyLogger

instance EventCtr KeyLogger where
    keyCallback _ =  return $ \_winH key _code _state _modifier -> print (show key)

main :: IO ()
main =
    let conf    = defaultAppConfig{ logPriority = DEBUG }
        winConf = WindowConfig (800, 600) []
    in void $
        execApplication "simple test app" conf $
            basicWindowLoop winConf KeyLogger $
                \_win ctr -> return ctr
