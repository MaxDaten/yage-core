{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Exception

import Control.Monad (when)

import Yage.Core.Application
import Yage.Core.Application.Loops
import Yage.Core.Application.Event
import Yage.Core.Application.Logging


main :: IO ()
main =
    let conf    = defaultAppConfig{ logPriority = DEBUG }
        size    = (800, 600)
        hints   = []
    in execApplication "simple test app" conf $ basicWindowLoop app size hints ()
    where
        app :: Window -> () -> InputState -> Application AnyException ()
        app _win _ inputState = do
            when (isPressed inputState Key'W) $ logM DEBUG "pressed"
