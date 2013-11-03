{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

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
    in execApplication "simple test app" conf $ basicWindowLoop size hints () $
        \_win _ inputState -> do
            when (isPressed inputState Key'W) $ logM DEBUG "pressed"
