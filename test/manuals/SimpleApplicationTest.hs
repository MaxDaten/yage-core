{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad (when)

import Yage.Core.Application
import Yage.Core.Application.Loops
import Yage.Core.Application.Logging


main :: IO ()
main =
    let conf    = defaultAppConfig{ logPriority = DEBUG }
        winConf    = WindowConfig (800, 600) []
    in execApplication "simple test app" conf $ basicWindowLoop winConf () $
        \_win (inputState, _) _ -> do
            when (isPressed inputState Key'W) $ logM DEBUG "pressed"
