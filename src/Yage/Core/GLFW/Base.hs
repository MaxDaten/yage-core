{-# LANGUAGE FlexibleContexts           #-}

module Yage.Core.GLFW.Base where

import           Yage.Prelude
import           Control.Monad.Exception
import           Yage.Core.Application.Exception
import           Control.Monad (unless)

import qualified Graphics.UI.GLFW as GLFW

import           Yage.Core.Application.Types

--------------------------------------------------------------------------------

{-# INLINE glfw #-}
glfw :: (Throws InternalException l)=> IO a -> Application l a
glfw m = wrapException IOException $ io m


{-# INLINE initGlfw #-}
initGlfw :: (Throws InternalException l) =>Application l ()
initGlfw = do
    inited <- glfw $ GLFW.init
    unless inited (throw $ IOException . toException $ InitException)



{-# INLINE terminateGlfw #-}
terminateGlfw :: (Throws InternalException l) => Application l ()
terminateGlfw = glfw $ GLFW.terminate

getGLFWVersion :: (Throws InternalException l) => Application l GLFW.Version
getGLFWVersion = glfw $ GLFW.getVersion
