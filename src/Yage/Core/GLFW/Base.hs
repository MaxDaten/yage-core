{-# LANGUAGE FlexibleContexts           #-}

module Yage.Core.GLFW.Base where

import           Yage.Prelude
import           Control.Monad.Exception
import           Yage.Core.Application.Exception

import qualified Graphics.UI.GLFW as GLFW

import           Yage.Core.Application.Types

--------------------------------------------------------------------------------

{-# INLINE glfw #-}
glfw :: (Throws InternalException l) => IO a -> Application l a
glfw = Application . wrapException IOException . io


{-# INLINE initGlfw #-}
initGlfw :: (Throws InternalException l) => Application l ()
initGlfw = do
    inited <- glfw $ GLFW.init
    unless inited (Application $ throw $ IOException . toException $ InitException)



{-# INLINE terminateGlfw #-}
terminateGlfw :: (Throws InternalException l) => Application l ()
terminateGlfw = glfw $ GLFW.terminate

getGLFWVersion :: (Throws InternalException l) => Application l GLFW.Version
getGLFWVersion = glfw $ GLFW.getVersion
