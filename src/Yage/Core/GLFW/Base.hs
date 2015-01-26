{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RecordWildCards            #-}

module Yage.Core.GLFW.Base where

import           Yage.Prelude
import           Control.Monad.Exception
import           Yage.Core.Application.Exception

import qualified Graphics.UI.GLFW as GLFW

import           Yage.Core.Application.Types
import           Data.Version
import           System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------

{-# INLINE glfw #-}
glfw :: (Throws InternalException l) => IO a -> Application l a
glfw = wrapException IOException . io


{-# INLINE initGlfw #-}
initGlfw :: (Throws InternalException l) => Application l ()
initGlfw = do
    inited <- glfw $ GLFW.init
    unless inited $ throw $ IOException . toException $ InitException



{-# INLINE terminateGlfw #-}
terminateGlfw :: (Throws InternalException l) => Application l ()
terminateGlfw = glfw $ GLFW.terminate

glfwVersion :: Version
glfwVersion = toVersion (unsafePerformIO GLFW.getVersion) where
    toVersion GLFW.Version{..} = Version [versionMajor, versionMinor, versionRevision] []
