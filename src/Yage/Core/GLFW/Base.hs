{-# LANGUAGE FlexibleContexts           #-}

module Yage.Core.GLFW.Base where

import           Control.Monad.Exception
import           Yage.Core.Application.Exception
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad (unless)

import qualified Graphics.UI.GLFW as GLFW

import           Yage.Core.Application.Types

--------------------------------------------------------------------------------

{-# INLINE glfw #-}
glfw :: (Throws InternalException l)=> IO a -> Application l a
glfw m = wrapException GLFWException $ liftIO m



{-# INLINE initGlfw #-}
initGlfw :: (Throws InternalException l) => Application l ()
initGlfw = do
    inited <- glfw $ GLFW.init
    unless inited (throw $ GLFWException . toException $ InitException)



{-# INLINE terminateGlfw #-}
terminateGlfw :: (Throws InternalException l) => Application l ()
terminateGlfw = glfw $ GLFW.terminate

