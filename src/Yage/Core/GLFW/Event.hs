{-# LANGUAGE FlexibleContexts           #-}
module Yage.Core.GLFW.Event where

import           Control.Monad.Exception
import           Yage.Core.Application.Exception

-- 3rd party apis
import qualified Graphics.UI.GLFW as GLFW

import           Yage.Core.Application.Types
import           Yage.Core.GLFW.Base

--------------------------------------------------------------------------------


{-# INLINE pollEvents #-}
pollEvents :: (Throws InternalException l) => Application l ()
pollEvents = glfw GLFW.pollEvents



{-# INLINE waitEvents #-}
waitEvents :: (Throws InternalException l) => Application l ()
waitEvents = glfw GLFW.waitEvents
