{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Yage.Core.GLFW.Window where

import           Control.Monad.Exception
import           Yage.Core.Application.Exception
import           Control.Applicative ((<$>))

-- 3rd party apis
import qualified Graphics.UI.GLFW as GLFW

import           Yage.Core.Application.Types
import           Yage.Core.GLFW.Base

{-# INLINE liftGlfw #-}
liftGlfw :: (Throws InternalException l) => (GLFW.Window -> IO a) -> Window -> Application l a
liftGlfw glfwAction = glfw . glfwAction . winHandle


--------------------------------------------------------------------------------
-- GLFW Action Mapping
--------------------------------------------------------------------------------
{-# INLINE iconifyWindow #-}
iconifyWindow :: (Throws InternalException l) => Window -> Application l ()
iconifyWindow = liftGlfw GLFW.iconifyWindow



{-# INLINE makeContextCurrent #-}
makeContextCurrent :: (Throws InternalException l) => Maybe Window -> Application l ()
makeContextCurrent mwin = glfw $ GLFW.makeContextCurrent (winHandle <$> mwin)



{-# INLINE directlyDestroyWindow #-}
directlyDestroyWindow :: (Throws InternalException l) => Window -> Application l ()
directlyDestroyWindow = liftGlfw GLFW.destroyWindow



{-# INLINE swapBuffers #-}
swapBuffers :: (Throws InternalException l) => Window -> Application l ()
swapBuffers = liftGlfw GLFW.swapBuffers



{-# INLINE mkWindow #-}
mkWindow :: Int -> Int -> String -> IO (Maybe Window)
mkWindow width height title = do
    mwin <- GLFW.createWindow width height title Nothing Nothing
    return $ Window title width height <$> mwin

