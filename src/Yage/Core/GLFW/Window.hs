{-# LANGUAGE FlexibleContexts           #-}

module Yage.Core.GLFW.Window where

import           Control.Monad.Exception
import           Yage.Core.Application.Exception
import           Control.Applicative ((<$>))

-- 3rd party apis
import qualified Graphics.UI.GLFW as GLFW

import           Yage.Core.Application.Types
import           Yage.Core.GLFW.Base


--------------------------------------------------------------------------------
-- GLFW Action Mapping
--------------------------------------------------------------------------------
{-# INLINE iconifyWindow #-}
iconifyWindow :: (Throws InternalException l) => Window -> Application l ()
iconifyWindow = glfw . GLFW.iconifyWindow . winHandle

{-# INLINE makeContextCurrent #-}
makeContextCurrent :: (Throws InternalException l) => Maybe Window -> Application l ()
makeContextCurrent mwin = glfw $ GLFW.makeContextCurrent (winHandle <$> mwin)

{-# INLINE directlyDestroyWindow #-}
directlyDestroyWindow :: (Throws InternalException l) => Window -> Application l ()
directlyDestroyWindow = glfw . GLFW.destroyWindow . winHandle

{-# INLINE swapBuffers #-}
swapBuffers :: (Throws InternalException l) => Window -> Application l ()
swapBuffers = glfw . GLFW.swapBuffers . winHandle

{-# INLINE windowShouldClose #-}
windowShouldClose :: (Throws InternalException l) => Window -> Application l Bool
windowShouldClose = glfw . GLFW.windowShouldClose . winHandle

{-# INLINE setWindowShouldClose #-}
setWindowShouldClose :: (Throws InternalException l) => Window -> Bool -> Application l ()
setWindowShouldClose win b = glfw . (\w -> GLFW.setWindowShouldClose w b) . winHandle $ win

{-# INLINE mkWindow #-}
mkWindow :: Int -> Int -> String -> IO (Maybe Window)
mkWindow width height title = do
    mwin <- GLFW.createWindow width height title Nothing Nothing
    return $ Window title (width, height) <$> mwin

withWindowHandle :: (Throws InternalException l) => Window -> (GLFW.Window -> Application l a) -> Application l a
withWindowHandle win f = f $ winHandle win

