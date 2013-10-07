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
iconifyWindow = glfw . GLFW.iconifyWindow . win'handle

{-# INLINE makeContextCurrent #-}
makeContextCurrent :: (Throws InternalException l) => Maybe Window -> Application l ()
makeContextCurrent mwin = glfw $ GLFW.makeContextCurrent (win'handle <$> mwin)

{-# INLINE directlyDestroyWindow #-}
directlyDestroyWindow :: (Throws InternalException l) => Window -> Application l ()
directlyDestroyWindow = glfw . GLFW.destroyWindow . win'handle

{-# INLINE swapBuffers #-}
swapBuffers :: (Throws InternalException l) => Window -> Application l ()
swapBuffers = glfw . GLFW.swapBuffers . win'handle

{-# INLINE windowShouldClose #-}
windowShouldClose :: (Throws InternalException l) => Window -> Application l Bool
windowShouldClose = glfw . GLFW.windowShouldClose . win'handle

{-# INLINE setWindowShouldClose #-}
setWindowShouldClose :: (Throws InternalException l) => Window -> Bool -> Application l ()
setWindowShouldClose win b = glfw . (\w -> GLFW.setWindowShouldClose w b) . win'handle $ win

createWindowHandle :: (Throws InternalException l) => Int -> Int -> String -> Maybe GLFW.Monitor -> Maybe GLFW.Window -> Application l GLFW.Window
createWindowHandle width height title mMon mWin = do
    mwin <- glfw $ GLFW.createWindow width height title mMon mWin
    case mwin of
        Just win -> return win
        Nothing -> throw . InternalException . toException $ WindowCreationException

withWindowHandle :: (Throws InternalException l) => Window -> (GLFW.Window -> Application l a) -> Application l a
withWindowHandle win f = f $ win'handle win
