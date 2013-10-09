{-# LANGUAGE FlexibleContexts           #-}

module Yage.Core.GLFW.Window where

import           Yage.Prelude
import           Control.Monad                   (mapM_)
import           Control.Monad.Exception
import           Yage.Core.Application.Exception

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

setGlobalWindowHints :: (Throws InternalException l) => [GLFW.WindowHint] -> Application l ()
setGlobalWindowHints = mapM_ (glfw . GLFW.windowHint)

revertGlobalWindowHints :: (Throws InternalException l) => Application l ()
revertGlobalWindowHints = glfw GLFW.defaultWindowHints

{-# INLINE getWindowClientAPI #-}
getWindowClientAPI :: (Throws InternalException l) => Window -> Application l GLFW.ClientAPI
getWindowClientAPI = glfw . GLFW.getWindowClientAPI . win'handle

getWindowContextGLVersion :: (Throws InternalException l) => Window -> Application l GLFW.Version
getWindowContextGLVersion win = withWindowHandle win $ \wh ->
    GLFW.Version
        <$> (glfw $ GLFW.getWindowContextVersionMajor wh)
        <*> (glfw $ GLFW.getWindowContextVersionMinor wh)
        <*> (glfw $ GLFW.getWindowContextVersionRevision wh)

getWindowGLProfile :: (Throws InternalException l) => Window -> Application l GLFW.OpenGLProfile
getWindowGLProfile = glfw . GLFW.getWindowOpenGLProfile . win'handle

createWindowHandle :: (Throws InternalException l) => Int -> Int -> String -> Maybe GLFW.Monitor -> Maybe GLFW.Window -> Application l GLFW.Window
createWindowHandle width height title mMon mWin = do
    mwin <- glfw $ GLFW.createWindow width height title mMon mWin
    case mwin of
        Just win -> return win
        Nothing -> throw $ WindowCreationException "no window created"

withWindowHandle :: (Throws InternalException l) => Window -> (GLFW.Window -> Application l a) -> Application l a
withWindowHandle win f = f $ win'handle win
