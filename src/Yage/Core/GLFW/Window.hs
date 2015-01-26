{-# LANGUAGE FlexibleContexts           #-}

module Yage.Core.GLFW.Window
    ( module Yage.Core.GLFW.Window
    , module GLFWExports
    ) where

import           Yage.Prelude
import           Control.Monad.Exception
import           Yage.Core.Application.Exception

import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW as GLFWExports (getWindowPos, getWindowSize, getWindowFocused, getWindowIconified, getFramebufferSize)

import           Yage.Core.Application.Types
import           Yage.Core.GLFW.Base
import           Data.Version


--------------------------------------------------------------------------------
-- GLFW Action Mapping
--------------------------------------------------------------------------------
{-# INLINE iconifyWindow #-}
iconifyWindow :: (Throws InternalException l) => Window -> Application l ()
iconifyWindow = glfw . GLFW.iconifyWindow . winHandle

{-# INLINE makeContextCurrent #-}
makeContextCurrent :: (Throws InternalException l) => Maybe (Window) -> Application l ()
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
setWindowShouldClose win b = glfw . (`GLFW.setWindowShouldClose` b) . winHandle $ win

setGlobalWindowHints :: (Throws InternalException l) => [GLFW.WindowHint] -> Application l ()
setGlobalWindowHints = mapM_ (glfw . GLFW.windowHint)

{-# INLINE setWindowTitle #-}
setWindowTitle :: (Throws InternalException l) => Window -> String -> Application l ()
setWindowTitle win title = glfw . (`GLFW.setWindowTitle` title) . winHandle $ win

revertGlobalWindowHints :: (Throws InternalException l) => Application l ()
revertGlobalWindowHints = glfw GLFW.defaultWindowHints

{-# INLINE getWindowClientAPI #-}
getWindowClientAPI :: (Throws InternalException l) => Window -> Application l GLFW.ClientAPI
getWindowClientAPI = glfw . GLFW.getWindowClientAPI . winHandle

getWindowContextGLVersion :: (Throws InternalException l) => Window -> Application l Version
getWindowContextGLVersion win = withWindowHandle win $ \wh ->
    (`Version` []) <$> sequence [glfw (GLFW.getWindowContextVersionMajor wh), glfw (GLFW.getWindowContextVersionMinor wh), glfw (GLFW.getWindowContextVersionRevision wh)]

getWindowGLProfile :: (Throws InternalException l) => Window -> Application l GLFW.OpenGLProfile
getWindowGLProfile = glfw . GLFW.getWindowOpenGLProfile . winHandle

createWindowHandle :: (Throws InternalException l) => Int -> Int -> String -> Maybe GLFW.Monitor -> Maybe GLFW.Window -> Application l GLFW.Window
createWindowHandle width height title mMon mWin = do
    mwin <- glfw $ GLFW.createWindow width height title mMon mWin
    case mwin of
        Just win -> return win
        Nothing -> throw $ WindowCreationException "no window created"

withWindowHandle :: (Throws InternalException l) => Window -> (GLFW.Window -> Application l a) -> Application l a
withWindowHandle win f = f $ winHandle win
