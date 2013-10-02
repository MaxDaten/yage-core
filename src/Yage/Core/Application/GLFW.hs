{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Yage.Core.Application.GLFW where

import           Control.Monad.Exception
import           Yage.Core.Application.Exception
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad (unless)
import           Control.Applicative ((<$>))

-- 3rd party apis
import qualified Graphics.UI.GLFW             as GLFW (Window, createWindow,
                                                       destroyWindow,
                                                       iconifyWindow, init,
                                                       makeContextCurrent,
                                                       terminate)

import           Yage.Core.Application.Types


glfw :: (Throws InternalException l)=> IO a -> Application l a
glfw m = wrapException GLFWException $ liftIO m



liftGlfw :: (Throws InternalException l) => (GLFW.Window -> IO a) -> Window -> Application l a
liftGlfw glfwAction = glfw . glfwAction . winHandle


--------------------------------------------------------------------------------
-- GLFW Action Mapping
--------------------------------------------------------------------------------

iconifyWindow :: (Throws InternalException l) => Window -> Application l ()
iconifyWindow = liftGlfw GLFW.iconifyWindow


-- TODO: SomeException ? NoExceptions
makeContextCurrent :: (Throws InternalException l) => Maybe Window -> Application l ()
makeContextCurrent mwin = glfw $ GLFW.makeContextCurrent (winHandle <$> mwin)


{-# INLINE directlyDestroyWindow #-}
directlyDestroyWindow :: (Throws InternalException l) => Window -> Application l ()
directlyDestroyWindow Window{winHandle} = glfw $ GLFW.destroyWindow winHandle


initGlfw :: (Throws InternalException l) => Application l ()
initGlfw = do
    inited <- glfw $ GLFW.init
    unless inited (throw $ GLFWException . toException $ InitException)

terminateGlfw :: (Throws InternalException l) => Application l ()
terminateGlfw = glfw $ GLFW.terminate


mkWindow :: Int -> Int -> String -> IO (Maybe Window)
mkWindow width height title = do
    mwin <- GLFW.createWindow width height title Nothing Nothing
    return $ Window title width height <$> mwin

