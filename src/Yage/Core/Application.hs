{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}


module Yage.Core.Application
    ( Application
    , execApplication
    , Window, createWindow, destroyWindow, windowByTitle, iconifyWindow, makeContextCurrent
    , io
    ) where

-- Types
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as BS (pack)
import           Data.Maybe                   (fromJust)
import           Data.Trie                    (Trie)
import qualified Data.Trie                    as T (delete, empty, insert,
                                                    lookup, toListBy)

-- concepts
import           Control.Applicative
import           Control.Monad.Exception
import           Control.Monad.Exception.Base (NoExceptions)
import           Control.Monad.IO.Class       ()
import           Control.Monad.State

-- 3rd party apis
import qualified Graphics.UI.GLFW             as GLFW (Window, createWindow,
                                                       destroyWindow,
                                                       iconifyWindow, init,
                                                       makeContextCurrent,
                                                       terminate)

import           Yage.Core.Application.Exception

type Application l a = EMT l (StateT ApplicationState IO) a


data Window = Window
    { winTitle  :: !String
    , winWidth  :: !Int
    , winHeight :: !Int
    , winHandle :: !GLFW.Window
    }
    deriving (Show)


data ApplicationState = ApplicationState
    { appTitle   :: !String
    , appWindows :: Trie (Window)
    }
    deriving (Show)


initialState :: ApplicationState
initialState = ApplicationState
    { appTitle = ""
    , appWindows = T.empty
    }


execApplication :: l ~ ApplicationException => String -> Application (Caught l NoExceptions) b -> IO b
execApplication title app = do
    let a = runEMT $ runApp app
                     `catch` exceptionHandler
                
    (eResult, st') <- runStateT a (initialState { appTitle = title })
    return eResult
    where
        --runApp :: (Throws ApplicationException l, Throws InternalException l) => Application l a -> Application l a
        runApp app = do
            startup
            x <- app
            shutdown
            return x

        startup = initGlfw
        shutdown = destroyAllWindows >> terminateGlfw
        exceptionHandler = \(e::ApplicationException) -> error $ show e


createWindow :: (Throws InternalException l) => Int -> Int -> String -> Application l Window
createWindow width height title = do
    win <- glfw $ liftM fromJust $ mkWindow width height title
    addWindow win
    return win


mkWindow :: Int -> Int -> String -> IO (Maybe Window)
mkWindow width height title = do
    mwin <- GLFW.createWindow width height title Nothing Nothing
    return $ Window title width height <$> mwin


windowByTitle :: String -> Application l (Maybe Window)
windowByTitle title = do
    wins <- gets appWindows
    return $ T.lookup (BS.pack title) wins


destroyWindow :: (Throws InternalException l) => Window -> Application l ()
destroyWindow Window{winTitle} = do
    appState <- get
    let (mwin, wins') = retrieve (BS.pack winTitle) $ appWindows appState
    maybe (return ()) directlyDestroyWindow mwin
    put appState{ appWindows = wins' }


destroyAllWindows :: (Throws InternalException l) => Application l ()
destroyAllWindows = do
    appState <- get
    let wins = T.toListBy (\_key win -> win) $ appWindows appState
    mapM_ directlyDestroyWindow wins
    put appState{ appWindows = T.empty}


--------------------------------------------------------------------------------
-- Helper
--------------------------------------------------------------------------------

addWindow :: Window -> Application l ()
addWindow win@Window{..} =
    modify $ \st ->
        let wins' = T.insert (BS.pack winTitle) win (appWindows st)
        in st{ appWindows = wins' }


{-# INLINE directlyDestroyWindow #-}
directlyDestroyWindow :: (Throws InternalException l) => Window -> Application l ()
directlyDestroyWindow Window{winHandle} = glfw $ GLFW.destroyWindow winHandle


glfw :: (Throws InternalException l, MonadIO m) => IO a -> EMT l m a
glfw m = wrapException GLFWException $ liftIO m


io :: (Throws ApplicationException l) => IO a -> Application l a
io m = wrapException IOException $ liftIO m


liftGlfw :: (Throws InternalException l) => (GLFW.Window -> IO a) -> Window -> Application l a
liftGlfw glfwAction = glfw . glfwAction . winHandle


-- TODO: efficent version
-- lookup and delete value
retrieve :: ByteString -> Trie a -> (Maybe a, Trie a)
retrieve q tri = (T.lookup q tri, T.delete q tri)


--------------------------------------------------------------------------------
-- GLFW Action Mapping
--------------------------------------------------------------------------------

iconifyWindow :: (Throws InternalException l) => Window -> Application l ()
iconifyWindow = liftGlfw GLFW.iconifyWindow


-- TODO: SomeException ? NoExceptions
makeContextCurrent :: (Throws InternalException l) => Maybe Window -> Application l ()
makeContextCurrent mwin = glfw $ GLFW.makeContextCurrent (winHandle <$> mwin)


initGlfw :: (Throws InternalException l) => Application l ()
initGlfw = do
    inited <- glfw $ GLFW.init
    unless inited (throw $ GLFWException . toException $ InitException)

terminateGlfw :: (Throws InternalException l) => Application l ()
terminateGlfw = glfw $ GLFW.terminate

