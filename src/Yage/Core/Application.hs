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
    , createWindow, destroyWindow, windowByTitle, iconifyWindow, makeContextCurrent
    , io
    , ApplicationException
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


-- add a convenient type signature (without Catch ...)
--execApplication :: String -> Application l a -> IO a
execApplication :: String -> EMT (Caught ApplicationException NoExceptions) (StateT ApplicationState IO) b -> IO b
execApplication title app = do
    let a = runEMT $
            (startUp >> app >>= tearDown)
            `catch` handleUncaughtExceptions
    (eResult, st') <- runStateT a (initialState { appTitle = title })
    print st'
    return eResult
    where
        startUp :: (Throws ApplicationException l) => Application l ()
        startUp = do
            inited <- io GLFW.init
            unless inited (throw InitException)

        tearDown :: (Throws ApplicationException l) => a -> Application l a
        tearDown x = do
            destroyAllWindows
            io GLFW.terminate
            return x

        handleUncaughtExceptions =
            \(e::ApplicationException) -> error ("Application Error: " ++ show e)
            `catch` \(e::SomeException) -> error ("Unexpected Error: " ++ show e)


createWindow :: (Throws ApplicationException l) => Int -> Int -> String -> Application l Window
createWindow width height title = do
    win <- io $ liftM fromJust $ mkWindow width height title
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


destroyWindow :: (Throws ApplicationException l) => Window -> Application l ()
destroyWindow Window{winTitle} = do
    appState <- get
    let (mwin, wins') = retrieve (BS.pack winTitle) $ appWindows appState
    maybe (return ()) directlyDestroyWindow mwin
    put appState{ appWindows = wins' }


destroyAllWindows :: (Throws ApplicationException l) => Application l ()
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
directlyDestroyWindow :: (Throws ApplicationException l) => Window -> Application l ()
directlyDestroyWindow Window{winHandle} = io $ GLFW.destroyWindow winHandle


io :: (Throws ApplicationException l, MonadIO m) => IO a -> EMT l m a
io m = wrapException GLFWException $ liftIO m


liftGlfw :: (Throws ApplicationException l) => (GLFW.Window -> IO a) -> Window -> Application l a
liftGlfw glfwAction = io . glfwAction . winHandle


-- TODO: efficent version
-- lookup and delete value
retrieve :: ByteString -> Trie a -> (Maybe a, Trie a)
retrieve q tri = (T.lookup q tri, T.delete q tri)


--------------------------------------------------------------------------------
-- GLFW Action Mapping
--------------------------------------------------------------------------------

iconifyWindow :: (Throws ApplicationException l) => Window -> Application l ()
iconifyWindow = liftGlfw GLFW.iconifyWindow


-- TODO: SomeException ? NoExceptions
makeContextCurrent :: (Throws ApplicationException l) => Maybe Window -> Application l ()
makeContextCurrent mwin = io $ GLFW.makeContextCurrent (winHandle <$> mwin)

