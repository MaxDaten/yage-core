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
{-# LANGUAGE UnicodeSyntax              #-}


module Yage.Core.Application
    ( Application
    , execApplication
    , Window, createWindow, windowByTitle, windowByHandle, destroyWindow
    , pollEvent
    , io

    , module Event
    , module Window
    ) where

--------------------------------------------------------------------------------

import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Char8           as BS (pack)
import           Data.Maybe                      (fromJust)
import           Data.Trie                       (Trie)
import qualified Data.Trie                       as T (delete, empty, insert,
                                                       lookup, toListBy)
import           Data.List                       (find)

import           Control.Applicative             ((<$>))
import           Control.Monad                   (liftM)
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.RWS.Strict        (evalRWST)
import           Control.Monad.State             (get, gets, put, modify)
import           Control.Monad.Reader            (ask, asks)
import           Control.Monad.Exception
import           Control.Concurrent.STM          (newTQueueIO, tryReadTQueue, atomically)

import qualified Graphics.UI.GLFW                as GLFW (Window)

import           Yage.Core.Application.Exception
import           Yage.Core.GLFW.Base
import           Yage.Core.GLFW.Window           as Window
import           Yage.Core.GLFW.Event
import           Yage.Core.GLFW.Callback
import           Yage.Core.Application.Types
import           Yage.Core.Application.Types     as Event (Event)
import           Yage.Core.Application.Event
--------------------------------------------------------------------------------

initialState :: ApplicationState
initialState = ApplicationState
    { appTitle = ""
    , appWindows = T.empty
    }

initalEnv :: IO (ApplicationEnv)
initalEnv = ApplicationEnv <$> newTQueueIO


execApplication :: (l ~ AnyException) => String -> Application l b -> IO b
execApplication title app = do
    let a = tryEMT $ runApp app
    
    env <- initalEnv

    (eResult, st') <- evalRWST a env (initialState { appTitle = title })
    print $ show st'
    case eResult of
        Right result -> return result
        Left ex -> error $ show ex
    where
        runApp app = do
            startup
            x <- app
            shutdown
            return x

        startup = do
            initGlfw
        shutdown = destroyAllWindows >> terminateGlfw


createWindow :: (Throws InternalException l) => Int -> Int -> String -> Application l Window
createWindow width height title = do
    win <- glfw $ liftM fromJust $ mkWindow width height title
    registerAllWindowCallbacks win
    addWindow win
    return win
    where 
        registerAllWindowCallbacks :: (Throws InternalException l) => Window -> Application l ()
        registerAllWindowCallbacks win = do
            tq <- asks appEventQ
            setWindowPositionCallback win $ Just $ windowPositionCallback tq
            setCursorPositionCallback win $ Just $ cursorPositionCallback tq




windowByTitle :: String -> Application l (Maybe Window)
windowByTitle title = do
    wins <- gets appWindows
    return $ T.lookup (BS.pack title) wins



-- TODO effective version
windowByHandle :: (Throws InternalException l) => GLFW.Window -> Application l Window
windowByHandle wh = do
    ws <- gets appWindows
    let wins = T.toListBy (flip const) ws
        mw   = find (\w -> winHandle w == wh) wins
    case mw of
        Just w -> return w
        Nothing -> throw InternalException




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


pollEvent :: (Throws ApplicationException l, Throws InternalException l) => Application l (Maybe Event)
pollEvent = do
    pollEvents
    queue <- asks appEventQ
    mevent <- io $ atomically $ tryReadTQueue queue
    return mevent

--------------------------------------------------------------------------------
-- Helper
--------------------------------------------------------------------------------

addWindow :: Window -> Application l ()
addWindow win@Window{..} =
    modify $ \st ->
        let wins' = T.insert (BS.pack winTitle) win (appWindows st)
        in st{ appWindows = wins' }



io :: (Throws ApplicationException l) => IO a -> Application l a
io m = wrapException IOException $ liftIO m


-- TODO: efficent version
-- lookup and delete value
retrieve :: ByteString -> Trie a -> (Maybe a, Trie a)
retrieve q tri = (T.lookup q tri, T.delete q tri)




