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
{-# LANGUAGE TupleSections              #-}


module Yage.Core.Application
    ( Application, ApplicationConfig(..), WindowHint(..), OpenGLProfile(..)
    , execApplication
    , Window, createWindow, windowByTitle, windowByHandle, destroyWindow
    , withWindowAsCurrent, withWindowHints, createWindowWithHints
    , pollEvent
    , io

    , module Event
    , module Window
    ) where

--------------------------------------------------------------------------------
import           Yage.Prelude

import qualified Data.ByteString.Char8           as BS (pack)
import           Data.Trie                       (Trie)
import qualified Data.Trie                       as T (delete, empty, insert,
                                                       lookup, toListBy)
import           Data.List                       (find, (++), filter)
import           Data.Char                       (isAlphaNum)

import           Control.Monad                   (mapM, mapM_, liftM)
import           Control.Monad.RWS.Strict        (evalRWST)
import           Control.Monad.State             (get, gets, put, modify)
import           Control.Monad.Reader            (asks)
import           Control.Monad.Exception
import           Control.Concurrent.STM          (newTQueueIO, tryReadTQueue, atomically)

import           System.IO                       (stderr)

import qualified Graphics.UI.GLFW                as GLFW (Window)

import           Yage.Core.Application.Exception
import           Yage.Core.GLFW.Base
import           Yage.Core.GLFW.Window           as Window
import           Yage.Core.GLFW.Event
import           Yage.Core.Application.Types
import           Yage.Core.Application.Types     as Event (Event)
import           Yage.Core.Application.Event
import           Yage.Core.Application.Logging
import qualified Yage.Core.Application.LogHandler as LogHandler
import           Yage.Core.Application.Utils
--------------------------------------------------------------------------------

initialState :: ApplicationState
initialState = ApplicationState
    { app'title = ""
    , app'windows = T.empty
    }

initalEnv :: String -> ApplicationConfig -> IO (ApplicationEnv)
initalEnv title conf = 
    let loggerName = "app." ++ clearAppTitle title
    in ApplicationEnv
            <$> newTQueueIO
            <*> pure conf
            <*> ((loggerName,) <$> getLogger loggerName)
    where
        clearAppTitle :: String -> String
        clearAppTitle = filter (isAlphaNum)



execApplication :: (l ~ AnyException) => String -> ApplicationConfig -> Application l b -> IO b
execApplication title conf app = do
    let a = tryEMT $ runApp app
    rootL <- getRootLogger
    
    env <- initalEnv title conf

    (eResult, st') <- evalRWST a env (initialState { app'title = title })
    
    logL rootL NOTICE $ format "Final state:[{0}]" [show st']

    case eResult of
        Right result -> removeAllHandlers >> return result
        Left ex -> do
            logL rootL CRITICAL $ format "exception" [show ex]
            error $ show ex
    where
        runApp app = do
            startup
            x <- app
            shutdown
            return x

        startup = do
            setupLogging
            initGlfw
            debugM . ("glfw-version: " ++) . show =<< getGLFWVersion
            registerGlobalErrorCallback =<< Just <$> getAppLogger

        shutdown = destroyAllWindows >> terminateGlfw

        setupLogging = do
            prio <- asks $ conf'logPriority . app'config
            io $ do
                h <- streamHandler stderr DEBUG >>= \lh -> return $
                     LogHandler.setFormatter lh (coloredLogFormatter "[$utcTime : $loggername : $prio]\t $msg")
                
                -- TODO set only app logger                 
                updateGlobalLogger rootLoggerName (setHandlers [h])
                updateGlobalLogger rootLoggerName (setLevel prio)


createWindow :: (Throws InternalException l) => Int -> Int -> String -> Application l Window
createWindow width height title = do
    win <- mkWindow width height title
    registerAllWindowCallbacks win
    addWindow win
    withWindowAsCurrent win $ \win -> mapM_ (debugM . winS win) =<< windowInfo win
    return win
    where 
        registerAllWindowCallbacks :: (Throws InternalException l) => Window -> Application l ()
        registerAllWindowCallbacks win = do
            tq <- asks app'eventQ
            ml <- Just <$> getAppLogger
            registerWindowCallbacks win tq ml
        winS :: Window -> ShowS
        winS win = ((show . win'handle $ win) ++)

createWindowWithHints :: (Throws InternalException l) => [WindowHint] -> Int -> Int -> String -> Application l Window
createWindowWithHints hints width height title = withWindowHints hints $ \_ -> createWindow width height title


withWindowAsCurrent :: (Throws InternalException l) => Window -> (Window -> Application l a) -> Application l a
withWindowAsCurrent win f = do
    makeContextCurrent $ Just win
    r <- f win
    makeContextCurrent Nothing
    return r


windowByTitle :: String -> Application l (Maybe Window)
windowByTitle title = do
    wins <- gets app'windows
    return $ T.lookup (BS.pack title) wins



-- TODO effective version
windowByHandle :: (Throws InternalException l) => GLFW.Window -> Application l Window
windowByHandle wh = do
    ws <- gets app'windows
    let wins = T.toListBy (flip const) ws
        mw   = find (\w -> win'handle w == wh) wins
    case mw of
        Just w -> return w
        Nothing -> throw . InternalException . toException $ InvalidWindowHandleException




destroyWindow :: (Throws InternalException l) => Window -> Application l ()
destroyWindow Window{win'title} = do
    appState <- get
    let (mwin, wins') = retrieve (BS.pack win'title) $ app'windows appState
    maybe (return ()) directlyDestroyWindow mwin
    put appState{ app'windows = wins' }



destroyAllWindows :: (Throws InternalException l) => Application l ()
destroyAllWindows = do
    appState <- get
    let wins = T.toListBy (\_key win -> win) $ app'windows appState
    mapM_ directlyDestroyWindow wins
    put appState{ app'windows = T.empty}


--------------------------------------------------------------------------------


pollEvent :: (Throws InternalException l) => Application l (Maybe Event)
pollEvent = do
    pollEvents
    queue <- asks app'eventQ
    mevent <- ioe $ atomically $ tryReadTQueue queue
    return mevent

--------------------------------------------------------------------------------
-- Helper
--------------------------------------------------------------------------------

addWindow :: Window -> Application l ()
addWindow win@Window{..} =
    modify $ \st ->
        let wins' = T.insert (BS.pack win'title) win (app'windows st)
        in st{ app'windows = wins' }


-- TODO: efficent version
-- lookup and delete value
retrieve :: ByteString -> Trie a -> (Maybe a, Trie a)
retrieve q tri = (T.lookup q tri, T.delete q tri)


{-# INLINE mkWindow #-}
mkWindow :: (Throws InternalException l) => Int -> Int -> String -> Application l Window
mkWindow width height title = do
    wh <- createWindowHandle width height title Nothing Nothing
    logger <- getWindowLogger wh
    return $ Window title (width, height) wh logger
    where
        getWindowLogger wh = do
            appLogger <- asks app'logger
            let loggerName = fst appLogger ++ "." ++ (show wh)
            ioe $ (loggerName,) <$> getLogger loggerName

-- TODO maybe make current
windowInfo :: (Throws InternalException l) => Window -> Application l [String]
windowInfo win = do
    api <- getWindowClientAPI win
    ver <- getWindowContextGLVersion win
    prof <- getWindowGLProfile win
    return [show api, show ver, show prof]


withWindowHints :: (Throws InternalException l) => [WindowHint] -> ([WindowHint] -> Application l a) -> Application l a
withWindowHints hints ma = do
    setGlobalWindowHints hints
    r <- ma hints
    revertGlobalWindowHints
    return r
