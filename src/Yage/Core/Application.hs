{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
-- {-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- {-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE TupleSections              #-}


module Yage.Core.Application
    ( execApplication
    , defaultAppConfig
    , createWindow, windowByTitle, windowByHandle, destroyWindow
    , withWindowAsCurrent, withWindowHints, createWindowWithHints
    , globalLogLevel
    , io

    , allocate, register, release, unprotect, resourceMask
    -- * Reexports
    , module Event
    , module Window
    , module Types
    , module Exception
    , module Logging
    , module Utils
    ) where

--------------------------------------------------------------------------------
import           Yage.Prelude hiding (finally)

import qualified Data.ByteString.Char8           as BS (pack)
import           Data.Trie                       (Trie)
import qualified Data.Trie                       as T (delete, empty, insert,
                                                       lookup, toListBy)

import           Data.Char                       (isAlphaNum)
import           Data.Version                    (showVersion)

import           Control.Monad.RWS.Strict        (evalRWST)
import           Control.Monad.RWS.Class
import           Control.Monad.Exception
import           Control.Monad.Trans.Resource


import           Yage.Core.Application.Exception as Exception
import           Yage.Core.GLFW.Base
import           Yage.Core.GLFW.Window           as Window
import           Yage.Core.Application.Types     as Types  hiding (appConfig, appLogger)
import           Yage.Core.Application.Event     as Event
import           Yage.Core.Application.Logging   as Logging
import qualified Yage.Core.Application.LogHandler as LogHandler
import           Yage.Core.Application.Utils     as Utils

import           Linear
--------------------------------------------------------------------------------

import Paths_yage_core

initialState :: ApplicationState
initialState = ApplicationState
    { appTitle   = ""
    , appWindows = T.empty
    , appGCTime  = 0
    }

initalEnv :: String -> ApplicationConfig -> IO ApplicationEnv
initalEnv title conf =
    let loggerName = "app." ++ clearAppTitle title
    in ApplicationEnv conf
            <$> ((loggerName,) <$> getLogger loggerName)
            <*> pure version
    where
        clearAppTitle :: String -> String
        clearAppTitle = filter isAlphaNum


defaultAppConfig :: ApplicationConfig
defaultAppConfig = ApplicationConfig
    { logPriority   = WARNING
    , logFormatter  = coloredLogFormatter "[$utcTime : $loggername : $prio]\t $msg"
    }


execApplication :: String -> ApplicationConfig -> Application AnyException b -> IO b
execApplication title conf app = do
    -- unpeel complete monad stack
    let runApp e s = runResourceT $ evalRWST (tryEMT $ startup >> app `finally` shutdown) e s
    rootL      <- getRootLogger
    env        <- initalEnv title conf

    (eResult, st') <- runApp env (initialState { appTitle = title })

    logL rootL NOTICE $ printf "Final state:[%s]" (show st')

    case eResult of
        Right result ->
            removeAllHandlers >> return result
        Left ex      -> do
            logL rootL CRITICAL $ printf ">> Application ended unexpectedly with: %s" ( show ex )
            throwM ex

    where

    startup = do
        setupLogging
        initGlfw
        coreV <- asks coreversion
        infoLog $ (asString $ printf "yage-core:    %s" (showVersion coreV))
        infoLog $ (asString $ printf "glfw-version: %s" (showVersion glfwVersion))
        registerGlobalErrorCallback =<< getAppLogger

    shutdown :: Application AnyException ()
    shutdown = destroyAllWindows >> terminateGlfw

    setupLogging = do
        prio <- asks $ logPriority . appConfig
        fmt  <- asks $ logFormatter . appConfig
        io $ do
            h <- streamHandler stderr DEBUG >>= \lh -> return $
                 LogHandler.setFormatter lh fmt

            -- TODO set only app logger
            updateGlobalLogger rootLoggerName (setHandlers [h])
            updateGlobalLogger rootLoggerName (setLevel prio)



globalLogLevel :: l ~ AnyException => Priority -> Application l ()
globalLogLevel = io . updateGlobalLogger rootLoggerName . setLevel



createWindow :: (Throws InternalException l) => Int -> Int -> String -> Application l Window
createWindow width height title = do
    win <- mkWindow width height title
    addWindow win
    return win


createWindowWithHints :: (Throws InternalException l) => [WindowHint] -> Int -> Int -> String -> Application l (Window)
createWindowWithHints hints width height title = withWindowHints hints $ \_ -> createWindow width height title


withWindowAsCurrent :: (Throws InternalException l) => Window -> (Window -> Application l a) -> Application l a
withWindowAsCurrent win f = do
    makeContextCurrent $ Just win
    r <- f win
    makeContextCurrent Nothing
    return r


windowByTitle :: String -> Application l (Maybe (Window))
windowByTitle title = do
    wins <- gets appWindows
    return $ T.lookup (BS.pack title) wins



-- TODO effective version
windowByHandle :: (Throws InternalException l) => WindowHandle -> Application l (Window)
windowByHandle wh = do
    ws <- gets appWindows
    let wins = T.toListBy (flip const) ws
        mw   = find (\w -> winHandle w == wh) wins
    case mw of
        Just w -> return w
        Nothing -> throw $ InternalException . toException $ InvalidWindowHandleException




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


-- TODO: efficent version
-- lookup and delete value
retrieve :: ByteString -> Trie a -> (Maybe a, Trie a)
retrieve q tri = (T.lookup q tri, T.delete q tri)


mkWindow :: (Throws InternalException l) => Int -> Int -> String -> Application l Window
mkWindow width height title = do
    wh <- createWindowHandle width height title Nothing Nothing
    logger     <- getWindowLogger wh
    winVar     <- ioe $ newTVarIO =<< initialWindowState wh
    return $ Window title wh logger winVar
    where
        getWindowLogger wh = do
            appLogger      <- asks appLogger
            let loggerName = fst appLogger ++ "." ++ show wh
            ioe $ (loggerName,) <$> getLogger loggerName

        initialWindowState win =
                WindowState <$> (uncurry V2 <$> getWindowPos win)
                            <*> (uncurry V2 <$> getWindowSize win)
                            <*> ((FocusState'Focused ==) <$> getWindowFocused win)
                            <*> ((IconifyState'Iconified ==) <$> getWindowIconified win)
                            <*> pure False
                            <*> pure False
                            <*> pure True -- FIXME: manual check
                            <*> (uncurry V2 <$> getFramebufferSize win)

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
