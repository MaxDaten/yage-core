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
    , Window, createWindow, windowByTitle, destroyWindow
    , io

    , module Callbacks
    , module Event
    , module Window
    ) where

--------------------------------------------------------------------------------

-- Types
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Char8           as BS (pack)
import           Data.Maybe                      (fromJust)
import           Data.Trie                       (Trie)
import qualified Data.Trie                       as T (delete, empty, insert,
                                                       lookup, toListBy)

-- concepts
import           Control.Monad.Exception
import           Control.Monad.IO.Class          ()
import           Control.Monad.State


import           Yage.Core.Application.Exception
import           Yage.Core.GLFW.Base
import           Yage.Core.GLFW.Window           as Window
import           Yage.Core.GLFW.Event            as Event
import           Yage.Core.GLFW.Callback         as Callbacks
import           Yage.Core.Application.Types

--------------------------------------------------------------------------------

initialState :: ApplicationState
initialState = ApplicationState
    { appTitle = ""
    , appWindows = T.empty
    }


execApplication :: (l ~ AnyException) => String -> Application l b -> IO b
execApplication title app = do
    let a = tryEMT $ runApp app

    (eResult, st') <- runStateT a (initialState { appTitle = title })
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

        startup = initGlfw
        shutdown = destroyAllWindows >> terminateGlfw


createWindow :: (Throws InternalException l) => Int -> Int -> String -> Application l Window
createWindow width height title = do
    win <- glfw $ liftM fromJust $ mkWindow width height title
    addWindow win
    return win



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



io :: (Throws ApplicationException l) => IO a -> Application l a
io m = wrapException IOException $ liftIO m


-- TODO: efficent version
-- lookup and delete value
retrieve :: ByteString -> Trie a -> (Maybe a, Trie a)
retrieve q tri = (T.lookup q tri, T.delete q tri)
