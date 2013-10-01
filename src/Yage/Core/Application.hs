{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Yage.Core.Application
    ( Application
    , execApplication
    , createWindow, destroyWindow, windowByTitle, iconifyWindow, makeContextCurrent
    , io
    , ApplicationException
    ) where

-- Types
import qualified Data.Trie as T (empty, insert, delete, lookup, toListBy)
import           Data.Trie (Trie)
import qualified Data.ByteString.Char8 as BS (pack)
import           Data.ByteString (ByteString)
import           Data.Maybe (fromJust)

-- concepts
import           Control.Monad.State
import           Control.Monad.Exception
import           Control.Monad (liftM, unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Applicative

-- 3rd party apis
import qualified Graphics.UI.GLFW as GLFW (
      init, terminate
    , Window, createWindow, destroyWindow, iconifyWindow, makeContextCurrent
    )

import           Yage.Core.ExceptionFD

type Application l a = EMT l (StateT ApplicationState IO) a


data Window = Window
    { winTitle          :: !String
    , winWidth          :: !Int
    , winHeight         :: !Int
    , winHandle         :: !GLFW.Window
    }
    deriving (Show)


data ApplicationState = ApplicationState
    { appTitle          :: !String
    , appWindows        :: Trie (Window)
    }
    deriving (Show)

data ApplicationException
    = ApplicationException
    | InitException
    | GLFWException SomeException
    deriving (Show, Typeable)

instance Exception ApplicationException

initialState :: ApplicationState
initialState = ApplicationState
    { appTitle = ""
    , appWindows = T.empty
    }


-- add a convenient type signature (without Catch ...)
--execApplication :: (Throws ApplicationException l) => String -> Application l a -> IO a
-- execApplication :: String -> Application (Caught ApplicationException l) a -> IO a
execApplication title app = do
    let a = runEMT $ 
            (startUp >> app >>= tearDown)
            `catch` \(e::ApplicationException) -> error ("Application Error: " ++ show e)
            `catch` \(e::SomeException) -> error ("Unexpected Error: " ++ show e)
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

        -- handleExceptions :: e -> IO a
        handleUncaughtExceptions = undefined -- error $ show e 


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
    case mwin of
        Just win -> directlyDestroyWindow win
        Nothing -> return ()
    put appState{ appWindows = wins' }


destroyAllWindows :: (Throws ApplicationException l) => Application l ()
destroyAllWindows = do
    appState <- get
    let wins = T.toListBy (\_key win -> win) $ appWindows appState
    mapM_ directlyDestroyWindow wins
    put appState{ appWindows = T.empty }


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

