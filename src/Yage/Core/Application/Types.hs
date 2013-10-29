{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}


module Yage.Core.Application.Types
    ( Application, ApplicationState(..), ApplicationEnv(..), ApplicationConfig(..)
    , Window(..), WindowHandle, GLFWError

    , GLFW.OpenGLProfile(..)
    , GLFW.WindowHint(..)

    , module EventTypes
    ) where

import           Yage.Prelude                 hiding (pass)
import           Data.Version                 (Version)

import           Control.Monad.Exception
import           Control.Concurrent.STM       (TQueue)
import           Control.Monad.RWS.Strict     (RWST)
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Trie                    (Trie)

import           System.Log.Logger            (Logger)
import qualified System.Log.Logger            as Logger (Priority)

import qualified Graphics.UI.GLFW             as GLFW (WindowHint(..), OpenGLProfile(..), Window)

import Yage.Core.Application.EventTypes       as EventTypes

--------------------------------------------------------------------------------
type GLFWError = Error

--------------------------------------------------------------------------------

type Application l a = EMT l (RWST ApplicationEnv () ApplicationState IO) a

type WindowHandle = GLFW.Window

data Window = Window
    { winTitle  :: !String
    , winSize   :: !(Int, Int)
    , winHandle :: !WindowHandle
    , winLogger :: (String, Logger) -- | Logger-Name and Logger
    }

instance Show Window where
    show Window {winTitle, winSize} =
        format "Window: {0} - Size: {1}" [winTitle, show winSize]


data ApplicationState = ApplicationState
    { appTitle   :: !String
    , appWindows :: Trie Window
    }
    deriving (Show)

data ApplicationEnv = ApplicationEnv
    { appEventQ  :: TQueue Event
    , appConfig  :: ApplicationConfig
    , appLogger  :: (String, Logger) -- | Logger-Name and Logger
    , coreversion :: Version
    }


data ApplicationConfig = ApplicationConfig
    { confLogPriority :: Logger.Priority }

--------------------------------------------------------------------------------


-- from: http://hackage.haskell.org/package/control-monad-exception-monadsfd-0.10.3/src/extensions/Control/Monad/Exception/MonadsFD.hs
instance MonadState s m => MonadState s (EMT l m) where
    put = lift . put
    get = lift get

instance MonadReader r m => MonadReader r (EMT l m) where
    ask = lift ask
    local f m = EMT (local f (unEMT m))

instance (Monoid w, MonadWriter w m) => MonadWriter w (EMT l m) where
    tell   = lift . tell
    listen m = EMT $ do
               (res, w) <- listen (unEMT m)
               return (fmap (\x -> (x,w)) res)
    pass m   = EMT $ pass $ do
               a <- unEMT m
               case a of
                 Left  l     -> return (Left l, id)
                 Right (r,f) -> return (Right r, f)


