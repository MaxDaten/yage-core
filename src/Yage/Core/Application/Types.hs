{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE RankNTypes           #-}



module Yage.Core.Application.Types
    ( Application, ApplicationState(..), ApplicationEnv(..), ApplicationConfig(..)
    , Window(..), WindowHandle, WindowConfig(..)

    , GLFWError
    , GLFW.OpenGLProfile(..)
    , GLFW.WindowHint(..)

    , module Types
    ) where

import           Yage.Prelude                 hiding (pass)
import           Data.Version                 (Version)

import           Control.Monad.Exception
import           Control.Concurrent.STM       (TVar)
import           Control.Monad.RWS.Strict     (RWST)
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Trie                    (Trie)

import           System.Log.Logger            (Logger)
import qualified System.Log.Logger            as Logger (Priority)
import           System.Log.Formatter         (LogFormatter)

import qualified Graphics.UI.GLFW             as GLFW (WindowHint(..), OpenGLProfile(..), Window, Error)

import           Yage.Core.Application.Internal.Types          as Types

--------------------------------------------------------------------------------
type GLFWError = GLFW.Error

--------------------------------------------------------------------------------

type Application l a = EMT l (RWST ApplicationEnv () ApplicationState IO) a


data WindowConfig = WindowConfig
    { windowSize  :: (Int, Int)
    , windowHints :: [GLFW.WindowHint]
    }


type WindowHandle = GLFW.Window


data Window = Window
    { winTitle   :: !String
    , winHandle  :: !WindowHandle
    , winLogger  :: (String, Logger) -- | Logger-Name and Logger
    , winState   :: TVar WindowState
    }


data ApplicationState = ApplicationState
    { appTitle   :: !String
    , appWindows :: Trie Window
    , appGCTime  :: Double           -- | in seconds
    }
    deriving (Show)

data ApplicationEnv = ApplicationEnv
    { appConfig  :: ApplicationConfig
    , appLogger  :: (String, Logger) -- | Logger-Name and Logger
    , coreversion :: Version
    }


data ApplicationConfig = ApplicationConfig
    { logPriority  :: Logger.Priority
    , logFormatter :: forall a. LogFormatter a
    }

--------------------------------------------------------------------------------

instance Show Window where
    show Window {winTitle} =
        format "Window: {0} - state: {1}" [winTitle, "N/A"]


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


