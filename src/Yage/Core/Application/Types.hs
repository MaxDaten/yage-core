{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# OPTIONS_GHC -fno-warn-type-defaults  #-}
{-# LANGUAGE NamedFieldPuns              #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE UndecidableInstances        #-}
{-# LANGUAGE RankNTypes                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleContexts            #-}


module Yage.Core.Application.Types
    ( Application, MonadApplication(..)
    , ApplicationState(..), ApplicationEnv(..), ApplicationConfig(..)
    , Window(..), WindowHandle, WindowConfig(..)

    , GLFWError
    , GLFW.OpenGLProfile(..)
    , GLFW.WindowHint(..)

    , module Types
    ) where

import           Yage.Prelude                 hiding (pass)
import           Data.Version                 (Version)

import           Control.Monad.Exception      as Ex
import           Control.Monad.RWS.Strict     (RWST)
import           Control.Monad.RWS.Class
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.Trans.Resource
import           Data.Trie                    (Trie)

import           System.Log.Logger            (Logger)
import qualified System.Log.Logger            as Logger (Priority)
import           System.Log.Formatter         (LogFormatter)

import qualified Graphics.UI.GLFW             as GLFW (WindowHint(..), OpenGLProfile(..), Window, Error)

import           Yage.Core.Application.Internal.Types          as Types

--------------------------------------------------------------------------------
type GLFWError = GLFW.Error

--------------------------------------------------------------------------------

type Application l = (EMT l (RWST ApplicationEnv () ApplicationState (ResourceT IO)))

instance (Throws SomeException l, MonadThrow m) => MonadThrow (EMT l m) where
    throwM = throw . toException

class Monad m => MonadApplication m where
    liftApp :: Application AnyException a -> m a

instance MonadApplication (Application AnyException) where
    liftApp = id
instance (MonadApplication m) => MonadApplication (StateT s m) where
    liftApp = lift . liftApp
instance (MonadApplication m, Monoid w) => MonadApplication (RWST r w s m) where
    liftApp = lift . liftApp
instance (MonadApplication m) => MonadApplication (ReaderT r m) where
    liftApp = lift . liftApp
instance (MonadApplication m, Monoid w) => MonadApplication (WriterT w m) where
    liftApp = lift . liftApp

instance (Throws SomeException l) => MonadResource (Application l) where
    liftResourceT = lift . lift

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
    { appConfig   :: ApplicationConfig
    , appLogger   :: (String, Logger) -- | Logger-Name and Logger
    , coreversion :: Version
    }


data ApplicationConfig = ApplicationConfig
    { logPriority  :: Logger.Priority
    , logFormatter :: forall a. LogFormatter a
    }

--------------------------------------------------------------------------------

instance Show Window where
    show Window {winTitle} = show $
        format "Window: {} - state: {}" ( Shown winTitle, Shown "N/A" )


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
