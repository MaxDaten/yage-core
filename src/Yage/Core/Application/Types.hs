{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}


module Yage.Core.Application.Types
    ( Application, Window(..), ApplicationState(..), ApplicationEnv(..), Event(..)
    , GLFW.FocusState
    , GLFW.IconifyState
    , GLFW.MouseButton, GLFW.MouseButtonState
    , GLFW.CursorState
    , GLFW.Key, GLFW.KeyState, GLFW.ModifierKeys
    , GLFW.Error
    ) where

import           Data.Data
import           Control.Monad.Exception
import           Control.Concurrent.STM       (TQueue)
import           Control.Monad.RWS.Strict     (RWST)
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Trie                    (Trie)

import           System.Log.Logger            (Logger)
import           Text.Format

import qualified Graphics.UI.GLFW             as GLFW


type Application l a = EMT l (RWST ApplicationEnv () ApplicationState IO) a


data Window = Window
    { winTitle  :: !String
    , winSize   :: !(Int, Int)
    , winHandle :: !GLFW.Window
    , winLogger :: (String, Logger) -- | Logger-Name and Logger
    }

instance Show Window where
    show Window {winTitle, winSize} =
        format "Window: {0} - Size: {1}" [winTitle, show winSize]


data ApplicationState = ApplicationState
    { appTitle   :: !String
    , appWindows :: Trie (Window)
    }
    deriving (Show)

data ApplicationEnv = ApplicationEnv
    { appEventQ :: TQueue Event
    , appLogger :: (String, Logger) -- | Logger-Name and Logger
    }


-- mainly inspired by glfw-b-demo
data Event = Event'Error             !GLFW.Error !String
           | Event'WindowPosition    !GLFW.Window !Int !Int
           | Event'WindowSize        !GLFW.Window !Int !Int
           | Event'WindowClose       !GLFW.Window
           | Event'WindowRefresh     !GLFW.Window
           | Event'WindowFocus       !GLFW.Window !GLFW.FocusState
           | Event'WindowIconify     !GLFW.Window !GLFW.IconifyState
           | Event'FramebufferSize   !GLFW.Window !Int !Int
           | Event'MousePosition     !GLFW.Window !Double !Double
           | Event'MouseEnter        !GLFW.Window !GLFW.CursorState
           | Event'MouseButton       !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
           | Event'MouseScroll       !GLFW.Window !Double !Double
           | Event'Key               !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
           | Event'Char              !GLFW.Window !Char
           deriving (Typeable, Show, Data)



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
