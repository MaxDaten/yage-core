module Yage.Core.Application.Types where

import           Control.Monad.Exception
import           Control.Monad.State
import qualified Graphics.UI.GLFW             as GLFW (Window)
import           Data.Trie                    (Trie)


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

