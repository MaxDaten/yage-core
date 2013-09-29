{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
module Yage.Core.Application
    ( Application
    , execApplication
    , createWindow
    , io
    ) where

-- Types
import qualified Data.Trie as T (Trie, empty, insert)
import qualified Data.ByteString.Char8 as BS (pack)

-- concepts
import           Control.Monad.State
import           Control.Monad.IO.Class
import           Control.Exception
import           Control.Applicative

-- 3rd party apis
import qualified Graphics.UI.GLFW as GLFW (
      init, terminate
    , Window, createWindow, destroyWindow
    , Monitor
    )

-- own
--import           EnableGUI -- for ghci with ghc >= 78
--import           Yage.Core.Types
--import           Yage.Core.API


newtype Application a = Application (StateT (ApplicationState) IO a)
    deriving (Monad, MonadIO, MonadState (ApplicationState))

newtype Window = Window GLFW.Window
    deriving Show

data WindowState = WindowState
    { winTitle          :: !String
    , winWidth          :: !Int
    , winHeight         :: !Int
    --, winRatio          :: !Double
    , winHandle         :: !GLFW.Window
    }

data ApplicationState = ApplicationState
    { appTitle          :: !String
    , appWindows        :: T.Trie (WindowState)
    }

initialState :: ApplicationState
initialState = ApplicationState
    { appTitle = ""
    , appWindows = T.empty
    }


execApplication :: String -> Application a -> IO (Either String a)
execApplication title (Application a) = do
    inited <- GLFW.init
    if inited 
        then do
            (a, st') <- runStateT a $ initialState { appTitle = title }
            destroyAllWindows $ appWindows st'
            GLFW.terminate
            return $ Right a
        else do
            return $ Left "Init failed"

createWindow :: Int -> Int -> String -> Application (Maybe Window)
createWindow width height title = do
    mwin <- liftIO $ GLFW.createWindow width height title Nothing Nothing
    -- addWindow <$> (WindowState title width height <$> mwin) -- need a nice way
    addWindow' (WindowState title width height <$> mwin)
    return $ Window <$> mwin

    where 
        addWindow' :: Maybe WindowState -> Application ()
        addWindow' = maybe (return ()) addWindow


-------------------------------------------------------------------------------
-- helper

addWindow :: WindowState -> Application ()
addWindow win@WindowState{..} = modify $ \st -> st{ appWindows = T.insert (BS.pack winTitle) win (appWindows st) }

destroyAllWindows :: T.Trie (WindowState) -> IO ()
destroyAllWindows ws = 
    (return $ fmap (GLFW.destroyWindow . winHandle) ws) >> return ()

io :: MonadIO m => IO a -> m a
io = liftIO
