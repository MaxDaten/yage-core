{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Yage.Core.Application.Internal.Event where

import           Yage.Prelude

import           Data.List                      (map)

import           Control.Monad.Reader           (asks)
import           Control.Monad                  (liftM, sequence, join)
import           Control.Concurrent.STM         (TQueue, atomically, tryReadTQueue, writeTQueue)

import           Yage.Core.GLFW.Callback        hiding (Key)
import           Yage.Core.GLFW.Event

import           Yage.Core.Application.Types
import           Yage.Core.Application.Logging
import           Yage.Core.Application.Exception
import           Yage.Core.Application.Utils

--------------------------------------------------------------------------------


errorCallback           :: TQueue Event -> Maybe Logger -> GLFWError -> String               -> IO ()
windowPositionCallback  :: TQueue Event -> Maybe Logger -> WindowHandle -> Int -> Int        -> IO ()
windowSizeCallback      :: TQueue Event -> Maybe Logger -> WindowHandle -> Int -> Int        -> IO ()
windowCloseCallback     :: TQueue Event -> Maybe Logger -> WindowHandle                      -> IO ()
windowRefreshCallback   :: TQueue Event -> Maybe Logger -> WindowHandle                      -> IO ()
windowFocusCallback     :: TQueue Event -> Maybe Logger -> WindowHandle -> FocusState        -> IO ()
windowIconifyCallback   :: TQueue Event -> Maybe Logger -> WindowHandle -> IconifyState      -> IO ()
framebufferSizeCallback :: TQueue Event -> Maybe Logger -> WindowHandle -> Int -> Int        -> IO ()
mouseButtonCallback     :: TQueue Event -> Maybe Logger -> WindowHandle -> MouseButton -> MouseButtonState -> ModifierKeys -> IO ()
mousePositionCallback   :: TQueue Event -> Maybe Logger -> WindowHandle -> Double -> Double  -> IO ()
mouseEnterCallback      :: TQueue Event -> Maybe Logger -> WindowHandle -> CursorState       -> IO ()
scrollCallback          :: TQueue Event -> Maybe Logger -> WindowHandle -> Double -> Double  -> IO ()
keyCallback             :: TQueue Event -> Maybe Logger -> WindowHandle -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
charCallback            :: TQueue Event -> Maybe Logger -> WindowHandle -> Char              -> IO ()

errorCallback           tc ml e s            = queueEvent tc ml $ EventError e s
windowPositionCallback  tc ml win x y        = queueEvent tc ml $ EventWindowPosition win  $ EWindowPosition x y
windowSizeCallback      tc ml win w h        = queueEvent tc ml $ EventWindowSize win      $ EWindowSize w h
windowCloseCallback     tc ml win            = queueEvent tc ml $ EventWindowClose win
windowRefreshCallback   tc ml win            = queueEvent tc ml $ EventWindowRefresh win
windowFocusCallback     tc ml win fa         = queueEvent tc ml $ EventWindowFocus win     $ EWindowFocus fa
windowIconifyCallback   tc ml win ia         = queueEvent tc ml $ EventWindowIconify win   $ EWindowIconify ia
framebufferSizeCallback tc ml win w h        = queueEvent tc ml $ EventFramebufferSize win $ EFramebufferSize w h
mouseButtonCallback     tc ml win mb mba mk  = queueEvent tc ml $ EventMouseButton win     $ EMouseButton mb mba mk
mousePositionCallback   tc ml win x y        = queueEvent tc ml $ EventMousePosition win   $ EMousePosition x y
mouseEnterCallback      tc ml win ca         = queueEvent tc ml $ EventMouseEnter win      $ EMouseEnter ca
scrollCallback          tc ml win x y        = queueEvent tc ml $ EventMouseScroll win     $ EMouseScroll x y
keyCallback             tc ml win k sc ka mk = queueEvent tc ml $ EventKey win             $ EKey k sc ka mk
charCallback            tc ml win c          = queueEvent tc ml $ EventChar win            $ EChar c

--------------------------------------------------------------------------------

queueEvent :: TQueue Event -> Maybe Logger -> Event -> IO ()
queueEvent tc ml e = do
    logEvent ml e
    atomically $ writeTQueue tc e
    where
        -- TODO Pattern-Matching and/or multiplexing
        logEvent :: Maybe Logger -> Event -> IO ()
        logEvent (Just l) e' = logL l NOTICE (show e')
        logEvent Nothing _ = return ()
--------------------------------------------------------------------------------

class EventHandler t a where
    handleEvent :: t -> EventHandling a

type EventHandling a = forall l. (Throws InternalException l, Throws ApplicationException l) => Event -> Application l a

--------------------------------------------------------------------------------


pollOneEvent :: (Throws InternalException l) => Application l (Maybe Event)
pollOneEvent = do
    pollEvents
    queue <- asks appEventQ
    ioe $ atomically $ tryReadTQueue queue



handleEventsWith :: (Throws InternalException l, Throws ApplicationException l)
                  => EventHandling a -> Application l ([a])
handleEventsWith handler = pollOneEvent >>= processEvent' []
    where
        processEvent' as Nothing = return as
        processEvent' as (Just e) = do
            internalEventHandling e
            a <- handler e
            processEvent' (a:as) =<< pollOneEvent



multiplexEvents :: (Throws InternalException l, Throws ApplicationException l, EventHandler h b)
                => [h] -> Application l [b]
multiplexEvents hs = liftM join $ handleEventsWith $ flip multiplex hs
    where
        multiplex :: (Throws InternalException l, Throws ApplicationException l, EventHandler h b) => Event -> [h] -> Application l [b]
        multiplex e = sequence . map (`handleEvent` e)



collectEvents :: (Throws InternalException l, Throws ApplicationException l)
              => Application l [Event]
collectEvents = handleEventsWith return



internalEventHandling :: EventHandling ()
internalEventHandling e = debugM $ show e



registerGlobalErrorCallback :: (Throws InternalException l) => Maybe Logger -> Application l ()
registerGlobalErrorCallback ml = do
    eventQ <- asks appEventQ
    setErrorCallback $ Just $ errorCallback eventQ ml



registerWindowCallbacks :: (Throws InternalException l) => Window -> TQueue Event -> Maybe Logger -> Application l ()
registerWindowCallbacks win tq ml = do
    -- annoying setup
    setWindowPositionCallback   win $ Just $ windowPositionCallback tq ml
    setWindowSizeCallback       win $ Just $ windowSizeCallback tq ml
    setWindowCloseCallback      win $ Just $ windowCloseCallback tq ml
    setWindowRefreshCallback    win $ Just $ windowRefreshCallback tq ml
    setWindowFocusCallback      win $ Just $ windowFocusCallback tq ml
    setWindowIconifyCallback    win $ Just $ windowIconifyCallback tq ml
    setFramebufferSizeCallback  win $ Just $ framebufferSizeCallback tq ml

    setKeyCallback              win $ Just $ keyCallback tq ml
    setCharCallback             win $ Just $ charCallback tq ml

    setMouseButtonCallback      win $ Just $ mouseButtonCallback tq ml
    setMousePositionCallback    win $ Just $ mousePositionCallback tq ml
    setMouseEnterCallback       win $ Just $ mouseEnterCallback tq ml
    setScrollCallback           win $ Just $ scrollCallback tq ml

--------------------------------------------------------------------------------
