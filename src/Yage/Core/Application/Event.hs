{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}

module Yage.Core.Application.Event where

import           Yage.Prelude

import           Control.Monad                  (liftM, sequence, join)
import           Control.Monad.Reader           (asks)
import           Control.Concurrent.STM         (TQueue, atomically, tryReadTQueue)

import           Data.Set                       hiding (map)
import           Data.List                      (map)

import           Yage.Core.Application.Types
import           Yage.Core.Application.Utils
import           Yage.Core.Application.Internal.Event
import           Yage.Core.Application.Exception
import           Yage.Core.Application.Logging
import           Yage.Core.GLFW.Callback        hiding (Key)
import           Yage.Core.GLFW.Event


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
              => Application l (Set Event)
collectEvents = liftM fromList $ handleEventsWith return



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
