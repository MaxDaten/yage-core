module Yage.Core.Application.Internal.Event where

import           Yage.Prelude


import           Control.Concurrent.STM         (TQueue, atomically, writeTQueue)

import           Yage.Core.Application.EventTypes
import           Yage.Core.Application.Types
import           Yage.Core.Application.Logging

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
