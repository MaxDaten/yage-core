{-# LANGUAGE FlexibleContexts           #-}

module Yage.Core.Application.Logging
    ( Logger
    , getAppLogger, getWinLogger
    , logM, debugM, infoM, noticeM, warningM, errorM, criticalM, alertM, emergencyM
    
    , Logger.Priority(..), Logger.getLogger, Logger.removeAllHandlers
    , Logger.setHandlers, Logger.addHandler, Logger.rootLoggerName, Logger.setLevel, Logger.getLevel
    , Logger.updateGlobalLogger, Logger.saveGlobalLogger
    
    , LogHandler.LogHandler, LogHandler.setFormatter
    , Formatter.simpleLogFormatter
    , HandlerSimple.streamHandler
    ) where

import           Control.Monad.Reader            (asks)

import           System.Log.Logger               (Logger, logL)
import qualified System.Log.Logger               as Logger
import qualified System.Log.Handler              as LogHandler
import qualified System.Log.Handler.Simple       as HandlerSimple
import qualified System.Log.Formatter            as Formatter

import           Yage.Core.Application.Types
import           Yage.Core.Application.Exception
import           Yage.Core.Application.Utils

getAppLogger :: Application l Logger
getAppLogger = asks $ snd . appLogger

getWinLogger :: Window -> Logger
getWinLogger = snd . winLogger

debugM :: (Throws InternalException l) => String -> Application l ()
debugM = logM Logger.DEBUG

infoM :: (Throws InternalException l) => String -> Application l ()
infoM = logM Logger.INFO

noticeM :: (Throws InternalException l) => String -> Application l ()
noticeM = logM Logger.NOTICE

warningM :: (Throws InternalException l) => String -> Application l ()
warningM = logM Logger.WARNING

errorM :: (Throws InternalException l) => String -> Application l ()
errorM = logM Logger.ERROR

criticalM :: (Throws InternalException l) => String -> Application l ()
criticalM = logM Logger.CRITICAL

alertM :: (Throws InternalException l) => String -> Application l ()
alertM = logM Logger.ALERT

emergencyM :: (Throws InternalException l) => String -> Application l ()
emergencyM = logM Logger.EMERGENCY


logM :: (Throws InternalException l) => Logger.Priority -> String -> Application l ()
logM pri msg = do
    l <- getAppLogger
    io $ logL l pri msg