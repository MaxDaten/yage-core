{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes           #-}

module Yage.Core.Application.Logging
    ( getAppLogger, getWinLogger
    , logM, debugM, infoM, noticeM, warningM, errorM, criticalM, alertM, emergencyM
    , coloredLogFormatter

    , module Logger
    , module HandlerSimple
    , module Format
    ) where

import           Yage.Prelude
import           Data.List                       ((++), drop, length, isPrefixOf)

import           Control.Monad.Reader            (asks)

import           System.Log.Logger               (Logger, logL)
import           System.Log.Logger               as Logger ( Logger, Priority(..), getLogger, getRootLogger
                                                   , removeAllHandlers, setHandlers, addHandler
                                                   , rootLoggerName, setLevel, getLevel, updateGlobalLogger
                                                   , saveGlobalLogger, logL)
import           System.Log.Handler.Simple       as HandlerSimple
import qualified System.Log.Formatter            as Formatter

import           System.Console.ANSI
import           Control.Concurrent (myThreadId)
import           System.Posix.Process (getProcessID)
import           System.Locale (defaultTimeLocale)
import           Data.Time (getZonedTime,getCurrentTime,formatTime)

import           Yage.Core.Application.Types
import           Yage.Core.Application.Exception
import           Yage.Core.Application.Utils
import           Text.Format                    as Format

--------------------------------------------------------------------------------

getAppLogger :: Application l Logger
getAppLogger = asks $ snd . app'logger

getWinLogger :: Window -> Logger
getWinLogger = snd . win'logger

debugM, infoM, noticeM, warningM, errorM, criticalM, alertM, emergencyM :: (Throws InternalException l) => String -> Application l ()

debugM     = logM Logger.DEBUG
infoM      = logM Logger.INFO
noticeM    = logM Logger.NOTICE
warningM   = logM Logger.WARNING
errorM     = logM Logger.ERROR
criticalM  = logM Logger.CRITICAL
alertM     = logM Logger.ALERT
emergencyM = logM Logger.EMERGENCY


logM :: (Throws InternalException l) => Logger.Priority -> String -> Application l ()
logM pri msg = do
    l <- getAppLogger
    ioe $ logL l pri msg

--------------------------------------------------------------------------------

coloredLogFormatter :: forall a. String -> Formatter.LogFormatter a
coloredLogFormatter = formatter
    where
        formatter :: String -> Formatter.LogFormatter a
        formatter format _ (prio,msg) loggername = 
            let vars = 
                    [("time", formatTime defaultTimeLocale timeFormat <$> getZonedTime)
                   ,("utcTime", formatTime defaultTimeLocale timeFormat <$> getCurrentTime)
                   ,("msg", return msg)
                   ,("prio", return $ show prio)
                   ,("loggername", return loggername)
                   ,("tid", show <$> myThreadId)
                   ,("pid", show <$> getProcessID)
                   ]
                color = msgColor prio
            in (\s -> color ++ s ++ reset) <$> (replaceVarM vars format)
        msgColor prio
            | prio == Logger.DEBUG   = debugColor
            | prio == Logger.WARNING = warnColor
            | prio >= Logger.ERROR   = errColor
            | otherwise              = normColor
        timeFormat = "%F %X %Z"
        normColor  = setSGRCode []
        warnColor  = setSGRCode [SetColor Foreground Vivid Yellow]
        errColor   = setSGRCode [SetColor Foreground Vivid Red]
        debugColor = setSGRCode [SetColor Foreground Vivid Blue]
        reset      = normColor

--------------------------------------------------------------------------------

-- is sadly not exported
-- http://hackage.haskell.org/package/hslogger-1.2.3/docs/src/System-Log-Formatter.html#simpleLogFormatter
-- | Replace some '$' variables in a string with supplied values
replaceVarM :: [(String, IO String)] -- ^ A list of (variableName, action to get the replacement string) pairs
           -> String   -- ^ String to perform substitution on
           -> IO String   -- ^ Resulting string
replaceVarM _ [] = return []
replaceVarM keyVals (s:ss) | s=='$' = do (f,rest) <- replaceStart keyVals ss
                                         repRest <- replaceVarM keyVals rest
                                         return $ f ++ repRest
                           | otherwise = replaceVarM keyVals ss >>= return . (s:)
    where
      replaceStart [] str = return ("$",str)
      replaceStart ((k,v):kvs) str | k `isPrefixOf` str = do vs <- v
                                                             return (vs, drop (length k) str)
                                   | otherwise = replaceStart kvs str