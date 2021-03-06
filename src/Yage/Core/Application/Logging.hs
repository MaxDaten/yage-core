{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes           #-}

module Yage.Core.Application.Logging
    ( getAppLogger, getWinLogger
    , logApp, logging
    , debugLog, infoLog, noticeLog, warningLog, errorLog, criticalLog, alertLog, emergencyLog
    , coloredLogFormatter

    , module Logger
    -- , module Formatter
    , module HandlerSimple
    ) where

import           Yage.Prelude

import           Control.Monad.Reader            (asks)

import           System.Log.Logger               (Logger, logL)
import           System.Log.Logger               as Logger ( Logger, Priority(..), getLogger, getRootLogger
                                                   , removeAllHandlers, setHandlers, addHandler
                                                   , rootLoggerName, setLevel, getLevel, updateGlobalLogger
                                                   , saveGlobalLogger, logL)
import           System.Log.Handler.Simple       as HandlerSimple
import qualified System.Log.Formatter            as Formatter

import           System.Console.ANSI
import           Control.Concurrent             (myThreadId)
import           Data.Time                      (getZonedTime)

import           Yage.Core.Application.Types
import           Yage.Core.Application.Exception
import           Yage.Core.Application.Utils

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

getAppLogger :: Application l Logger
getAppLogger = asks $ snd . appLogger

getWinLogger :: Window -> Logger
getWinLogger = snd . winLogger

debugLog, infoLog, noticeLog, warningLog, errorLog, criticalLog, alertLog, emergencyLog :: (Show a, MonadApplication m) => a -> m ()

debugLog     = logApp Logger.DEBUG
infoLog      = logApp Logger.INFO
noticeLog    = logApp Logger.NOTICE
warningLog   = logApp Logger.WARNING
errorLog     = logApp Logger.ERROR
criticalLog  = logApp Logger.CRITICAL
alertLog     = logApp Logger.ALERT
emergencyLog = logApp Logger.EMERGENCY


logApp :: (Show a, MonadApplication m) => Logger.Priority -> a -> m ()
logApp p m = liftApp $ do
    l <- getAppLogger
    logging l p m

logging :: (Show a, MonadApplication m) => Logger -> Logger.Priority -> a -> m ()
logging logger pri msg = liftApp $ ioe $ Logger.logL logger pri (show msg)

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
                   ]
                color = msgColor prio
            in (\s -> color ++ s ++ reset) <$> replaceVarM vars format
        msgColor prio
            | prio == Logger.DEBUG   = debugColor
            | prio == Logger.WARNING = warnColor
            | prio == Logger.NOTICE  = noticeColor
            | prio >= Logger.ERROR   = errColor
            | otherwise              = normColor
        timeFormat = "%F %X %Z"
        normColor  = setSGRCode []
        debugColor = setSGRCode [ SetColor Foreground Vivid Blue   ]
        warnColor  = setSGRCode [ SetColor Foreground Vivid Yellow ]
        noticeColor= setSGRCode [ SetColor Foreground Dull  Yellow ]
        errColor   = setSGRCode [ SetColor Foreground Vivid Red    ]
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
                           | otherwise = liftM (s:) $ replaceVarM keyVals ss
    where
      replaceStart [] str = return ("$",str)
      replaceStart ((k,v):kvs) str | k `isPrefixOf` str = do vs <- v
                                                             return (vs, drop (length k) str)
                                   | otherwise = replaceStart kvs str
