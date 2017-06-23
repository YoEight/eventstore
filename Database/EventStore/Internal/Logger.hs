{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Logger
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Logger
  ( LogManager
  , Logger
  , LogLevel(..)
  , LoggerSettings(..)
  , LogType(..)
  , defaultLoggerSettings
  , Shown(..)
  , Only(..)
  , newLogManager
  , getLogger
  , logMsg
  , logFormat
  , closeLogManager
  ) where

--------------------------------------------------------------------------------
import Data.Text.Format
import Data.Text.Format.Params
import System.Log.FastLogger

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Prelude

--------------------------------------------------------------------------------
data LoggerSettings =
  LoggerSettings { loggerType  :: LogType
                 , loggerLevel :: LogLevel
                 }

--------------------------------------------------------------------------------
defaultLoggerSettings :: LoggerSettings
defaultLoggerSettings =
  LoggerSettings { loggerType  = LogStdout 0
                 , loggerLevel = Info
                 }

--------------------------------------------------------------------------------
data LogManager =
  LogManager { logCallback :: TimedFastLogger
             , logLevel    :: LogLevel
             , _cleanUp    :: IO ()
             }

--------------------------------------------------------------------------------
data Logger =
  Logger { loggerName      :: Text
         , _loggerCallback :: TimedFastLogger
         , _loggerLevel    :: LogLevel
         }

--------------------------------------------------------------------------------
data LogLevel
  = Debug
  | Info
  | Warn
  | Error
  | Fatal
  deriving (Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
logLvlTxt :: LogLevel -> Text
logLvlTxt Debug = "[DEBUG]"
logLvlTxt Info  = "[INFO]"
logLvlTxt Warn  = "[WARN]"
logLvlTxt Error = "[ERROR]"
logLvlTxt Fatal = "[FATAL]"

--------------------------------------------------------------------------------
newLogManager :: LoggerSettings -> IO LogManager
newLogManager setts = do
  cache         <- newTimeCache simpleTimeFormat'
  (callback, cleanup) <- newTimedFastLogger cache (loggerType setts)
  return (LogManager callback (loggerLevel setts) cleanup)

--------------------------------------------------------------------------------
closeLogManager :: LogManager -> IO ()
closeLogManager LogManager{..} = _cleanUp

--------------------------------------------------------------------------------
getLogger :: Text -> LogManager -> Logger
getLogger name mgr =
  Logger { loggerName      = name
         , _loggerCallback = logCallback mgr
         , _loggerLevel    = logLevel mgr
         }

--------------------------------------------------------------------------------
logMsg :: MonadIO m => Logger -> LogLevel -> Text -> m ()
logMsg Logger{..} lvl msg
  | lvl < _loggerLevel = return ()
  | otherwise = liftIO $
    _loggerCallback $ \t ->
      toLogStr ("["`mappend` t `mappend`"]") `mappend` " eventstore "
                               `mappend` toLogStr (logLvlTxt lvl)
                               `mappend` toLogStr ("[" `mappend` loggerName `mappend` "] ")
                               `mappend` toLogStr msg
                               `mappend` "\n"

--------------------------------------------------------------------------------
logFormat :: (MonadIO m, Params ps)
          => Logger
          -> LogLevel
          -> Format
          -> ps
          -> m ()
logFormat Logger{..} lvl fm ps
  | lvl < _loggerLevel = return ()
  | otherwise = liftIO $
    _loggerCallback $ \t ->
      toLogStr ("["`mappend` t `mappend`"]") `mappend` " eventstore "
                               `mappend` toLogStr (logLvlTxt lvl)
                               `mappend` toLogStr ("[" `mappend` loggerName `mappend` "] ")
                               `mappend` toLogStr (format fm ps)
                               `mappend` "\n"
