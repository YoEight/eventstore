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
  , Shown(..)
  , Only(..)
  , newLogManager
  , getLogger
  , logMsg
  , logFormat
  ) where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Text.Format
import Data.Text.Format.Params
import System.Log.FastLogger

--------------------------------------------------------------------------------
newtype LoggerSettings =
  LoggerSettings { loggerType :: LogType }

--------------------------------------------------------------------------------
data LogManager =
  LogManager { logCallback :: TimedFastLogger }

--------------------------------------------------------------------------------
data Logger =
  Logger { loggerName     :: Text
         , loggerCallback :: TimedFastLogger
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
  (callback, _) <- newTimedFastLogger cache (loggerType setts)
  return (LogManager callback)

--------------------------------------------------------------------------------
getLogger :: Text -> LogManager -> Logger
getLogger name mgr =
  Logger { loggerName     = name
         , loggerCallback = logCallback mgr
         }

--------------------------------------------------------------------------------
logMsg :: MonadIO m => Logger -> LogLevel -> Text -> m ()
logMsg Logger{..} lvl msg = liftIO $
  loggerCallback $ \t ->
    toLogStr t <> "eventstore:"
               <> toLogStr (logLvlTxt lvl)
               <> toLogStr ("[" <> loggerName <> "]:")
               <> toLogStr msg

--------------------------------------------------------------------------------
logFormat :: (MonadIO m, Params ps)
          => Logger
          -> LogLevel
          -> Format
          -> ps
          -> m ()
logFormat Logger{..} lvl fm ps = liftIO $
  loggerCallback $ \t ->
    toLogStr t <> "eventstore:"
               <> toLogStr (logLvlTxt lvl)
               <> toLogStr ("[" <> loggerName <> "]:")
               <> toLogStr (format fm ps)