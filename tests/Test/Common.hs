{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------------------
-- |
-- Module : Test.Common
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Test.Common where

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Discovery
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Settings

--------------------------------------------------------------------------------
data Foo = Foo deriving Typeable

--------------------------------------------------------------------------------
newtype Counter = Counter (TVar Int)

--------------------------------------------------------------------------------
newCounter :: IO Counter
newCounter = Counter <$> newTVarIO 0

--------------------------------------------------------------------------------
incrCounter :: Counter -> IO ()
incrCounter (Counter var) = atomically $ modifyTVar' var (+1)

--------------------------------------------------------------------------------
readCounterSTM :: Counter -> STM Int
readCounterSTM (Counter var) = readTVar var

--------------------------------------------------------------------------------
testDisc :: Discovery
testDisc = staticEndPointDiscovery "localhost" 1234

--------------------------------------------------------------------------------
testSettings :: Settings
testSettings =
  defaultSettings { s_loggerType   = testGlobalLog
                  , s_loggerFilter = LoggerLevel LevelDebug
                  }

--------------------------------------------------------------------------------
secs :: Int
secs = 1000 * 1000

--------------------------------------------------------------------------------
testStdout :: LogType
testStdout = LogStdout 0

--------------------------------------------------------------------------------
testFile :: FilePath -> LogType
testFile path = LogFileNoRotate path 0

--------------------------------------------------------------------------------
testGlobalLog :: LogType
testGlobalLog = LogNone

--------------------------------------------------------------------------------
createLoggerRef :: LogType -> IO LoggerRef
createLoggerRef tpe = newLoggerRef tpe (LoggerLevel LevelDebug) False