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
import Database.EventStore.Internal.Types
import Test.Tasty.Hspec

--------------------------------------------------------------------------------
data Foo = Foo deriving Typeable

--------------------------------------------------------------------------------
newtype Counter = Counter (TVar Int)

--------------------------------------------------------------------------------
newCounter :: IO Counter
newCounter = Counter <$> newTVarIO 0

--------------------------------------------------------------------------------
incrCounter :: Counter -> IO ()
incrCounter (Counter var) = atomically $ do
  i <- readTVar var
  writeTVar var (i+1)

--------------------------------------------------------------------------------
readCounterSTM :: Counter -> STM Int
readCounterSTM (Counter var) = readTVar var

--------------------------------------------------------------------------------
testDisc :: Discovery
testDisc = staticEndPointDiscovery "localhost" 1234

--------------------------------------------------------------------------------
testSettings :: Settings
testSettings =
  defaultSettings { s_loggerSettings = testLoggerSettings }

--------------------------------------------------------------------------------
secs :: Int
secs = 1000 * 1000

--------------------------------------------------------------------------------
testLoggerSettings :: LoggerSettings
testLoggerSettings = LoggerSettings
                     { loggerLevel = Debug
                     , loggerType  = LogNone
                     }

--------------------------------------------------------------------------------
initLogManager :: ActionWith LogManager -> IO ()
initLogManager action = do
  logMgr <- newLogManager testLoggerSettings
  action logMgr
