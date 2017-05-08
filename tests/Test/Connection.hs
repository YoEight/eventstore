--------------------------------------------------------------------------------
-- |
-- Module : Test.Connection
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Test.Connection (spec) where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Connection
import Database.EventStore.Internal.Discovery
import Database.EventStore.Internal.Exec
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
import Test.Tasty
import Test.Tasty.Hspec

--------------------------------------------------------------------------------
newtype Counter = Counter (TVar Int)

--------------------------------------------------------------------------------
newCounter :: IO Counter
newCounter = Counter <$> newTVarIO 0

--------------------------------------------------------------------------------
incr :: Counter -> IO ()
incr (Counter var) = atomically $ do
  i <- readTVar var
  writeTVar var (i+1)

--------------------------------------------------------------------------------
readCounterSTM :: Counter -> STM Int
readCounterSTM (Counter var) = readTVar var

--------------------------------------------------------------------------------
bugousConnectionBuilder :: Counter -> ConnectionBuilder
bugousConnectionBuilder counter = ConnectionBuilder $ \_ -> do
  incr counter
  fail "we simulate a connection problem."

--------------------------------------------------------------------------------
spec :: Spec
spec = do
  specify "Connection should retry on connection failure" $ do
    counter <- newCounter
    let builder = bugousConnectionBuilder counter
        disc    = staticEndPointDiscovery "localhost" 2000
        setts   = defaultSettings { s_retry           = atMost 3
                                  , s_reconnect_delay = 0.25 -- seconds
                                  , s_loggerSettings  = defaultLoggerSettings
                                                        { loggerType = LogNone }
                                  }

    _ <- newExec setts builder disc

    atomically $ do
      i <- readCounterSTM counter
      when (i /= 3) retrySTM
