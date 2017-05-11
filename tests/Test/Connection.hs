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
import Test.Bogus.Connection
import Test.Common
import Test.Tasty
import Test.Tasty.Hspec

--------------------------------------------------------------------------------
spec :: Spec
spec = do
  specify "Connection should retry on connection failure" $ do
    counter <- newCounter
    let builder = alwaysFailConnectionBuilder $ incrCounter counter
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
