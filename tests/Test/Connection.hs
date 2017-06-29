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
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Discovery
import Database.EventStore.Internal.Exec
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.Prelude

--------------------------------------------------------------------------------
import Test.Bogus.Connection
import Test.Common
import Test.Tasty.Hspec

spec :: Spec
spec = beforeAll (createLoggerRef testGlobalLog) $ do
  specify "Connection should retry on connection failure" $ \logRef -> do
    counter <- newCounter
    bus     <- newBus logRef testSettings
    let builder = alwaysFailConnectionBuilder $ incrCounter counter
        disc    = staticEndPointDiscovery "localhost" 2000

    exec <- newExec testSettings bus builder disc

    atomically $ do
      i <- readCounterSTM counter
      when (i /= 3) retrySTM

    publish exec SystemShutdown
    execWaitTillClosed exec
