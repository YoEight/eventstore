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
import Database.EventStore.Internal.Test hiding (i)

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

    publishWith exec SystemShutdown
    execWaitTillClosed exec

  specify "Connection should close on heartbeat timeout" $ \logRef -> do
    counter <- newCounter
    bus     <- newBus logRef testSettings
    let builder = silentConnectionBuilder $ incrCounter counter
        disc    = staticEndPointDiscovery "localhost" 2000

    exec <- newExec testSettings bus builder disc

    atomically $ do
      i <- readCounterSTM counter
      unless (i > 1) retrySTM

    publishWith exec SystemShutdown
    execWaitTillClosed exec
