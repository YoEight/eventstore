--------------------------------------------------------------------------------
-- |
-- Module : Test.Operation
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Test.Operation (spec) where

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Serialize
import Database.EventStore.Internal.Test hiding (i)

--------------------------------------------------------------------------------
import Test.Bogus.Connection
import Test.Common
import Test.Tasty.Hspec

--------------------------------------------------------------------------------
alwaysNotHandled :: Package -> Package
alwaysNotHandled pkg =
  pkg { packageCmd  = notHandledCmd
      , packageData = runPut $ encodeMessage msg
      }
  where
    msg = NotHandledBuf
          { notHandledReason         = putField N_NotMaster
          , notHandledAdditionalInfo = putField $ Just info
          }

    info = MasterInfoBuf
           { bufMasterExternalTcpAddr       = putField "addr"
           , bufMasterExternalTcpPort       = putField 1
           , bufMasterExternalHttpAddr      = putField "http"
           , bufMasterExternalHttpPort      = putField 1
           , bufMasterExternalSecureTcpAddr = putField Nothing
           , bufMasterExternalSecureTcpPort = putField Nothing
           }

--------------------------------------------------------------------------------
spec :: Spec
spec = beforeAll (createLoggerRef testGlobalLog) $ do
  specify "Operation manager should behave on not handled [not-master]" $ \logRef -> do
    bus <- newBus logRef testSettings
    var <- newEmptyMVar
    let builder = respondMWithConnectionBuilder $ \ept pkg -> do
            emptyVar <- isEmptyMVar var
            when (ept == EndPoint "addr" 1 && emptyVar) $
              putMVar var ()

            return $ alwaysNotHandled pkg

    exec <- newExec testSettings bus builder testDisc

    p <- newPromise
    let op = readEvent testSettings "foo" 1 True
    publishWith exec (SubmitOperation p op)

    res <- takeMVar var

    publishWith exec SystemShutdown
    execWaitTillClosed exec

    res `shouldBe` ()
