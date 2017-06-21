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
import ClassyPrelude
import Data.ProtocolBuffers
import Data.Serialize
import Database.EventStore.Internal.Callback
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.EndPoint
import Database.EventStore.Internal.Exec
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.Operations
import Database.EventStore.Internal.Types

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
spec :: LogManager -> Spec
spec logMgr = do
  specify "Operation manager should behave on not handled [not-master]" $ do
    bus <- newBus logMgr "operation-not-handled-test"
    var <- newEmptyMVar
    let builder = respondMWithConnectionBuilder (asPub bus) $ \ept pkg -> do
            emptyVar <- isEmptyMVar var
            when (ept == EndPoint "addr" 1 && emptyVar) $
              putMVar var ()

            return $ alwaysNotHandled pkg

    exec <- newExec testSettings logMgr bus builder testDisc

    p <- newPromise
    let op = readEvent testSettings "foo" 1 True
    publish exec (SubmitOperation p op)

    res <- takeMVar var

    publish exec SystemShutdown
    execWaitTillClosed exec

    res `shouldBe` ()
