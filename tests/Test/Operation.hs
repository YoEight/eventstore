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
import Database.EventStore.Internal.Connection
import Database.EventStore.Internal.Exec
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.Operations
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
import Test.Bogus.Connection
import Test.Common
import Test.Tasty
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
spec = do
  specify "Operation manager should behave on not handled" $ do
    let builder = respondWithConnectionBuilder alwaysNotHandled

    exec <- newExec testSettings builder testDisc

    var <- newEmptyMVar
    subscribe exec $ \ForceReconnect{} ->
      putMVar var ()

    p <- newPromise
    let op = readEvent testSettings "foo" 1 True
    publish exec (SubmitOperation p op)

    res <- takeMVar var
    res `shouldBe` ()
