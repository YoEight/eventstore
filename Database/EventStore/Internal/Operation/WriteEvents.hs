{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.WriteEvents
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.WriteEvents ( writeEvents ) where

--------------------------------------------------------------------------------
import Data.Maybe

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Serialize
import Data.Text

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Write.Common
import Database.EventStore.Internal.Operation.WriteEvents.Message
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
writeEvents :: Settings
            -> Text
            -> ExpectedVersion
            -> [NewEvent]
            -> Operation 'Init WriteResult
writeEvents Settings{..} s v evts = Operation create
  where
    create :: forall a. Input 'Init WriteResult a -> a
    create (Create uuid) =
        let msg = newRequest s (expVersionInt32 v) evts s_requireMaster
            pkg = Package
                  { packageCmd         = 0x82
                  , packageCorrelation = uuid
                  , packageData        = runPut $ encodeMessage msg
                  , packageCred        = s_credentials
                  } in
        (pkg, Operation pending)

    pending :: forall a. Input 'Pending WriteResult a -> a
    pending (Arrived Package{..})
        | packageCmd == 0x83 =
            Right $ decodeResp packageData $ \resp ->
                let r            = getField $ _result resp
                    com_pos      = getField $ _commitPosition resp
                    prep_pos     = getField $ _preparePosition resp
                    lst_num      = getField $ _lastNumber resp
                    com_pos_int  = fromMaybe (-1) com_pos
                    prep_pos_int = fromMaybe (-1) prep_pos
                    pos          = Position com_pos_int prep_pos_int
                    res          = WriteResult lst_num pos in
                case r of
                    OP_SUCCESS                -> success res
                    OP_PREPARE_TIMEOUT        -> retry create
                    OP_FORWARD_TIMEOUT        -> retry create
                    OP_COMMIT_TIMEOUT         -> retry create
                    OP_WRONG_EXPECTED_VERSION -> wrongVersion s v
                    OP_STREAM_DELETED         -> streamDeleted s
                    OP_INVALID_TRANSACTION    -> invalidTransaction
                    OP_ACCESS_DENIED          -> accessDenied (StreamName s)
        | otherwise = Left $ Operation pending
