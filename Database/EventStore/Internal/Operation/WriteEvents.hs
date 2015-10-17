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
module Database.EventStore.Internal.Operation.WriteEvents
    ( writeEvents
    , writeEventsSM
    ) where

--------------------------------------------------------------------------------
import Data.Maybe

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Serialize
import Data.Text
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Write.Common
import Database.EventStore.Internal.Operation.WriteEvents.Message
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
writeEventsSM :: Settings
              -> Text
              -> ExpectedVersion
              -> [NewEvent]
              -> UUID
              -> SM WriteResult ()
writeEventsSM Settings{..} s v evts uuid = do
    let msg = newRequest s (expVersionInt32 v) evts s_requireMaster
        pkg = Package
              { packageCmd         = 0x82
              , packageCorrelation = uuid
              , packageData        = runPut $ encodeMessage msg
              , packageCred        = s_credentials
              }
    Package{..} <- send pkg
    if packageCmd == exp_cmd
        then do
            resp <- decodeResp packageData
            let r            = getField $ _result resp
                com_pos      = getField $ _commitPosition resp
                prep_pos     = getField $ _preparePosition resp
                lst_num      = getField $ _lastNumber resp
                com_pos_int  = fromMaybe (-1) com_pos
                prep_pos_int = fromMaybe (-1) prep_pos
                pos          = Position com_pos_int prep_pos_int
                res          = WriteResult lst_num pos
            case r of
                OP_SUCCESS                -> yield res
                OP_PREPARE_TIMEOUT        -> retry
                OP_FORWARD_TIMEOUT        -> retry
                OP_COMMIT_TIMEOUT         -> retry
                OP_WRONG_EXPECTED_VERSION -> wrongVersion s v
                OP_STREAM_DELETED         -> streamDeleted s
                OP_INVALID_TRANSACTION    -> invalidTransaction
                OP_ACCESS_DENIED          -> accessDenied (StreamName s)
        else invalidServerResponse exp_cmd packageCmd
  where
    exp_cmd = 0x83

--------------------------------------------------------------------------------
writeEvents :: Settings
            -> Text
            -> ExpectedVersion
            -> [NewEvent]
            -> Operation WriteResult
writeEvents setts s v evts = operation $ writeEventsSM setts s v evts
