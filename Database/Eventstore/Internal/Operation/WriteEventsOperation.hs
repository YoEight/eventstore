--------------------------------------------------------------------------------
-- |
-- Module : Database.Eventstore.Internal.Operation.WriteEventsOperation
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.Eventstore.Internal.Operation.WriteEventsOperation where

--------------------------------------------------------------------------------
import Control.Concurrent.STM
import Data.Maybe
import Data.Traversable

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Serialize.Get
import Data.Text
import Data.UUID

--------------------------------------------------------------------------------
import Database.Eventstore.Internal.Packages
import Database.Eventstore.Internal.Types

--------------------------------------------------------------------------------
writeEventsOperation :: Settings
                     -> TMVar (OperationExceptional WriteResult)
                     -> Text
                     -> ExpectedVersion
                     -> [Event]
                     -> Operation
writeEventsOperation settings mvar evt_stream exp_ver evts =
    Operation
    { operationCreatePackage = createPackage
    , operationInspect       = inspect mvar evt_stream exp_ver
    }
  where
    createPackage uuid = do
        new_evts <- traverse eventToNewEvent evts

        let require_master = _requireMaster settings
            exp_ver_int32  = expVersionInt32 exp_ver
            write_evt      = newWriteEvents evt_stream
                                            exp_ver_int32
                                            new_evts
                                            require_master

        return $ writeEventsPackage uuid None write_evt

--------------------------------------------------------------------------------
inspect :: TMVar (OperationExceptional WriteResult)
        -> Text
        -> ExpectedVersion
        -> Package
        -> IO Decision
inspect mvar stream exp_ver pack =
    case packageCmd pack of
        WriteEventsCompletedCmd -> writeEventsCompleted mvar stream exp_ver pack
        _                       -> undefined --unhandled

--------------------------------------------------------------------------------
writeEventsCompleted :: TMVar (OperationExceptional WriteResult)
                     -> Text
                     -> ExpectedVersion
                     -> Package
                     -> IO Decision
writeEventsCompleted mvar stream exp_ver pack =
    case runGet getWriteEventsCompleted bs of
        Left e    -> return EndOperation
        Right wec -> do
            case getField $ writeCompletedResult wec of
                OP_SUCCESS
                    -> succeed mvar wec
                OP_PREPARE_TIMEOUT
                    -> return Retry
                OP_FORWARD_TIMEOUT
                    -> return Retry
                OP_COMMIT_TIMEOUT
                    -> return Retry
                OP_WRONG_EXPECTED_VERSION
                    -> failed mvar (WrongExpectedVersion stream exp_ver)
                OP_STREAM_DELETED
                    -> failed mvar (StreamDeleted stream)
                OP_INVALID_TRANSACTION
                    -> failed mvar InvalidTransaction
                OP_ACCESS_DENIED
                    -> failed mvar (AccessDenied stream)
  where
    corr_id = packageCorrelation pack
    bs      = packageData pack

--------------------------------------------------------------------------------
succeed :: TMVar (OperationExceptional WriteResult)
        -> WriteEventsCompleted
        -> IO Decision
succeed mvar wec = do
    atomically $ putTMVar mvar (Right wr)
    return EndOperation
  where
    last_evt_num = getField $ writeCompletedLastNumber wec
    com_pos      = getField $ writeCompletedCommitPosition wec
    pre_pos      = getField $ writeCompletedPreparePosition wec
    com_pos_int  = fromMaybe (-1) com_pos
    pre_pos_int  = fromMaybe (-1) pre_pos
    pos          = Position com_pos_int pre_pos_int
    wr           = WriteResult last_evt_num pos

--------------------------------------------------------------------------------
failed :: TMVar (OperationExceptional WriteResult)
       -> OperationException
       -> IO Decision
failed mvar e = do
    atomically $ putTMVar mvar (Left e)
    return EndOperation

--------------------------------------------------------------------------------
eventToNewEvent :: Event -> IO NewEvent
eventToNewEvent evt =
    newEvent evt_type
             evt_data_type
             evt_metadata_type
             evt_data_bytes
             evt_metadata_bytes
  where
    evt_type           = eventType evt
    evt_data_bytes     = eventDataBytes $ eventData evt
    evt_data_type      = eventDataType $ eventData evt
    evt_metadata_bytes = eventMetadataBytes $ eventData evt
    evt_metadata_type  = eventMetadataType $ eventData evt
