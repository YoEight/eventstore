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
module Database.Eventstore.Internal.Operation.WriteEventsOperation
    ( writeEventsOperation ) where

--------------------------------------------------------------------------------
import Control.Concurrent.STM
import Data.Maybe
import Data.Traversable

--------------------------------------------------------------------------------
import Data.Text

--------------------------------------------------------------------------------
import Database.Eventstore.Internal.Operation.Common
import Database.Eventstore.Internal.Types

--------------------------------------------------------------------------------
writeEventsOperation :: Settings
                     -> TMVar (OperationExceptional WriteResult)
                     -> Text
                     -> ExpectedVersion
                     -> [Event]
                     -> Operation
writeEventsOperation settings mvar evt_stream exp_ver evts =
    createOperation params
  where
    params =
        OperationParams
        { opSettings    = settings
        , opRequestCmd  = WriteEventsCmd
        , opResponseCmd = WriteEventsCompletedCmd

        , opRequest = do
              new_evts <- traverse eventToNewEvent evts

              let require_master = _requireMaster settings
                  exp_ver_int32  = expVersionInt32 exp_ver
                  request        = newWriteEvents evt_stream
                                                  exp_ver_int32
                                                  new_evts
                                                  require_master
              return request

        , opSuccess = inspect mvar evt_stream exp_ver
        , opFailure = failed mvar
        }

--------------------------------------------------------------------------------
inspect :: TMVar (OperationExceptional WriteResult)
        -> Text
        -> ExpectedVersion
        -> WriteEventsCompleted
        -> IO Decision
inspect mvar stream exp_ver wec = go (getField $ writeCompletedResult wec)
  where
    go OP_SUCCESS                = succeed mvar wec
    go OP_PREPARE_TIMEOUT        = return Retry
    go OP_FORWARD_TIMEOUT        = return Retry
    go OP_COMMIT_TIMEOUT         = return Retry
    go OP_WRONG_EXPECTED_VERSION = failed mvar wrong_version
    go OP_STREAM_DELETED         = failed mvar (StreamDeleted stream)
    go OP_INVALID_TRANSACTION    = failed mvar InvalidTransaction
    go OP_ACCESS_DENIED          = failed mvar (AccessDenied stream)

    wrong_version = WrongExpectedVersion stream exp_ver

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
