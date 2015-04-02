{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.TransactionStartOperation
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.TransactionStartOperation
    ( transactionStartOperation ) where

--------------------------------------------------------------------------------
import Control.Concurrent
import Control.Exception
import Data.Int
import Data.Maybe
#if MIN_VERSION_base(4,8,0)
#else
import Data.Traversable
#endif

--------------------------------------------------------------------------------
import Control.Concurrent.Async
import Data.ProtocolBuffers
import Data.Text

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Manager.Operation
import Database.EventStore.Internal.Processor
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data TransactionEnv
    = TransactionEnv
      { _transSettings        :: Settings
      , _transProcessor       :: Processor
      , _transStreamId        :: Text
      , _transExpectedVersion :: ExpectedVersion
      }

--------------------------------------------------------------------------------
transactionStartOperation :: Settings
                          -> Processor
                          -> MVar (OperationExceptional Transaction)
                          -> Text
                          -> ExpectedVersion
                          -> OperationParams
transactionStartOperation settings procss mvar stream_id exp_ver =
    OperationParams
    { opSettings    = settings
    , opRequestCmd  = 0x84
    , opResponseCmd = 0x85

    , opRequest =
        let req_master  = s_requireMaster settings
            exp_ver_int = expVersionInt32 exp_ver
            request     = newTransactionStart stream_id
                                              exp_ver_int
                                              req_master in
         return request

    , opSuccess = inspectTrans env mvar
    , opFailure = failed mvar
    }
  where
    env = TransactionEnv
          { _transSettings        = settings
          , _transProcessor       = procss
          , _transStreamId        = stream_id
          , _transExpectedVersion = exp_ver
          }

--------------------------------------------------------------------------------
inspectTrans :: TransactionEnv
             -> MVar (OperationExceptional Transaction)
             -> TransactionStartCompleted
             -> IO Decision
inspectTrans env mvar tsc = go (getField $ transactionSCResult tsc)
  where
    go OP_SUCCESS                = succeedTrans env mvar tsc
    go OP_PREPARE_TIMEOUT        = return Retry
    go OP_FORWARD_TIMEOUT        = return Retry
    go OP_COMMIT_TIMEOUT         = return Retry
    go OP_WRONG_EXPECTED_VERSION = failed mvar wrong_version
    go OP_STREAM_DELETED         = failed mvar (StreamDeleted stream_id)
    go OP_INVALID_TRANSACTION    = failed mvar InvalidTransaction
    go OP_ACCESS_DENIED          = failed mvar (AccessDenied stream_id)

    exp_ver       = _transExpectedVersion env
    stream_id     = _transStreamId env
    wrong_version = WrongExpectedVersion stream_id exp_ver

--------------------------------------------------------------------------------
succeedTrans :: TransactionEnv
             -> MVar (OperationExceptional Transaction)
             -> TransactionStartCompleted
             -> IO Decision
succeedTrans env mvar tsc = do
    putMVar mvar (Right trans)
    return EndOperation
  where
    trans_id = getField $ transactionSCId tsc
    trans    = createTransaction env trans_id

--------------------------------------------------------------------------------
failed :: MVar (OperationExceptional a)
       -> OperationException
       -> IO Decision
failed mvar e = do
    putMVar mvar (Left e)
    return EndOperation

--------------------------------------------------------------------------------
createTransaction :: TransactionEnv -> Int64 -> Transaction
createTransaction env@TransactionEnv{..} trans_id = trans
  where
    trans = Transaction
            { transactionId              = trans_id
            , transactionStreamId        = _transStreamId
            , transactionExpectedVersion = _transExpectedVersion

            , transactionCommit = do
                  (as, mvar) <- createAsync

                  let op = transactionCommitOperation env trans_id mvar

                  processorNewOperation _transProcessor op
                  return as

            , transactionSendEvents = \evts -> do
                  (as, mvar) <- createAsync

                  let op = transactionWriteOperation env trans_id mvar evts

                  processorNewOperation _transProcessor op
                  return as

            , transactionRollback = return ()
            }

--------------------------------------------------------------------------------
transactionWriteOperation :: TransactionEnv
                          -> Int64
                          -> MVar (OperationExceptional ())
                          -> [Event]
                          -> OperationParams
transactionWriteOperation env trans_id mvar evts =
    OperationParams
    { opSettings    = settings
    , opRequestCmd  = 0x86
    , opResponseCmd = 0x87

    , opRequest = do
        new_evts <- traverse eventToNewEvent evts

        let request = newTransactionWrite trans_id
                      new_evts
                      req_master

        return request

    , opSuccess = inspectWrite env mvar
    , opFailure = failed mvar
    }
  where
    settings   = _transSettings env
    req_master = s_requireMaster settings

--------------------------------------------------------------------------------
transactionCommitOperation :: TransactionEnv
                           -> Int64
                           -> MVar (OperationExceptional WriteResult)
                           -> OperationParams
transactionCommitOperation env trans_id mvar =
    OperationParams
    { opSettings    = settings
    , opRequestCmd  = 0x88
    , opResponseCmd = 0x89

    , opRequest =
        let request = newTransactionCommit trans_id req_master in

         return request

    , opSuccess = inspectCommit env mvar
    , opFailure = failed mvar
    }
  where
    settings   = _transSettings env
    req_master = s_requireMaster settings

--------------------------------------------------------------------------------
inspectWrite :: TransactionEnv
             -> MVar (OperationExceptional ())
             -> TransactionWriteCompleted
             -> IO Decision
inspectWrite env mvar twc = go (getField $ transactionWCResult twc)
  where
    go OP_SUCCESS                = succeedWrite mvar twc
    go OP_PREPARE_TIMEOUT        = return Retry
    go OP_FORWARD_TIMEOUT        = return Retry
    go OP_COMMIT_TIMEOUT         = return Retry
    go OP_WRONG_EXPECTED_VERSION = failed mvar wrong_version
    go OP_STREAM_DELETED         = failed mvar (StreamDeleted stream_id)
    go OP_INVALID_TRANSACTION    = failed mvar InvalidTransaction
    go OP_ACCESS_DENIED          = failed mvar (AccessDenied stream_id)

    exp_ver       = _transExpectedVersion env
    stream_id     = _transStreamId env
    wrong_version = WrongExpectedVersion stream_id exp_ver

--------------------------------------------------------------------------------
succeedWrite :: MVar (OperationExceptional ())
             -> TransactionWriteCompleted
             -> IO Decision
succeedWrite mvar _ = do
    putMVar mvar (Right ())
    return EndOperation

--------------------------------------------------------------------------------
inspectCommit :: TransactionEnv
              -> MVar (OperationExceptional WriteResult)
              -> TransactionCommitCompleted
              -> IO Decision
inspectCommit env mvar tcc = go (getField $ transactionCCResult tcc)
  where
    go OP_SUCCESS                = succeedCommit mvar tcc
    go OP_PREPARE_TIMEOUT        = return Retry
    go OP_FORWARD_TIMEOUT        = return Retry
    go OP_COMMIT_TIMEOUT         = return Retry
    go OP_WRONG_EXPECTED_VERSION = failed mvar wrong_version
    go OP_STREAM_DELETED         = failed mvar (StreamDeleted stream_id)
    go OP_INVALID_TRANSACTION    = failed mvar InvalidTransaction
    go OP_ACCESS_DENIED          = failed mvar (AccessDenied stream_id)

    exp_ver       = _transExpectedVersion env
    stream_id     = _transStreamId env
    wrong_version = WrongExpectedVersion stream_id exp_ver

--------------------------------------------------------------------------------
succeedCommit :: MVar (OperationExceptional WriteResult)
              -> TransactionCommitCompleted
              -> IO Decision
succeedCommit mvar tcc = do
    putMVar mvar (Right wr)
    return EndOperation
  where
    last_evt_num = getField $ transactionCCLastNumber tcc
    com_pos      = getField $ transactionCCCommitPosition tcc
    pre_pos      = getField $ transactionCCPreparePosition tcc
    com_pos_int  = fromMaybe (-1) com_pos
    pre_pos_int  = fromMaybe (-1) pre_pos
    pos          = Position com_pos_int pre_pos_int
    wr           = WriteResult last_evt_num pos

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

--------------------------------------------------------------------------------
createAsync :: IO (Async a, MVar (OperationExceptional a))
createAsync = do
    mvar <- newEmptyMVar
    as   <- async $ do
        res <- readMVar mvar
        either throwIO return res

    return (as, mvar)
