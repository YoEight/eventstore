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
import Control.Concurrent.STM
import Data.Maybe
import Data.Traversable

--------------------------------------------------------------------------------
import Data.Int
import Data.Text

--------------------------------------------------------------------------------
import Control.Concurrent.Async
import Database.EventStore.Internal.Operation.Common
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data TransactionEnv
    = TransactionEnv
      { _transSettings        :: Settings
      , _transChan            :: TChan Msg
      , _transStreamId        :: Text
      , _transExpectedVersion :: ExpectedVersion
      }

--------------------------------------------------------------------------------
transactionStartOperation :: Settings
                          -> TChan Msg
                          -> TMVar (OperationExceptional Transaction)
                          -> Text
                          -> ExpectedVersion
                          -> Operation
transactionStartOperation settings chan mvar stream_id exp_ver =
    createOperation params
  where
    env = TransactionEnv
          { _transSettings        = settings
          , _transChan            = chan
          , _transStreamId        = stream_id
          , _transExpectedVersion = exp_ver
          }

    params = OperationParams
             { opSettings    = settings
             , opRequestCmd  = TransactionStartCmd
             , opResponseCmd = TransactionStartCompletedCmd

             , opRequest =
                   let req_master  = _requireMaster settings
                       exp_ver_int = expVersionInt32 exp_ver
                       request     = newTransactionStart stream_id
                                                         exp_ver_int
                                                         req_master in
                    return request

             , opSuccess = inspectTrans env mvar
             , opFailure = failed mvar
             }

--------------------------------------------------------------------------------
inspectTrans :: TransactionEnv
             -> TMVar (OperationExceptional Transaction)
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
             -> TMVar (OperationExceptional Transaction)
             -> TransactionStartCompleted
             -> IO Decision
succeedTrans env mvar tsc = do
    atomically $ putTMVar mvar (Right trans)
    return EndOperation
  where
    trans_id = getField $ transactionSCId tsc
    trans    = createTransaction env trans_id

--------------------------------------------------------------------------------
failed :: TMVar (OperationExceptional a)
       -> OperationException
       -> IO Decision
failed mvar e = do
    atomically $ putTMVar mvar (Left e)
    return EndOperation

--------------------------------------------------------------------------------
createTransaction :: TransactionEnv -> Int64 -> Transaction
createTransaction env trans_id = trans
  where
    stream_id = _transStreamId env
    chan      = _transChan env
    exp_ver   = _transExpectedVersion env
    trans     = Transaction
                { transactionId              = trans_id
                , transactionStreamId        = stream_id
                , transactionExpectedVersion = exp_ver

                , transactionCommit = do
                      (as, mvar) <- createAsync

                      let op = transactionCommitOperation env trans_id mvar

                      sendMsg chan (RegisterOperation op)
                      return as

                , transactionSendEvents = \evts -> do
                      (as, mvar) <- createAsync

                      let op = transactionWriteOperation env trans_id mvar evts

                      sendMsg chan (RegisterOperation op)
                      return as

                , transactionRollback = return ()
                }

--------------------------------------------------------------------------------
transactionWriteOperation :: TransactionEnv
                          -> Int64
                          -> TMVar (OperationExceptional ())
                          -> [Event]
                          -> Operation
transactionWriteOperation env trans_id mvar evts =
    createOperation params
  where
    settings   = _transSettings env
    req_master = _requireMaster settings

    params     = OperationParams
                 { opSettings    = settings
                 , opRequestCmd  = TransactionWriteCmd
                 , opResponseCmd = TransactionWriteCompletedCmd

                 , opRequest = do
                       new_evts <- traverse eventToNewEvent evts

                       let request = newTransactionWrite trans_id
                                                           new_evts
                                                           req_master

                       return request

                 , opSuccess = inspectWrite env mvar
                 , opFailure = failed mvar
                 }

--------------------------------------------------------------------------------
transactionCommitOperation :: TransactionEnv
                           -> Int64
                           -> TMVar (OperationExceptional WriteResult)
                           -> Operation
transactionCommitOperation env trans_id mvar =
    createOperation params
  where
    settings   = _transSettings env
    req_master = _requireMaster settings

    params     = OperationParams
                 { opSettings    = settings
                 , opRequestCmd  = TransactionCommitCmd
                 , opResponseCmd = TransactionCommitCompletedCmd

                 , opRequest =
                       let request = newTransactionCommit trans_id req_master in

                       return request

                 , opSuccess = inspectCommit env mvar
                 , opFailure = failed mvar
                 }

--------------------------------------------------------------------------------
inspectWrite :: TransactionEnv
             -> TMVar (OperationExceptional ())
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
succeedWrite :: TMVar (OperationExceptional ())
             -> TransactionWriteCompleted
             -> IO Decision
succeedWrite mvar _ = do
    atomically $ putTMVar mvar (Right ())
    return EndOperation

--------------------------------------------------------------------------------
inspectCommit :: TransactionEnv
              -> TMVar (OperationExceptional WriteResult)
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
succeedCommit :: TMVar (OperationExceptional WriteResult)
              -> TransactionCommitCompleted
              -> IO Decision
succeedCommit mvar tcc = do
    atomically $ putTMVar mvar (Right wr)
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
sendMsg :: TChan Msg -> Msg -> IO ()
sendMsg chan msg = atomically $ writeTChan chan msg

--------------------------------------------------------------------------------
createAsync :: IO (Async a, TMVar (OperationExceptional a))
createAsync = do
    mvar <- atomically newEmptyTMVar
    as   <- async $ atomically $ do
        res <- readTMVar mvar
        either throwSTM return res

    return (as, mvar)
