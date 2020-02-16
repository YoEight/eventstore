{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.Transaction
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.Transaction
    ( transactionStart
    , transactionWrite
    , transactionCommit
    ) where

--------------------------------------------------------------------------------
import Data.Int
import Data.Maybe

--------------------------------------------------------------------------------
import Data.ProtocolBuffers

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Control (publishWith)
import Database.EventStore.Internal.Communication (Transmit(..))
import Database.EventStore.Internal.Exec (Exec)
import Database.EventStore.Internal.Operation (OpResult(..))
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Transaction.Message
import Database.EventStore.Internal.Operation.Write.Common
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Settings
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Start transaction operation.
transactionStart
  :: Settings
  -> Exec
  -> Text
  -> ExpectedVersion
  -> Maybe Credentials
  -> IO (Async Int64)
transactionStart Settings{..} exec stream exp_v cred
  = do m <- mailboxNew
       async $
         do let req = newStart stream (expVersionInt64 exp_v) s_requireMaster
            pkg <- createPkg transactionStartCmd cred req

            keepLooping $
              do publishWith exec (Transmit m OneTime pkg)
                 outcome <- mailboxReadDecoded m
                 case outcome of
                   Left e
                     -> throw e
                   Right resp
                     -> let tid = getField $ _transId resp
                            r   = getField $ _result resp in
                        case r of
                          OP_PREPARE_TIMEOUT -> pure Loop
                          OP_FORWARD_TIMEOUT -> pure Loop
                          OP_COMMIT_TIMEOUT -> pure Loop
                          OP_WRONG_EXPECTED_VERSION -> throw $ WrongExpectedVersion stream exp_v
                          OP_STREAM_DELETED -> throw $ StreamDeleted $ StreamName stream
                          OP_INVALID_TRANSACTION -> throw InvalidTransaction
                          OP_ACCESS_DENIED -> throw $ AccessDenied $ StreamName stream
                          OP_SUCCESS -> pure $ Break tid

--------------------------------------------------------------------------------
-- | Transactional write operation.
transactionWrite
  :: Settings
  -> Exec
  -> Text
  -> ExpectedVersion
  -> Int64
  -> [Event]
  -> Maybe Credentials
  -> IO (Async ())
transactionWrite Settings{..} exec stream exp_v trans_id evts cred
  = do m <- mailboxNew
       async $
         do nevts <- traverse eventToNewEventIO evts
            let req = newWrite trans_id nevts s_requireMaster
            pkg <- createPkg transactionWriteCmd cred req
            keepLooping $
              do publishWith exec (Transmit m OneTime pkg)
                 outcome <- mailboxReadDecoded m
                 case outcome of
                   Left e
                     -> throw e
                   Right resp
                     -> let r = getField $ _wwResult resp in
                        case r of
                          OP_PREPARE_TIMEOUT -> pure Loop
                          OP_FORWARD_TIMEOUT -> pure Loop
                          OP_COMMIT_TIMEOUT -> pure Loop
                          OP_WRONG_EXPECTED_VERSION -> throw $ WrongExpectedVersion stream exp_v
                          OP_STREAM_DELETED -> throw $ StreamDeleted $ StreamName stream
                          OP_INVALID_TRANSACTION -> throw InvalidTransaction
                          OP_ACCESS_DENIED -> throw $ AccessDenied $ StreamName stream
                          OP_SUCCESS -> pure $ Break ()

--------------------------------------------------------------------------------
-- | Transactional commit operation.
transactionCommit
  :: Settings
  -> Exec
  -> Text
  -> ExpectedVersion
  -> Int64
  -> Maybe Credentials
  -> IO (Async WriteResult)
transactionCommit Settings{..} exec stream exp_v trans_id cred
  = do m <- mailboxNew
       async $
         do let req = newCommit trans_id s_requireMaster
            pkg <- createPkg transactionCommitCmd cred req
            keepLooping $
              do publishWith exec (Transmit m OneTime pkg)
                 outcome <- mailboxReadDecoded m
                 case outcome of
                   Left e
                     -> throw e
                   Right resp
                     -> let r = getField $ _ccResult resp
                            com_pos = getField $ _commitPosition resp
                            pre_pos = getField $ _preparePosition resp
                            lst_num = getField $ _lastNumber resp
                            p_int = fromMaybe (-1) pre_pos
                            c_int = fromMaybe (-1) com_pos
                            pos = Position c_int p_int
                            res = WriteResult lst_num pos in
                        case r of
                          OP_PREPARE_TIMEOUT -> pure Loop
                          OP_FORWARD_TIMEOUT -> pure Loop
                          OP_COMMIT_TIMEOUT -> pure Loop
                          OP_WRONG_EXPECTED_VERSION -> throw $ WrongExpectedVersion stream exp_v
                          OP_STREAM_DELETED -> throw $ StreamDeleted $ StreamName stream
                          OP_INVALID_TRANSACTION -> throw InvalidTransaction
                          OP_ACCESS_DENIED -> throw $ AccessDenied $ StreamName stream
                          OP_SUCCESS -> pure $ Break res
