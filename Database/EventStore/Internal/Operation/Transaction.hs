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
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Transaction.Message
import Database.EventStore.Internal.Operation.Write.Common
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Settings
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Start transaction operation.
transactionStart :: Settings -> Text -> ExpectedVersion -> Operation Int64
transactionStart Settings{..} stream exp_v = construct $ do
    let msg = newStart stream (expVersionInt32 exp_v) s_requireMaster
    resp <- send transactionStartCmd transactionStartCompletedCmd msg
    let tid = getField $ _transId resp
        r   = getField $ _result resp
    case r of
        OP_PREPARE_TIMEOUT        -> retry
        OP_FORWARD_TIMEOUT        -> retry
        OP_COMMIT_TIMEOUT         -> retry
        OP_WRONG_EXPECTED_VERSION -> wrongVersion stream exp_v
        OP_STREAM_DELETED         -> streamDeleted stream
        OP_INVALID_TRANSACTION    -> invalidTransaction
        OP_ACCESS_DENIED          -> accessDenied $ StreamName stream
        OP_SUCCESS                -> yield tid

--------------------------------------------------------------------------------
-- | Transactional write operation.
transactionWrite :: Settings
                 -> Text
                 -> ExpectedVersion
                 -> Int64
                 -> [Event]
                 -> Operation ()
transactionWrite Settings{..} stream exp_v trans_id evts = construct $ do
    nevts <- traverse eventToNewEvent evts
    let msg = newWrite trans_id nevts s_requireMaster
    resp <- send transactionWriteCmd transactionWriteCompletedCmd msg
    let r = getField $ _wwResult resp
    case r of
        OP_PREPARE_TIMEOUT        -> retry
        OP_FORWARD_TIMEOUT        -> retry
        OP_COMMIT_TIMEOUT         -> retry
        OP_WRONG_EXPECTED_VERSION -> wrongVersion stream exp_v
        OP_STREAM_DELETED         -> streamDeleted stream
        OP_INVALID_TRANSACTION    -> invalidTransaction
        OP_ACCESS_DENIED          -> accessDenied $ StreamName stream
        OP_SUCCESS                -> yield ()

--------------------------------------------------------------------------------
-- | Transactional commit operation.
transactionCommit :: Settings
                  -> Text
                  -> ExpectedVersion
                  -> Int64
                  -> Operation WriteResult
transactionCommit Settings{..} stream exp_v trans_id = construct $ do
    let msg = newCommit trans_id s_requireMaster
    resp <- send transactionCommitCmd transactionCommitCompletedCmd msg
    let r = getField $ _ccResult resp
        com_pos = getField $ _commitPosition resp
        pre_pos = getField $ _preparePosition resp
        lst_num = getField $ _lastNumber resp
        p_int   = fromMaybe (-1) pre_pos
        c_int   = fromMaybe (-1) com_pos
        pos     = Position c_int p_int
        res     = WriteResult lst_num pos
    case r of
        OP_PREPARE_TIMEOUT        -> retry
        OP_FORWARD_TIMEOUT        -> retry
        OP_COMMIT_TIMEOUT         -> retry
        OP_WRONG_EXPECTED_VERSION -> wrongVersion stream exp_v
        OP_STREAM_DELETED         -> streamDeleted stream
        OP_INVALID_TRANSACTION    -> invalidTransaction
        OP_ACCESS_DENIED          -> accessDenied $ StreamName stream
        OP_SUCCESS                -> yield res
