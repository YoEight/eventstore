--------------------------------------------------------------------------------
-- |
-- Module : Database.Eventstore.Internal.Operation.DeleteStreamOperation
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.Eventstore.Internal.Operation.DeleteStreamOperation
    ( deleteStreamOperation ) where

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
deleteStreamOperation :: Settings
                      -> TMVar (OperationExceptional DeleteResult)
                      -> Text
                      -> ExpectedVersion
                      -> Maybe Bool
                      -> Operation
deleteStreamOperation settings mvar stream_id exp_ver hard_del =
    createOperation params
  where
    params = OperationParams
             { opSettings    = settings
             , opRequestCmd  = DeleteStreamCmd
             , opResponseCmd = DeleteStreamCompletedCmd

             , opRequest =
                   let req_master  = _requireMaster settings
                       exp_ver_int = expVersionInt32 exp_ver
                       request     = newDeleteStream stream_id
                                                     exp_ver_int
                                                     req_master
                                                     hard_del in
                   return request

             , opSuccess = inspect mvar stream_id exp_ver
             , opFailure = failed mvar
             }

--------------------------------------------------------------------------------
inspect :: TMVar (OperationExceptional DeleteResult)
        -> Text
        -> ExpectedVersion
        -> DeleteStreamCompleted
        -> IO Decision
inspect mvar stream exp_ver dsc = go (getField $ deleteCompletedResult dsc)
  where
    go OP_SUCCESS                = succeed mvar dsc
    go OP_PREPARE_TIMEOUT        = return Retry
    go OP_FORWARD_TIMEOUT        = return Retry
    go OP_COMMIT_TIMEOUT         = return Retry
    go OP_WRONG_EXPECTED_VERSION = failed mvar wrong_version
    go OP_STREAM_DELETED         = failed mvar (StreamDeleted stream)
    go OP_INVALID_TRANSACTION    = failed mvar InvalidTransaction
    go OP_ACCESS_DENIED          = failed mvar (AccessDenied stream)

    wrong_version = WrongExpectedVersion stream exp_ver

--------------------------------------------------------------------------------
succeed :: TMVar (OperationExceptional DeleteResult)
        -> DeleteStreamCompleted
        -> IO Decision
succeed mvar wec = do
    atomically $ putTMVar mvar (Right wr)
    return EndOperation
  where
    com_pos      = getField $ deleteCompletedCommitPosition wec
    pre_pos      = getField $ deleteCompletedPreparePosition wec
    com_pos_int  = fromMaybe (-1) com_pos
    pre_pos_int  = fromMaybe (-1) pre_pos
    pos          = Position com_pos_int pre_pos_int
    wr           = DeleteResult pos

--------------------------------------------------------------------------------
failed :: TMVar (OperationExceptional DeleteResult)
       -> OperationException
       -> IO Decision
failed mvar e = do
    atomically $ putTMVar mvar (Left e)
    return EndOperation
