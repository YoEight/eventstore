{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.DeleteStreamOperation
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.DeleteStreamOperation
    ( deleteStreamOperation ) where

--------------------------------------------------------------------------------
import Control.Concurrent
import Data.Int
import Data.Maybe
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Text

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Manager.Operation
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data DeleteStream
    = DeleteStream
      { _deleteStreamId              :: Required 1 (Value Text)
      , _deleteStreamExpectedVersion :: Required 2 (Value Int32)
      , _deleteStreamRequireMaster   :: Required 3 (Value Bool)
      , _deleteStreamHardDelete      :: Optional 4 (Value Bool)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode DeleteStream

--------------------------------------------------------------------------------
newDeleteStream :: Text
                -> Int32
                -> Bool
                -> Maybe Bool
                -> DeleteStream
newDeleteStream stream_id exp_ver req_master hard_delete =
    DeleteStream
    { _deleteStreamId              = putField stream_id
    , _deleteStreamExpectedVersion = putField exp_ver
    , _deleteStreamRequireMaster   = putField req_master
    , _deleteStreamHardDelete      = putField hard_delete
    }

--------------------------------------------------------------------------------
data DeleteStreamCompleted
    = DeleteStreamCompleted
      { _deleteCompletedResult          :: Required 1 (Enumeration OpResult)
      , _deleteCompletedMessage         :: Optional 2 (Value Text)
      , _deleteCompletedPreparePosition :: Optional 3 (Value Int64)
      , _deleteCompletedCommitPosition  :: Optional 4 (Value Int64)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode DeleteStreamCompleted

--------------------------------------------------------------------------------
deleteStreamOperation :: Settings
                      -> MVar (OperationExceptional DeleteResult)
                      -> Text
                      -> ExpectedVersion
                      -> Maybe Bool
                      -> OperationParams
deleteStreamOperation settings mvar stream_id exp_ver hard_del =
    OperationParams
    { opSettings    = settings
    , opRequestCmd  = 0x8A
    , opResponseCmd = 0x8B

    , opRequest =
        let req_master  = s_requireMaster settings
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
inspect :: MVar (OperationExceptional DeleteResult)
        -> Text
        -> ExpectedVersion
        -> DeleteStreamCompleted
        -> IO Decision
inspect mvar stream exp_ver dsc = go (getField $ _deleteCompletedResult dsc)
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
succeed :: MVar (OperationExceptional DeleteResult)
        -> DeleteStreamCompleted
        -> IO Decision
succeed mvar wec = do
    putMVar mvar (Right wr)
    return EndOperation
  where
    com_pos      = getField $ _deleteCompletedCommitPosition wec
    pre_pos      = getField $ _deleteCompletedPreparePosition wec
    com_pos_int  = fromMaybe (-1) com_pos
    pre_pos_int  = fromMaybe (-1) pre_pos
    pos          = Position com_pos_int pre_pos_int
    wr           = DeleteResult pos

--------------------------------------------------------------------------------
failed :: MVar (OperationExceptional DeleteResult)
       -> OperationException
       -> IO Decision
failed mvar e = do
    putMVar mvar (Left e)
    return EndOperation
