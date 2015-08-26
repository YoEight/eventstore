{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.TransactionStart.Message
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.TransactionStart.Message where

--------------------------------------------------------------------------------
import Data.Int
import GHC.Generics

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Text

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Request
    = Request
      { _streamId        :: Required 1 (Value Text)
      , _expectedVersion :: Required 2 (Value Int32)
      , _requireMaster   :: Required 3 (Value Bool)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
newRequest :: Text -> Int32 -> Bool -> Request
newRequest stream_id exp_ver req_master =
    Request
    { _streamId        = putField stream_id
    , _expectedVersion = putField exp_ver
    , _requireMaster   = putField req_master
    }

--------------------------------------------------------------------------------
instance Encode Request

--------------------------------------------------------------------------------
data Response
    = Response
      { _id     :: Required 1 (Value Int64)
      , _result :: Required 2 (Enumeration OpResult)
      , _error  :: Optional 3 (Value Text)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode Response

--------------------------------------------------------------------------------
data Write
    = Write
      { _wId            :: Required 1 (Value Int64)
      , _wEvents        :: Repeated 2 (Message NewEvent)
      , _wRequireMaster :: Required 3 (Value Bool)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode Write

--------------------------------------------------------------------------------
newWrite :: Int64 -> [NewEvent] -> Bool -> Write
newWrite trans_id evts req_master =
    Write
    { _wId            = putField trans_id
    , _wEvents        = putField evts
    , _wRequireMaster = putField req_master
    }

--------------------------------------------------------------------------------
data WriteCompleted
    = WriteCompleted
      { _wcId      :: Required 1 (Value Int64)
      , _wcResult  :: Required 2 (Enumeration OpResult)
      , _wcError   :: Optional 3 (Value Text)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode WriteCompleted

--------------------------------------------------------------------------------
data Commit
    = Commit
      { _cId            :: Required 1 (Value Int64)
      , _cRequireMaster :: Required 2 (Value Bool)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode Commit

--------------------------------------------------------------------------------
newCommit :: Int64 -> Bool -> Commit
newCommit trans_id req_master =
    Commit
    { _cId            = putField trans_id
    , _cRequireMaster = putField req_master
    }

--------------------------------------------------------------------------------
data CommitCompleted
    = CommitCompleted
      { _ccId              :: Required 1 (Value Int64)
      , _ccResult          :: Required 2 (Enumeration OpResult)
      , _ccError           :: Optional 3 (Value Text)
      , _ccFirstNumber     :: Required 4 (Value Int32)
      , _ccLastNumber      :: Required 5 (Value Int32)
      , _ccPreparePosition :: Optional 6 (Value Int64)
      , _ccCommitPosition  :: Optional 7 (Value Int64)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode CommitCompleted
