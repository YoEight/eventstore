{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.Transaction.Message
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.Transaction.Message where

--------------------------------------------------------------------------------
import Data.Int

--------------------------------------------------------------------------------
import Data.ProtocolBuffers

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Start transaction request.
data Start =
    Start
    { _streamId        :: Required 1 (Value Text)
    , _expectedVersion :: Required 2 (Value Int64)
    , _requireMaster   :: Required 3 (Value Bool)
    }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode Start

--------------------------------------------------------------------------------
-- | 'Start' smart constructor.
newStart :: Text -> Int64 -> Bool -> Start
newStart stream_id exp_ver req_master =
    Start
    { _streamId        = putField stream_id
    , _expectedVersion = putField exp_ver
    , _requireMaster   = putField req_master
    }

--------------------------------------------------------------------------------
-- | Start transaction response.
data Started =
    Started
    { _transId :: Required 1 (Value Int64)
    , _result  :: Required 2 (Enumeration OpResult)
    , _message :: Optional 3 (Value Text)
    }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode Started

--------------------------------------------------------------------------------
-- | Write transactional events request.
data Write =
    Write
    { _wTransId       :: Required 1 (Value Int64)
    , _events         :: Repeated 2 (Message NewEvent)
    , _wRequireMaster :: Required 3 (Value Bool)
    }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode Write

--------------------------------------------------------------------------------
-- | 'Write' smart constructor.
newWrite :: Int64 -> [NewEvent] -> Bool -> Write
newWrite trans_id evts req_master =
    Write
    { _wTransId       = putField trans_id
    , _events         = putField evts
    , _wRequireMaster = putField req_master
    }

--------------------------------------------------------------------------------
-- | Write transactional events response.
data Written =
    Written
    { _wwTransId :: Required 1 (Value Int64)
    , _wwResult  :: Required 2 (Enumeration OpResult)
    , _wwMessage :: Optional 3 (Value Text)
    }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode Written

--------------------------------------------------------------------------------
-- | Commit transaction request.
data Commit =
    Commit
    { _cTransId       :: Required 1 (Value Int64)
    , _cRequireMaster :: Required 2 (Value Bool)
    }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode Commit

--------------------------------------------------------------------------------
-- | 'Commit' smart constructor.
newCommit :: Int64 -> Bool -> Commit
newCommit trans_id req_master =
    Commit
    { _cTransId       = putField trans_id
    , _cRequireMaster = putField req_master
    }

--------------------------------------------------------------------------------
-- | Commit transaction response.
data Committed =
    Committed
    { _ccTransId       :: Required 1 (Value Int64)
    , _ccResult        :: Required 2 (Enumeration OpResult)
    , _ccMessage       :: Optional 3 (Value Text)
    , _firstNumber     :: Required 4 (Value Int64)
    , _lastNumber      :: Required 5 (Value Int64)
    , _preparePosition :: Optional 6 (Value Int64)
    , _commitPosition  :: Optional 7 (Value Int64)
    }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode Committed
