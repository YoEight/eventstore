{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.WriteEvents.Message
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.WriteEvents.Message where

--------------------------------------------------------------------------------
import Data.Int

--------------------------------------------------------------------------------
import Data.ProtocolBuffers

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Write events request.
data Request
    = Request
      { _streamId        :: Required 1 (Value Text)
      , _expectedVersion :: Required 2 (Value Int32)
      , _events          :: Repeated 3 (Message NewEvent)
      , _requireMaster   :: Required 4 (Value Bool)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode Request

--------------------------------------------------------------------------------
-- | 'Request' smart constructor.
newRequest :: Text        -- ^ Stream
           -> Int32       -- ^ Expected version
           -> [NewEvent]  -- ^ Events
           -> Bool        -- ^ Require master
           -> Request
newRequest stream_id exp_ver evts req_master =
    Request
    { _streamId        = putField stream_id
    , _expectedVersion = putField exp_ver
    , _events          = putField evts
    , _requireMaster   = putField req_master
    }

--------------------------------------------------------------------------------
-- | Write events response.
data Response
    = Response
      { _result          :: Required 1 (Enumeration OpResult)
      , _message         :: Optional 2 (Value Text)
      , _firstNumber     :: Required 3 (Value Int32)
      , _lastNumber      :: Required 4 (Value Int32)
      , _preparePosition :: Optional 5 (Value Int64)
      , _commitPosition  :: Optional 6 (Value Int64)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode Response
