{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.ReadStreamEvents.Message
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.ReadStreamEvents.Message where

--------------------------------------------------------------------------------
import Data.Int

--------------------------------------------------------------------------------
import Data.ProtocolBuffers

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Batch read on regular stream request.
data Request
    = Request
      { _streamId       :: Required 1 (Value Text)
      , _eventNumber    :: Required 2 (Value Int64)
      , _maxCount       :: Required 3 (Value Int32)
      , _resolveLinkTos :: Required 4 (Value Bool)
      , _requireMaster  :: Required 5 (Value Bool)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
-- | 'Request' smart constructor.
newRequest :: Text -> Int64 -> Int32 -> Bool -> Bool -> Request
newRequest stream_id evt_num max_c res_link_tos req_master =
    Request
    { _streamId       = putField stream_id
    , _eventNumber    = putField evt_num
    , _maxCount       = putField max_c
    , _resolveLinkTos = putField res_link_tos
    , _requireMaster  = putField req_master
    }

--------------------------------------------------------------------------------
instance Encode Request

--------------------------------------------------------------------------------
-- | Enumeration detailing the possible outcomes of reading a slice of a stream
data Result
    = SUCCESS
    | NO_STREAM
    | STREAM_DELETED
    | NOT_MODIFIED
    | ERROR
    | ACCESS_DENIED
    deriving (Eq, Enum, Show)

--------------------------------------------------------------------------------
-- | Batch read on regular stream response.
data Response
    = Response
      { _events             :: Repeated 1 (Message ResolvedIndexedEvent)
      , _result             :: Required 2 (Enumeration Result)
      , _nextNumber         :: Required 3 (Value Int64)
      , _lastNumber         :: Required 4 (Value Int64)
      , _endOfStream        :: Required 5 (Value Bool)
      , _lastCommitPosition :: Required 6 (Value Int64)
      , _error              :: Optional 7 (Value Text)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode Response
