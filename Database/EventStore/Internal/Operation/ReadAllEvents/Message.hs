{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.ReadAllEvents.Message
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.ReadAllEvents.Message where

--------------------------------------------------------------------------------
import Data.Int
import GHC.Generics

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Text

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Batch read on $all stream request.
data Request
    = Request
      { _commitPosition  :: Required 1 (Value Int64)
      , _preparePosition :: Required 2 (Value Int64)
      , _maxCount        :: Required 3 (Value Int32)
      , _resolveLinkTos  :: Required 4 (Value Bool)
      , _requireMaster   :: Required 5 (Value Bool)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode Request

--------------------------------------------------------------------------------
-- | 'Request' smart constructor.
newRequest :: Int64
           -> Int64
           -> Int32
           -> Bool
           -> Bool
           -> Request
newRequest c_pos p_pos max_c res_link_tos req_master =
    Request
    { _commitPosition  = putField c_pos
    , _preparePosition = putField p_pos
    , _maxCount        = putField max_c
    , _resolveLinkTos  = putField res_link_tos
    , _requireMaster   = putField req_master
    }

--------------------------------------------------------------------------------
-- | Enumeration detailing the possible outcomes of reading a slice of $all
--   stream.
data Result
    = SUCCESS
    | NOT_MODIFIED
    | ERROR
    | ACCESS_DENIED
    deriving (Eq, Enum, Show)

--------------------------------------------------------------------------------
-- | Batch read on $all stream response.
data Response
    = Response
      { _CommitPosition      :: Required 1 (Value Int64)
      , _PreparePosition     :: Required 2 (Value Int64)
      , _Events              :: Repeated 3 (Message ResolvedEventBuf)
      , _NextCommitPosition  :: Required 4 (Value Int64)
      , _NextPreparePosition :: Required 5 (Value Int64)
      , _Result              :: Optional 6 (Enumeration Result)
      , _Error               :: Optional 7 (Value Text)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode Response
