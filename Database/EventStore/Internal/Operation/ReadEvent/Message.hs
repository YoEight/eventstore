{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DataKinds             #-}
{-# OPTIONS_GHC -fcontext-stack=26 #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.ReadEvent.Message
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.ReadEvent.Message where

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
      { _streamId       :: Required 1 (Value Text)
      , _eventNumber    :: Required 2 (Value Int32)
      , _resolveLinkTos :: Required 3 (Value Bool)
      , _requireMaster  :: Required 4 (Value Bool)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode Request

--------------------------------------------------------------------------------
newRequest :: Text -> Int32 -> Bool -> Bool -> Request
newRequest stream_id evt_num res_link_tos req_master =
    Request
    { _streamId       = putField stream_id
    , _eventNumber    = putField evt_num
    , _resolveLinkTos = putField res_link_tos
    , _requireMaster  = putField req_master
    }

--------------------------------------------------------------------------------
-- | Enumeration representing the status of a single event read operation.
data Result
    = SUCCESS
    | NOT_FOUND
    | NO_STREAM
    | STREAM_DELETED
    | ERROR
    | ACCESS_DENIED
    deriving (Eq, Enum, Show)

--------------------------------------------------------------------------------
data Response
    = Response
      { _result       :: Required 1 (Enumeration Result)
      , _indexedEvent :: Required 2 (Message ResolvedIndexedEvent)
      , _error        :: Optional 3 (Value Text)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode Response
