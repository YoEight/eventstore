{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.DeleteStream.Message
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.DeleteStream.Message where

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
      , _hardDelete      :: Optional 4 (Value Bool)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode Request

--------------------------------------------------------------------------------
newRequest :: Text -> Int32 -> Bool -> Maybe Bool -> Request
newRequest stream_id exp_ver req_master hard_delete =
    Request
    { _streamId        = putField stream_id
    , _expectedVersion = putField exp_ver
    , _requireMaster   = putField req_master
    , _hardDelete      = putField hard_delete
    }

--------------------------------------------------------------------------------
data Response
    = Response
      { _result          :: Required 1 (Enumeration OpResult)
      , _message         :: Optional 2 (Value Text)
      , _preparePosition :: Optional 3 (Value Int64)
      , _commitPosition  :: Optional 4 (Value Int64)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode Response
