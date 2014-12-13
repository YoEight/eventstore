{-# LANGUAGE    DeriveGeneric      #-}
{-# LANGUAGE    DataKinds          #-}
{-# OPTIONS_GHC -fcontext-stack=26 #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.ReadEventOperation
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.ReadEventOperation
    ( ReadResult(..)
    , readEventOperation
    ) where

--------------------------------------------------------------------------------
import Control.Concurrent.STM
import Data.Int
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Text

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Manager.Operation
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data ReadEvent
    = ReadEvent
      { readEventStreamId       :: Required 1 (Value Text)
      , readEventNumber         :: Required 2 (Value Int32)
      , readEventResolveLinkTos :: Required 3 (Value Bool)
      , readEventRequireMaster  :: Required 4 (Value Bool)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode ReadEvent

--------------------------------------------------------------------------------
newReadEvent :: Text -> Int32 -> Bool -> Bool -> ReadEvent
newReadEvent stream_id evt_num res_link_tos req_master =
    ReadEvent
    { readEventStreamId       = putField stream_id
    , readEventNumber         = putField evt_num
    , readEventResolveLinkTos = putField res_link_tos
    , readEventRequireMaster  = putField req_master
    }

--------------------------------------------------------------------------------
data ReadEventResult
    = RE_SUCCESS
    | RE_NOT_FOUND
    | RE_NO_STREAM
    | RE_STREAM_DELETED
    | RE_ERROR
    | RE_ACCESS_DENIED
    deriving (Eq, Enum, Show)

--------------------------------------------------------------------------------
data ReadEventCompleted
    = ReadEventCompleted
      { readCompletedResult       :: Required 1 (Enumeration ReadEventResult)
      , readCompletedIndexedEvent :: Required 2 (Message ResolvedIndexedEvent)
      , readCompletedError        :: Optional 3 (Value Text)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode ReadEventCompleted

--------------------------------------------------------------------------------
data ReadResult
    = ReadResult
      { readResultStatus        :: !ReadEventResult
      , readResultStreamId      :: !Text
      , readResultEventNumber   :: !Int32
      , readResultResolvedEvent :: !(Maybe ResolvedEvent)
      }
    deriving Show

--------------------------------------------------------------------------------
newReadResult :: ReadEventResult
              -> Text
              -> Int32
              -> ResolvedIndexedEvent
              -> ReadResult
newReadResult status stream_id evt_num rie = rr
  where
    may_re =
        case status of
            RE_SUCCESS -> Just $ newResolvedEvent rie
            _          -> Nothing

    rr = ReadResult
         { readResultStatus        = status
         , readResultStreamId      = stream_id
         , readResultEventNumber   = evt_num
         , readResultResolvedEvent = may_re
         }

--------------------------------------------------------------------------------
readEventOperation :: Settings
                   -> TMVar (OperationExceptional ReadResult)
                   -> Text
                   -> Int32
                   -> Bool -- ^ Resolve link TOS
                   -> OperationParams
readEventOperation settings mvar stream_id evt_num res_link_tos =
    OperationParams
    { opSettings    = settings
    , opRequestCmd  = 0xB0
    , opResponseCmd = 0xB1

    , opRequest =
        let req_master = _requireMaster settings
            request    = newReadEvent stream_id
                                      evt_num
                                      res_link_tos
                                      req_master in
         return request

    , opSuccess = inspect mvar stream_id evt_num
    , opFailure = failed mvar
    }

--------------------------------------------------------------------------------
inspect :: TMVar (OperationExceptional ReadResult)
        -> Text
        -> Int32
        -> ReadEventCompleted
        -> IO Decision
inspect mvar stream_id evt_num reco = go (getField $ readCompletedResult reco)
  where
    may_err = getField $ readCompletedError reco

    go RE_ERROR         = failed mvar (ServerError may_err)
    go RE_ACCESS_DENIED = failed mvar (AccessDenied stream_id)
    go _                = succeed mvar stream_id evt_num reco

--------------------------------------------------------------------------------
succeed :: TMVar (OperationExceptional ReadResult)
        -> Text
        -> Int32
        -> ReadEventCompleted
        -> IO Decision
succeed mvar stream_id evt_num reco = do
    atomically $ putTMVar mvar (Right rr)
    return EndOperation
  where
    status = getField $ readCompletedResult reco
    rie    = getField $ readCompletedIndexedEvent reco
    rr     = newReadResult status stream_id evt_num rie

--------------------------------------------------------------------------------
failed :: TMVar (OperationExceptional ReadResult)
       -> OperationException
       -> IO Decision
failed mvar e = do
    atomically $ putTMVar mvar (Left e)
    return EndOperation
