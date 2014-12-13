{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.ReadStreamEventsOperation
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.ReadStreamEventsOperation
    ( StreamEventsSlice(..)
    , readStreamEventsOperation
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
data ReadStreamEvents
    = ReadStreamEvents
      { readStreamId             :: Required 1 (Value Text)
      , readStreamEventNumber    :: Required 2 (Value Int32)
      , readStreamMaxCount       :: Required 3 (Value Int32)
      , readStreamResolveLinkTos :: Required 4 (Value Bool)
      , readStreamRequireMaster  :: Required 5 (Value Bool)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
newReadStreamEvents :: Text
                    -> Int32
                    -> Int32
                    -> Bool
                    -> Bool
                    -> ReadStreamEvents
newReadStreamEvents stream_id evt_num max_c res_link_tos req_master =
    ReadStreamEvents
    { readStreamId             = putField stream_id
    , readStreamEventNumber    = putField evt_num
    , readStreamMaxCount       = putField max_c
    , readStreamResolveLinkTos = putField res_link_tos
    , readStreamRequireMaster  = putField req_master
    }

--------------------------------------------------------------------------------
instance Encode ReadStreamEvents

--------------------------------------------------------------------------------
data ReadStreamResult
    = RS_SUCCESS
    | RS_NO_STREAM
    | RS_STREAM_DELETED
    | RS_NOT_MODIFIED
    | RS_ERROR
    | RS_ACCESS_DENIED
    deriving (Eq, Enum, Show)

--------------------------------------------------------------------------------
data ReadStreamEventsCompleted
    = ReadStreamEventsCompleted
      { readSECEvents             :: Repeated 1 (Message ResolvedIndexedEvent)
      , readSECResult             :: Required 2 (Enumeration ReadStreamResult)
      , readSECNextNumber         :: Required 3 (Value Int32)
      , readSECLastNumber         :: Required 4 (Value Int32)
      , readSECEndOfStream        :: Required 5 (Value Bool)
      , readSECLastCommitPosition :: Required 6 (Value Int64)
      , readSECError              :: Optional 7 (Value Text)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode ReadStreamEventsCompleted

--------------------------------------------------------------------------------
data StreamEventsSlice
    = StreamEventsSlice
      { streamEventsSliceResult    :: !ReadStreamResult
      , streamEventsSliceStreamId  :: !Text
      , streamEventsSliceStart     :: !Int32
      , streamEventsSliceNext      :: !Int32
      , streamEventsSliceLast      :: !Int32
      , streamEventsSliceIsEOS     :: !Bool
      , streamEventsSliceEvents    :: ![ResolvedEvent]
      , streamEventsSliceDirection :: !ReadDirection
      }
    deriving Show

--------------------------------------------------------------------------------
newStreamEventsSlice :: Text
                     -> Int32
                     -> ReadDirection
                     -> ReadStreamEventsCompleted
                     -> StreamEventsSlice
newStreamEventsSlice stream_id start dir reco = ses
  where
    evts = getField $ readSECEvents reco

    ses = StreamEventsSlice
          { streamEventsSliceResult    = getField $ readSECResult reco
          , streamEventsSliceStreamId  = stream_id
          , streamEventsSliceStart     = start
          , streamEventsSliceNext      = getField $ readSECNextNumber reco
          , streamEventsSliceLast      = getField $ readSECLastNumber reco
          , streamEventsSliceIsEOS     = getField $ readSECEndOfStream reco
          , streamEventsSliceEvents    = fmap newResolvedEvent evts
          , streamEventsSliceDirection = dir
          }

--------------------------------------------------------------------------------
readStreamEventsOperation :: Settings
                          -> ReadDirection
                          -> TMVar (OperationExceptional StreamEventsSlice)
                          -> Text
                          -> Int32
                          -> Int32
                          -> Bool
                          -> OperationParams
readStreamEventsOperation settings dir mvar stream_id start cnt res_link_tos =
    OperationParams
    { opSettings    = settings
    , opRequestCmd  = req
    , opResponseCmd = resp

    , opRequest =
        let req_master = _requireMaster settings
            request    = newReadStreamEvents stream_id
                                             start
                                             cnt
                                             res_link_tos
                                             req_master in
         return request

    , opSuccess = inspect mvar dir stream_id start
    , opFailure = failed mvar
    }
  where
    req = case dir of
              Forward  -> 0xB2
              Backward -> 0xB4

    resp = case dir of
               Forward  -> 0xB3
               Backward -> 0xB5

--------------------------------------------------------------------------------
inspect :: TMVar (OperationExceptional StreamEventsSlice)
        -> ReadDirection
        -> Text
        -> Int32
        -> ReadStreamEventsCompleted
        -> IO Decision
inspect mvar dir stream_id start rsec = go (getField $ readSECResult rsec)
  where
    may_err = getField $ readSECError rsec

    go RS_ERROR         = failed mvar (ServerError may_err)
    go RS_ACCESS_DENIED = failed mvar (AccessDenied stream_id)
    go _                = succeed mvar dir stream_id start rsec

--------------------------------------------------------------------------------
succeed :: TMVar (OperationExceptional StreamEventsSlice)
        -> ReadDirection
        -> Text
        -> Int32
        -> ReadStreamEventsCompleted
        -> IO Decision
succeed mvar dir stream_id start rsec = do
    atomically $ putTMVar mvar (Right ses)
    return EndOperation
  where
    ses = newStreamEventsSlice stream_id start dir rsec

--------------------------------------------------------------------------------
failed :: TMVar (OperationExceptional StreamEventsSlice)
       -> OperationException
       -> IO Decision
failed mvar e = do
    atomically $ putTMVar mvar (Left e)
    return EndOperation
