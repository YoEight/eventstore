{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.ReadAllEventsOperation
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.ReadAllEventsOperation
    ( AllEventsSlice(..)
    , ReadAllResult(..)
    , readAllEventsOperation
    ) where

--------------------------------------------------------------------------------
import Control.Concurrent
import Data.Int
import Data.Maybe
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
import Data.Text hiding (null)
import Data.ProtocolBuffers

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Manager.Operation
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data ReadAllEvents
    = ReadAllEvents
      { readAllEventsCommitPosition  :: Required 1 (Value Int64)
      , readAllEventsPreparePosition :: Required 2 (Value Int64)
      , readAllEventsMaxCount        :: Required 3 (Value Int32)
      , readAllEventsResolveLinkTos  :: Required 4 (Value Bool)
      , readAllEventsRequireMaster   :: Required 5 (Value Bool)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode ReadAllEvents

--------------------------------------------------------------------------------
newReadAllEvents :: Int64
                 -> Int64
                 -> Int32
                 -> Bool
                 -> Bool
                 -> ReadAllEvents
newReadAllEvents c_pos p_pos max_c res_link_tos req_master =
    ReadAllEvents
    { readAllEventsCommitPosition  = putField c_pos
    , readAllEventsPreparePosition = putField p_pos
    , readAllEventsMaxCount        = putField max_c
    , readAllEventsResolveLinkTos  = putField res_link_tos
    , readAllEventsRequireMaster   = putField req_master
    }

--------------------------------------------------------------------------------
data ReadAllResult
    = RA_SUCCESS
    | RA_NOT_MODIFIED
    | RA_ERROR
    | RA_ACCESS_DENIED
    deriving (Eq, Enum, Show)

--------------------------------------------------------------------------------
data ReadAllEventsCompleted
    = ReadAllEventsCompleted
      { readAECCommitPosition      :: Required 1 (Value Int64)
      , readAECPreparePosition     :: Required 2 (Value Int64)
      , readAECEvents              :: Repeated 3 (Message ResolvedEventBuf)
      , readAECNextCommitPosition  :: Required 4 (Value Int64)
      , readAECNextPreparePosition :: Required 5 (Value Int64)
      , readAECResult              :: Optional 6 (Enumeration ReadAllResult)
      , readAECError               :: Optional 7 (Value Text)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode ReadAllEventsCompleted

--------------------------------------------------------------------------------
data AllEventsSlice
    = AllEventsSlice
      { allEventsSliceResult    :: !ReadAllResult
      , allEventsSliceFrom      :: !Position
      , allEventsSliceNext      :: !Position
      , allEventsSliceIsEOS     :: !Bool
      , allEventsSliceEvents    :: ![ResolvedEvent]
      , allEventsSliceDirection :: !ReadDirection
      }
    deriving Show

--------------------------------------------------------------------------------
newAllEventsSlice :: ReadDirection -> ReadAllEventsCompleted -> AllEventsSlice
newAllEventsSlice dir raec = aes
  where
    res      = fromMaybe RA_SUCCESS (getField $ readAECResult raec)
    evts     = fmap newResolvedEventFromBuf (getField $ readAECEvents raec)
    r_com    = getField $ readAECCommitPosition raec
    r_pre    = getField $ readAECPreparePosition raec
    r_n_com  = getField $ readAECNextCommitPosition raec
    r_n_pre  = getField $ readAECNextPreparePosition raec
    from_pos = Position r_com r_pre
    next_pos = Position r_n_com r_n_pre

    aes = AllEventsSlice
          { allEventsSliceResult    = res
          , allEventsSliceFrom      = from_pos
          , allEventsSliceNext      = next_pos
          , allEventsSliceIsEOS     = null evts
          , allEventsSliceEvents    = evts
          , allEventsSliceDirection = dir
          }

--------------------------------------------------------------------------------
readAllEventsOperation :: Settings
                       -> ReadDirection
                       -> MVar (OperationExceptional AllEventsSlice)
                       -> Int64
                       -> Int64
                       -> Int32
                       -> Bool
                       -> OperationParams
readAllEventsOperation settings dir mvar c_pos p_pos max_c res_link_tos =
    OperationParams
    { opSettings    = settings
    , opRequestCmd  = req
    , opResponseCmd = resp

    , opRequest =
        let req_master = s_requireMaster settings
            request    = newReadAllEvents c_pos
                                          p_pos
                                          max_c
                                          res_link_tos
                                          req_master in
         return request

    , opSuccess = inspect mvar dir
    , opFailure = failed mvar
    }
  where
    req = case dir of
              Forward  -> 0xB6
              Backward -> 0xB8

    resp = case dir of
               Forward  -> 0xB7
               Backward -> 0xB9

--------------------------------------------------------------------------------
inspect :: MVar (OperationExceptional AllEventsSlice)
        -> ReadDirection
        -> ReadAllEventsCompleted
        -> IO Decision
inspect mvar dir raec = go res
  where
    res     = fromMaybe RA_SUCCESS (getField $ readAECResult raec)
    may_err = getField $ readAECError raec

    go RA_ERROR         = failed mvar (ServerError may_err)
    go RA_ACCESS_DENIED = failed mvar (AccessDenied "$all")
    go _                = succeed mvar dir raec

--------------------------------------------------------------------------------
succeed :: MVar (OperationExceptional AllEventsSlice)
        -> ReadDirection
        -> ReadAllEventsCompleted
        -> IO Decision
succeed mvar dir raec = do
    putMVar mvar (Right ses)
    return EndOperation
  where
    ses = newAllEventsSlice dir raec

--------------------------------------------------------------------------------
failed :: MVar (OperationExceptional AllEventsSlice)
       -> OperationException
       -> IO Decision
failed mvar e = do
    putMVar mvar (Left e)
    return EndOperation
