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
    ( readEventOperation ) where

--------------------------------------------------------------------------------
import Control.Concurrent.STM
import Data.Int

--------------------------------------------------------------------------------
import Data.Text

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation.Common
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
readEventOperation :: Settings
                   -> TMVar (OperationExceptional ReadResult)
                   -> Text
                   -> Int32
                   -> Bool -- ^ Resolve link TOS
                   -> Operation
readEventOperation settings mvar stream_id evt_num res_link_tos =
    createOperation params
  where
    params = OperationParams
             { opSettings    = settings
             , opRequestCmd  = ReadEventCmd
             , opResponseCmd = ReadEventCompletedCmd

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
