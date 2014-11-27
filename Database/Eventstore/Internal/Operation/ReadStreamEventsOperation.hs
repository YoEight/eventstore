--------------------------------------------------------------------------------
-- |
-- Module : Database.Eventstore.Internal.Operation.ReadStreamEventsOperation
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.Eventstore.Internal.Operation.ReadStreamEventsOperation
    ( readStreamEventsOperation ) where

--------------------------------------------------------------------------------
import Control.Concurrent.STM
import Data.Int
import Data.Maybe

--------------------------------------------------------------------------------
import Data.Text

--------------------------------------------------------------------------------
import Database.Eventstore.Internal.Operation.Common
import Database.Eventstore.Internal.Types

--------------------------------------------------------------------------------
readStreamEventsOperation :: Settings
                          -> ReadDirection
                          -> TMVar (OperationExceptional StreamEventsSlice)
                          -> Text
                          -> Int32
                          -> Int32
                          -> Bool
                          -> Operation
readStreamEventsOperation settings dir mvar stream_id start count res_link_tos =
    createOperation params
  where
    req = case dir of
              Forward  -> ReadStreamEventsForwardCmd
              Backward -> ReadStreamEventsBackwardCmd

    resp = case dir of
               Forward  -> ReadStreamEventsForwardCompletedCmd
               Backward -> ReadStreamEventsBackwardCompletedCmd

    params = OperationParams
             { opSettings    = settings
             , opRequestCmd  = req
             , opResponseCmd = resp

             , opRequest =
                 let req_master = _requireMaster settings
                     request    = newReadStreamEvents stream_id
                                                      start
                                                      count
                                                      res_link_tos
                                                      req_master in
                 return request

             , opSuccess = inspect mvar dir stream_id start
             , opFailure = failed mvar
             }

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
