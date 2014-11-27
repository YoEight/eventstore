{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.Eventstore.Internal.Operation.ReadAllEventsOperation
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.Eventstore.Internal.Operation.ReadAllEventsOperation
    ( readAllEventsOperation ) where

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
readAllEventsOperation :: Settings
                       -> ReadDirection
                       -> TMVar (OperationExceptional AllEventsSlice)
                       -> Int64
                       -> Int64
                       -> Int32
                       -> Bool
                       -> Operation
readAllEventsOperation settings dir mvar c_pos p_pos max_c res_link_tos =
    createOperation params
  where
    req = case dir of
              Forward  -> ReadAllEventsForwardCmd
              Backward -> ReadAllEventsBackwardCmd

    resp = case dir of
               Forward  -> ReadAllEventsForwardCompletedCmd
               Backward -> ReadAllEventsBackwardCompletedCmd

    params = OperationParams
             { opSettings    = settings
             , opRequestCmd  = req
             , opResponseCmd = resp

             , opRequest =
                 let req_master = _requireMaster settings
                     request    = newReadAllEvents c_pos
                                                   p_pos
                                                   max_c
                                                   res_link_tos
                                                   req_master in
                 return request

             , opSuccess = inspect mvar dir
             , opFailure = failed mvar
             }

--------------------------------------------------------------------------------
inspect :: TMVar (OperationExceptional AllEventsSlice)
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
succeed :: TMVar (OperationExceptional AllEventsSlice)
        -> ReadDirection
        -> ReadAllEventsCompleted
        -> IO Decision
succeed mvar dir raec = do
    atomically $ putTMVar mvar (Right ses)
    return EndOperation
  where
    ses = newAllEventsSlice dir raec

--------------------------------------------------------------------------------
failed :: TMVar (OperationExceptional AllEventsSlice)
       -> OperationException
       -> IO Decision
failed mvar e = do
    atomically $ putTMVar mvar (Left e)
    return EndOperation
