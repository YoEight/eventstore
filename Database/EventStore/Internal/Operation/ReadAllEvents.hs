{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.ReadAllEvents
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.ReadAllEvents
    ( readAllEvents ) where

--------------------------------------------------------------------------------
import Data.Int
import Data.Maybe

--------------------------------------------------------------------------------
import Data.ProtocolBuffers

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Read.Common
import Database.EventStore.Internal.Operation.ReadAllEvents.Message
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Settings
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Batch read on $all stream operation.
readAllEvents :: Settings
              -> Int64
              -> Int64
              -> Int32
              -> Bool
              -> ReadDirection
              -> Maybe Credentials
              -> Operation AllSlice
readAllEvents Settings{..} c_pos p_pos max_c tos dir cred = construct $ do
    let msg = newRequest c_pos p_pos max_c tos s_requireMaster
        cmd = case dir of
            Forward  -> readAllEventsForwardCmd
            Backward -> readAllEventsBackwardCmd

        resp_cmd = case dir of
            Forward  -> readAllEventsForwardCompletedCmd
            Backward -> readAllEventsBackwardCompletedCmd
    resp <- send cmd resp_cmd cred msg
    let r      = getField $ _Result resp
        err    = getField $ _Error resp
        nc_pos = getField $ _NextCommitPosition resp
        np_pos = getField $ _NextPreparePosition resp
        es     = getField $ _Events resp
        evts   = fmap newResolvedEventFromBuf es
        eos    = null evts
        n_pos  = Position nc_pos np_pos
        slice  =
            if eos then SliceEndOfStream else Slice evts (Just n_pos)
    case fromMaybe SUCCESS r of
        ERROR         -> serverError err
        ACCESS_DENIED -> accessDenied All
        _             -> yield slice
