{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.ReadStreamEvents
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.ReadStreamEvents
    ( readStreamEvents ) where

--------------------------------------------------------------------------------
import Data.Int

--------------------------------------------------------------------------------
import Data.ProtocolBuffers

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Read.Common
import Database.EventStore.Internal.Operation.ReadStreamEvents.Message
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Settings
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Batch read from a regular stream operation.
readStreamEvents :: Settings
                 -> ReadDirection
                 -> Text
                 -> Int64
                 -> Int32
                 -> Bool
                 -> Maybe Credentials
                 -> Operation (ReadResult EventNumber StreamSlice)
readStreamEvents Settings{..} dir s st cnt tos cred = construct $ do
    let req_cmd =
            case dir of
                Forward  -> readStreamEventsForwardCmd
                Backward -> readStreamEventsBackwardCmd
        resp_cmd =
            case dir of
                Forward  -> readStreamEventsForwardCompletedCmd
                Backward -> readStreamEventsBackwardCompletedCmd

        msg = newRequest s st cnt tos s_requireMaster
    resp <- send req_cmd resp_cmd cred msg
    let r     = getField $ _result resp
        es    = getField $ _events resp
        evts  = fmap newResolvedEvent es
        err   = getField $ _error resp
        eos   = getField $ _endOfStream resp
        nxt   = getField $ _nextNumber resp
        found =
            if null evts && eos
            then SliceEndOfStream
            else Slice evts (if eos then Nothing else Just $ EventNumber nxt)
    case r of
        NO_STREAM      -> yield ReadNoStream
        STREAM_DELETED -> yield $ ReadStreamDeleted $ StreamName s
        NOT_MODIFIED   -> yield ReadNotModified
        ERROR          -> yield (ReadError err)
        ACCESS_DENIED  -> yield $ ReadAccessDenied $ StreamName s
        SUCCESS        -> yield (ReadSuccess found)
