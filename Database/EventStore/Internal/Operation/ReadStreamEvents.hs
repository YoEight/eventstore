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
import ClassyPrelude
import Data.ProtocolBuffers

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Read.Common
import Database.EventStore.Internal.Operation.ReadStreamEvents.Message
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Batch read from a regular stream operation.
readStreamEvents :: Settings
                 -> ReadDirection
                 -> Text
                 -> Int32
                 -> Int32
                 -> Bool
                 -> Operation (ReadResult 'RegularStream StreamSlice)
readStreamEvents Settings{..} dir s st cnt tos = do
    let req_cmd =
            case dir of
                Forward  -> 0xB2
                Backward -> 0xB4
        resp_cmd =
            case dir of
                Forward  -> 0xB3
                Backward -> 0xB5

        msg = newRequest s st cnt tos s_requireMaster
    resp <- send req_cmd resp_cmd msg
    let r     = getField $ _result resp
        es    = getField $ _events resp
        evts  = fmap newResolvedEvent es
        err   = getField $ _error resp
        eos   = getField $ _endOfStream resp
        nxt   = getField $ _nextNumber resp
        lst   = getField $ _lastNumber resp
        found = StreamSlice s lst dir st nxt evts eos
    case r of
        NO_STREAM      -> yield ReadNoStream
        STREAM_DELETED -> yield $ ReadStreamDeleted s
        NOT_MODIFIED   -> yield ReadNotModified
        ERROR          -> yield (ReadError err)
        ACCESS_DENIED  -> yield $ ReadAccessDenied $ StreamName s
        SUCCESS        -> yield (ReadSuccess found)
