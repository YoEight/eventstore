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
import Database.EventStore.Internal.Control (publishWith)
import Database.EventStore.Internal.Communication (Transmit(..))
import Database.EventStore.Internal.Exec (Exec)
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Read.Common
import Database.EventStore.Internal.Operation.ReadStreamEvents.Message
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Settings
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Batch read from a regular stream operation.
readStreamEvents
  :: Settings
  -> Exec
  -> ReadDirection
  -> Text
  -> Int64
  -> Int32
  -> Bool
  -> Maybe Credentials
  -> IO (Async (ReadResult EventNumber StreamSlice))
readStreamEvents Settings{..} exec dir stream st cnt tos cred
  = do m <- mailboxNew
       async $
         do let reqCmd =
                  case dir of
                    Forward  -> readStreamEventsForwardCmd
                    Backward -> readStreamEventsBackwardCmd

                req = newRequest stream st cnt tos s_requireMaster
            pkg <- createPkg reqCmd cred req
            publishWith exec (Transmit m OneTime pkg)
            outcome <- mailboxReadDecoded m
            case outcome of
              Left e
                -> throw e
              Right resp
                -> let r     = getField $ _result resp
                       es    = getField $ _events resp
                       evts  = fmap newResolvedEvent es
                       err   = getField $ _error resp
                       eos   = getField $ _endOfStream resp
                       nxt   = getField $ _nextNumber resp
                       found =
                           if null evts && eos
                           then SliceEndOfStream
                           else Slice evts (if eos then Nothing else Just $ EventNumber nxt) in
                   case r of
                     NO_STREAM -> pure ReadNoStream
                     STREAM_DELETED -> pure $ ReadStreamDeleted $ StreamName stream
                     NOT_MODIFIED -> pure ReadNotModified
                     ERROR -> pure (ReadError err)
                     ACCESS_DENIED -> pure $ ReadAccessDenied $ StreamName stream
                     SUCCESS -> pure (ReadSuccess found)
