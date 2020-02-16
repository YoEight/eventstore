{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.DeleteStream
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.ReadEvent
    ( ReadEvent(..)
    , readEvent
    ) where

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
import Database.EventStore.Internal.Operation.ReadEvent.Message
import Database.EventStore.Internal.Operation.Read.Common
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Settings
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Represents the result of looking up a specific event number from a stream.
data ReadEvent
    = ReadEventNotFound
      { readEventStream :: !Text
      , readEventNumber :: !Int64
      }
    | ReadEvent
      { readEventStream   :: !Text
      , readEventNumber   :: !Int64
      , readEventResolved :: !ResolvedEvent
      } deriving Show

--------------------------------------------------------------------------------
-- | Read a specific event given event number operation.
readEvent
  :: Settings
  -> Exec
  -> Text
  -> Int64
  -> Bool
  -> Maybe Credentials
  -> IO (Async (ReadResult EventNumber ReadEvent))
readEvent Settings{..} exec stream evtn tos creds
  = do m <- mailboxNew
       async $
         do let req = newRequest stream evtn tos s_requireMaster
            pkg <- createPkg readEventCmd creds req
            publishWith exec (Transmit m OneTime pkg)
            outcome <- mailboxReadDecoded m
            case outcome of
              Left e
                -> throw e
              Right resp
                -> let r = getField $ _result resp
                       evt = newResolvedEvent $ getField $ _indexedEvent resp
                       err = getField $ _error resp
                       notFound = ReadSuccess $ ReadEventNotFound stream evtn
                       found = ReadSuccess $ ReadEvent stream evtn evt in
                   case r of
                     NOT_FOUND      -> pure notFound
                     NO_STREAM      -> pure ReadNoStream
                     STREAM_DELETED -> pure $ ReadStreamDeleted $ StreamName stream
                     ERROR          -> pure (ReadError err)
                     ACCESS_DENIED  -> pure $ ReadAccessDenied $ StreamName stream
                     SUCCESS        -> pure found


