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
      , readEventNumber :: !Int32
      }
    | ReadEvent
      { readEventStream   :: !Text
      , readEventNumber   :: !Int32
      , readEventResolved :: !ResolvedEvent
      } deriving Show

--------------------------------------------------------------------------------
-- | Read a specific event given event number operation.
readEvent :: Settings
          -> Text
          -> Int32
          -> Bool
          -> Operation (ReadResult 'RegularStream ReadEvent)
readEvent Settings{..} s evtn tos = construct $ do
    let msg = newRequest s evtn tos s_requireMaster
    resp <- send readEventCmd readEventCompletedCmd msg
    let r         = getField $ _result resp
        evt       = newResolvedEvent $ getField $ _indexedEvent resp
        err       = getField $ _error resp
        not_found = ReadSuccess $ ReadEventNotFound s evtn
        found     = ReadSuccess $ ReadEvent s evtn evt
    case r of
        NOT_FOUND      -> yield not_found
        NO_STREAM      -> yield ReadNoStream
        STREAM_DELETED -> yield $ ReadStreamDeleted s
        ERROR          -> yield (ReadError err)
        ACCESS_DENIED  -> yield $ ReadAccessDenied $ StreamName s
        SUCCESS        -> yield found
