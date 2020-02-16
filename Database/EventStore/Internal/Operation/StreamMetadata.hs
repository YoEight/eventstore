{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.StreamMetadata
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.StreamMetadata
    ( readMetaStream
    , setMetaStream
    ) where

--------------------------------------------------------------------------------
import Data.Int

--------------------------------------------------------------------------------
import Data.Aeson (decode)

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Exec (Exec)
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Read.Common
import Database.EventStore.Internal.Operation.ReadEvent
import Database.EventStore.Internal.Operation.Write.Common
import Database.EventStore.Internal.Operation.WriteEvents
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Settings
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
metaStream :: Text -> Text
metaStream s = "$$" <> s

--------------------------------------------------------------------------------
-- | Read stream metadata operation.
readMetaStream
  :: Settings
  -> Exec
  -> Text
  -> Maybe Credentials
  -> IO (Async StreamMetadataResult)
readMetaStream setts exec s cred
  = async $
      do as <- readEvent setts exec (metaStream s) (-1) False cred
         tmp <- wait as
         onReadResult tmp $ \n evtNum evt ->
           do let bytes = recordedEventData $ resolvedEventOriginal evt
              case decode $ fromStrict bytes of
                Just pv -> pure $ StreamMetadataResult n evtNum pv
                Nothing -> throw invalidFormat

--------------------------------------------------------------------------------
-- | Set stream metadata operation.
setMetaStream
  :: Settings
  -> Exec
  -> Text
  -> ExpectedVersion
  -> Maybe Credentials
  -> StreamMetadata
  -> IO (Async WriteResult)
setMetaStream setts exec s v cred meta
  = let stream = metaStream s
        json   = streamMetadataJSON meta
        evt    = createEvent StreamMetadataType Nothing (withJson json) in
    writeEvents setts exec stream v cred [evt]

--------------------------------------------------------------------------------
invalidFormat :: OperationError
invalidFormat = InvalidOperation "Invalid metadata format"

--------------------------------------------------------------------------------
streamNotFound :: OperationError
streamNotFound = InvalidOperation "Read metadata on an inexistant stream"

--------------------------------------------------------------------------------
onReadResult :: ReadResult EventNumber ReadEvent
             -> (Text -> Int64 -> ResolvedEvent -> IO a)
             -> IO a
onReadResult (ReadSuccess r) k =
    case r of
      ReadEvent s n e -> k s n e
      _ -> throw streamNotFound

onReadResult ReadNoStream _          = throw streamNotFound
onReadResult (ReadStreamDeleted s) _ = throw $ StreamDeleted s
onReadResult ReadNotModified _       = throw $ ServerError Nothing
onReadResult (ReadError e) _         = throw $ ServerError e
onReadResult (ReadAccessDenied s) _  = throw $ AccessDenied s
