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
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Read.Common
import Database.EventStore.Internal.Operation.ReadEvent
import Database.EventStore.Internal.Operation.Write.Common
import Database.EventStore.Internal.Operation.WriteEvents
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Settings
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
metaStream :: Text -> Text
metaStream s = "$$" <> s

--------------------------------------------------------------------------------
-- | Read stream metadata operation.
readMetaStream :: Settings
               -> Text
               -> Maybe Credentials
               -> Operation StreamMetadataResult
readMetaStream setts s cred = construct $ do
    let op = readEvent setts (metaStream s) (-1) False cred
    tmp <- deconstruct (fmap Left op)
    onReadResult tmp $ \n e_num evt -> do
        let bytes = recordedEventData $ resolvedEventOriginal evt
        case decode $ fromStrict bytes of
            Just pv -> yield $ StreamMetadataResult n e_num pv
            Nothing -> failure invalidFormat

--------------------------------------------------------------------------------
-- | Set stream metadata operation.
setMetaStream :: Settings
              -> Text
              -> ExpectedVersion
              -> Maybe Credentials
              -> StreamMetadata
              -> Operation WriteResult
setMetaStream setts s v cred meta =
    let stream = metaStream s
        json   = streamMetadataJSON meta
        evt    = createEvent StreamMetadataType Nothing (withJson json) in
     writeEvents setts stream v cred [evt]

--------------------------------------------------------------------------------
invalidFormat :: OperationError
invalidFormat = InvalidOperation "Invalid metadata format"

--------------------------------------------------------------------------------
streamNotFound :: OperationError
streamNotFound = InvalidOperation "Read metadata on an inexistant stream"

--------------------------------------------------------------------------------
onReadResult :: ReadResult EventNumber ReadEvent
             -> (Text -> Int64 -> ResolvedEvent -> Code o a)
             -> Code o a
onReadResult (ReadSuccess r) k =
    case r of
      ReadEvent s n e -> k s n e
      _               -> failure streamNotFound
onReadResult ReadNoStream _          = failure streamNotFound
onReadResult (ReadStreamDeleted s) _ = failure $ StreamDeleted s
onReadResult ReadNotModified _       = failure $ ServerError Nothing
onReadResult (ReadError e) _         = failure $ ServerError e
onReadResult (ReadAccessDenied s) _  = failure $ AccessDenied s
