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
import Data.Monoid ((<>))

--------------------------------------------------------------------------------
import Data.Aeson (decode)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Read.Common
import Database.EventStore.Internal.Operation.ReadEvent
import Database.EventStore.Internal.Operation.Write.Common
import Database.EventStore.Internal.Operation.WriteEvents
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
metaStream :: Text -> Text
metaStream s = "$$" <> s

--------------------------------------------------------------------------------
readMetaStream :: Settings -> Text -> Operation StreamMetadataResult
readMetaStream setts s =
    foreach (readEvent setts (metaStream s) (-1) False) $ \tmp -> do
        onReadResult tmp $ \n e_num evt -> do
            let bytes = recordedEventData $ resolvedEventOriginal evt
            case decode $ fromStrict bytes of
                Just pv -> yield $ StreamMetadataResult n e_num pv
                Nothing -> failure invalidFormat

--------------------------------------------------------------------------------
setMetaStream :: Settings
              -> Text
              -> ExpectedVersion
              -> StreamMetadata
              -> Operation WriteResult
setMetaStream setts s v meta =
    let stream = metaStream s
        json   = streamMetadataJSON meta
        evt    = createEvent "$metadata" Nothing (withJson json)
        inner  = writeEvents setts stream v [evt] in
    foreach inner yield

--------------------------------------------------------------------------------
invalidFormat :: OperationError
invalidFormat = InvalidOperation "Invalid metadata format"

--------------------------------------------------------------------------------
streamNotFound :: OperationError
streamNotFound = InvalidOperation "Read metadata on an inexistant stream"

--------------------------------------------------------------------------------
onReadResult :: ReadResult 'RegularStream ReadEvent
             -> (Text -> Int32 -> ResolvedEvent -> SM a b)
             -> SM a b
onReadResult (ReadSuccess r) k =
    case r of
      ReadEvent s n e -> k s n e
      _               -> failure streamNotFound
onReadResult ReadNoStream _          = failure streamNotFound
onReadResult (ReadStreamDeleted s) _ = failure $ StreamDeleted s
onReadResult ReadNotModified _       = failure $ ServerError Nothing
onReadResult (ReadError e) _         = failure $ ServerError e
onReadResult (ReadAccessDenied s) _  = failure $ AccessDenied s
