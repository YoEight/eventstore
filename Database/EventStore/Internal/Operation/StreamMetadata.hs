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

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Text (Text)
import Data.UUID (UUID)

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Read.Common
import Database.EventStore.Internal.Operation.ReadEvent
import Database.EventStore.Internal.Operation.Write.Common
import Database.EventStore.Internal.Operation.WriteEvents
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
readMetaStream :: Settings -> Text -> Operation 'Init StreamMetadataResult
readMetaStream setts s =
    Operation (create $ readEvent setts meta_s (-1) False)
  where
    meta_s = "$$" <> s

    create :: forall a. Operation 'Init (ReadResult 'RegularStream ReadEvent)
           -> Input 'Init StreamMetadataResult a
           -> a
    create op (Create uuid) =
        let (pkg, nxt_op) = createPackage uuid op in
        (pkg, Operation $ pending nxt_op)

    pending :: forall a. Operation 'Pending (ReadResult 'RegularStream ReadEvent)
            -> Input 'Pending StreamMetadataResult a
            -> a
    pending op (Arrived pkg) =
        case packageArrived pkg op of
            Left nxt_op  -> Left $ Operation $ pending nxt_op
            Right com_op -> Right $
              case getReport com_op of
                  Retry n_op  -> retry $ create n_op
                  Error e     -> errored e
                  Success tmp -> onReadResult tmp $ \n e_num evt ->
                      let action = do
                            orig <- resolvedEventOriginal evt
                            decode $ fromStrict $ recordedEventData orig in
                      case action of
                          Just pv -> success $ StreamMetadataResult n e_num pv
                          Nothing -> errored invalidFormat

--------------------------------------------------------------------------------
setMetaStream :: Settings
              -> Text
              -> ExpectedVersion
              -> UUID
              -> StreamMetadata
              -> Operation 'Init WriteResult
setMetaStream setts s v evt_uuid meta =
    Operation $ create (writeEvents setts s v [createNewEvent meta evt_uuid])
  where
    create :: forall a. Operation 'Init WriteResult
           -> Input 'Init WriteResult a
           -> a
    create op (Create uuid) =
        let (pkg, nxt_op) = createPackage uuid op in
        (pkg, Operation $ pending nxt_op)

    pending :: forall a. Operation 'Pending WriteResult
            -> Input 'Pending WriteResult a
            -> a
    pending op (Arrived pkg) =
        case packageArrived pkg op of
            Left nxt_op  -> Left $ Operation $ pending nxt_op
            Right com_op -> Right $
                case getReport com_op of
                    Retry n_op -> retry $ create n_op
                    Error e    -> errored e
                    Success r  -> success r

--------------------------------------------------------------------------------
invalidFormat :: OperationError
invalidFormat = InvalidOperation "Invalid metadata format"

--------------------------------------------------------------------------------
streamNotFound :: OperationError
streamNotFound = InvalidOperation "Read metadata on an inexistant stream"

--------------------------------------------------------------------------------
onReadResult :: ReadResult 'RegularStream ReadEvent
             -> (Text -> Int32 -> ResolvedEvent -> Operation 'Completed b)
             -> Operation 'Completed b
onReadResult (ReadSuccess r) k =
    case r of
      ReadEvent s n e -> k s n e
      _               -> errored streamNotFound
onReadResult ReadNoStream _          = errored streamNotFound
onReadResult (ReadStreamDeleted s) _ = errored $ StreamDeleted s
onReadResult ReadNotModified _       = errored $ ServerError Nothing
onReadResult (ReadError e) _         = errored $ ServerError e
onReadResult (ReadAccessDenied s) _  = errored $ AccessDenied s

--------------------------------------------------------------------------------
createNewEvent :: StreamMetadata -> UUID -> NewEvent
createNewEvent meta uuid =
    newEvent "$metadata" uuid 1 1 bytes Nothing
  where
    bytes = toStrict $ encode $ streamMetadataJSON meta
