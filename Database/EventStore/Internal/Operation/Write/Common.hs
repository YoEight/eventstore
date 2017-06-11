--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.Write.Common
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.Write.Common where

--------------------------------------------------------------------------------
import Data.Int

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Returned after writing to a stream.
data WriteResult
    = WriteResult
      { writeNextExpectedVersion :: !Int32
        -- ^ Next expected version of the stream.
      , writePosition :: !Position
        -- ^ 'Position' of the write.
      }
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Constructs a 'NewEvent' from an 'Event'.
eventToNewEvent :: Event -> Code o NewEvent
eventToNewEvent evt = do
    uuid <- maybe freshId return evt_id
    return $ newEvent evt_type
                      uuid
                      evt_data_type
                      evt_metadata_type
                      evt_data_bytes
                      evt_metadata_bytes
  where
    evt_type           = eventTypeText $ eventType evt
    evt_id             = eventId evt
    evt_data_bytes     = eventDataBytes $ eventData evt
    evt_data_type      = eventDataType $ eventData evt
    evt_metadata_bytes = eventMetadataBytes $ eventData evt
    evt_metadata_type  = eventMetadataType $ eventData evt
