{-# LANGUAGE GADTs, ExistentialQuantification #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Engine
-- Copyright : (C) 2016 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Engine where

--------------------------------------------------------------------------------
import ClassyPrelude
import Control.Monad.State
import Data.ProtocolBuffers
import Data.Serialize
import Data.UUID
import Data.UUID.V4

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Feature
import Database.EventStore.Internal.Operation hiding (SM(..))
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
import qualified Database.EventStore.Internal.Operation.WriteEvents.Message as Write
import qualified Database.EventStore.Internal.Operation.ReadEvent.Message as ReadEvent

--------------------------------------------------------------------------------
data Status = Status

--------------------------------------------------------------------------------
data Request = Request Package (ByteString -> StateT EngineState IO ())

--------------------------------------------------------------------------------
data EngineState =
    EngineState

--------------------------------------------------------------------------------
execute :: (MonadState EngineState m, MonadIO m) => Feature -> m Status
execute _ = undefined

--------------------------------------------------------------------------------
freshUUID :: MonadIO m => m UUID
freshUUID = liftIO nextRandom

--------------------------------------------------------------------------------
package :: Encode e => Settings -> UUID -> Command -> e -> Package
package setts uuid cmd e =
    Package
    { packageCmd         = cmd
    , packageCorrelation = uuid
    , packageData        = runPut $ encodeMessage e
    , packageCred        = s_credentials setts
    }

--------------------------------------------------------------------------------
request :: MonadIO m => Settings -> Feature -> m Request
request setts feature@(Operation op) = do
    uuid <- freshUUID
    pkg <-
        case op of
            SendEvents stream ver evts _ -> do
                xs <- traverse eventToNewEvent evts
                let ver32 = expVersionInt32 ver
                    msg   = Write.newRequest stream ver32 xs req_master
                return $ package setts uuid 0x82 msg
            ReadEvent stream eid tos _ ->
                let msg = ReadEvent.newRequest stream eid tos req_master in
                return $ package setts uuid 0xB0 msg

    return $ Request pkg (response feature)
  where
    req_master = s_requireMaster setts

--------------------------------------------------------------------------------
response :: Feature -> ByteString -> StateT EngineState IO ()
response = undefined

--------------------------------------------------------------------------------
-- | Constructs a 'NewEvent' from an 'Event'.
eventToNewEvent :: MonadIO m => Event -> m NewEvent
eventToNewEvent evt = do
    uuid <- maybe freshUUID return evt_id
    return $ newEvent evt_type
                      uuid
                      evt_data_type
                      evt_metadata_type
                      evt_data_bytes
                      evt_metadata_bytes
  where
    evt_type           = eventType evt
    evt_id             = eventId evt
    evt_data_bytes     = eventDataBytes $ eventData evt
    evt_data_type      = eventDataType $ eventData evt
    evt_metadata_bytes = eventMetadataBytes $ eventData evt
    evt_metadata_type  = eventMetadataType $ eventData evt