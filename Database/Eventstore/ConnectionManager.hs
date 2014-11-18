--------------------------------------------------------------------------------
-- |
-- Module : Database.Eventstore.ConnectionManager
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.Eventstore.ConnectionManager
    ( ConnectionManager
    , Event
    , EventData
    , ExpectedVersion(..)
      -- * Event
    , createEvent
    , withJson
    , withJsonAndMetadata
      -- * Connection manager
    , defaultSettings
    , eventStoreConnect
    , eventStoreSendEvent
    , eventStoreSendEvents
    , eventStoreShutdown
    ) where

--------------------------------------------------------------------------------
import Control.Concurrent
import Control.Concurrent.STM
import Data.ByteString (ByteString, empty)
import Data.ByteString.Lazy (toStrict)
import Data.Int
import Data.Traversable (traverse)
import System.IO

--------------------------------------------------------------------------------
import Control.Concurrent.Async
import Data.Aeson
import Data.Text

--------------------------------------------------------------------------------
import Database.Eventstore.Internal.Packages
import Database.Eventstore.Internal.Processor
import Database.Eventstore.Internal.Types

--------------------------------------------------------------------------------
type HostName = String
type Port     = Int

--------------------------------------------------------------------------------
-- Event
--------------------------------------------------------------------------------
data Event
    = Event
      { eventType :: !Text
      , eventData :: !EventData
      }

--------------------------------------------------------------------------------
createEvent :: Text -> EventData -> Event
createEvent = Event

--------------------------------------------------------------------------------
data EventData
    = Json Value (Maybe Value)

--------------------------------------------------------------------------------
eventDataType :: EventData -> Int32
eventDataType (Json _ _) = 1

--------------------------------------------------------------------------------
eventMetadataType :: EventData -> Int32
eventMetadataType _ = 0

--------------------------------------------------------------------------------
withJson :: Value -> EventData
withJson value = Json value Nothing

--------------------------------------------------------------------------------
withJsonAndMetadata :: Value -> Value -> EventData
withJsonAndMetadata value metadata = Json value (Just metadata)

--------------------------------------------------------------------------------
eventDataBytes :: EventData -> ByteString
eventDataBytes (Json value _) = toStrict $ encode value

--------------------------------------------------------------------------------
eventMetadataBytes :: EventData -> Maybe ByteString
eventMetadataBytes (Json _ meta_m) = fmap (toStrict . encode) meta_m

--------------------------------------------------------------------------------
-- Expected Version
--------------------------------------------------------------------------------
data ExpectedVersion
    = Any         -- ^ Says that you should not conflict with anything
    | NoStream    -- ^ Stream should not exist when doing your write
    | EmptyStream -- ^ Stream should exist but be empty when doing the write

--------------------------------------------------------------------------------
expVersionInt32 :: ExpectedVersion -> Int32
expVersionInt32 Any         = -2
expVersionInt32 NoStream    = -1
expVersionInt32 EmptyStream = 0

--------------------------------------------------------------------------------
-- ConnectionManager
--------------------------------------------------------------------------------
data ConnectionManager
    = ConnectionManager
      { mgrChan     :: TChan Msg
      , mgrSettings :: Settings
      , mgrThreadId :: ThreadId
      }

--------------------------------------------------------------------------------
msgQueue :: ConnectionManager -> Msg -> IO ()
msgQueue mgr msg = atomically $ writeTChan chan msg
  where
    chan = mgrChan mgr

--------------------------------------------------------------------------------
eventStoreConnect :: Settings -> HostName -> Port -> IO ConnectionManager
eventStoreConnect settings host port = do
    chan <- newTChanIO
    app  <- newProcessor settings chan host port
    tid  <- forkFinally (appProcess app) (\_ -> appFinalizer app)

    return $ ConnectionManager chan settings tid

--------------------------------------------------------------------------------
eventStoreShutdown :: ConnectionManager -> IO ()
eventStoreShutdown mgr = killThread tid
  where
    tid = mgrThreadId mgr

--------------------------------------------------------------------------------
eventStoreSendEvent :: ConnectionManager
                    -> Text             -- ^ Stream
                    -> ExpectedVersion
                    -> Event
                    -> IO (Async WriteResult)
eventStoreSendEvent mgr evt_stream exp_ver evt =
    eventStoreSendEvents mgr evt_stream exp_ver [evt]

--------------------------------------------------------------------------------
eventStoreSendEvents :: ConnectionManager
                     -> Text             -- ^ Stream
                     -> ExpectedVersion
                     -> [Event]
                     -> IO (Async WriteResult)
eventStoreSendEvents mgr evt_stream exp_ver evts = do
    new_evts <- traverse eventToNewEvent evts
    let write_evt = newWriteEvents evt_stream
                                   exp_ver_int32
                                   new_evts
                                   require_master

    pack <- writeEventsPackage None write_evt
    mvar <- atomically newEmptyTMVar
    as   <- async $ atomically $ readTMVar mvar
    msgQueue mgr (SendPackage (Just $ sendEventsOperation mvar) pack)

    return as
  where
    require_master = _requireMaster $ mgrSettings mgr
    exp_ver_int32  = expVersionInt32 exp_ver

--------------------------------------------------------------------------------
eventToNewEvent :: Event -> IO NewEvent
eventToNewEvent evt =
    newEvent evt_type
             evt_data_type
             evt_metadata_type
             evt_data_bytes
             evt_metadata_bytes
  where
    evt_type           = eventType evt
    evt_data_bytes     = eventDataBytes $ eventData evt
    evt_data_type      = eventDataType $ eventData evt
    evt_metadata_bytes = eventMetadataBytes $ eventData evt
    evt_metadata_type  = eventMetadataType $ eventData evt

--------------------------------------------------------------------------------
sendEventsOperation :: TMVar WriteResult -> Operation
sendEventsOperation mvar res =
    case res of
        WriteResultR we -> atomically $ putTMVar mvar we
        _               -> return ()
