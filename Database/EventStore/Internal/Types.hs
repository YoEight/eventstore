{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Types
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Types where

--------------------------------------------------------------------------------
import Data.Maybe
import Data.Monoid (Endo(..))
import Foreign.C.Types (CTime(..))

--------------------------------------------------------------------------------
import           ClassyPrelude hiding (Builder)
import qualified Data.Aeson as A
import           Data.Aeson.Types (Object, ToJSON(..), Pair, Parser, (.=))
import           Data.DotNet.TimeSpan
import           Data.HashMap.Strict (filterWithKey)
import           Data.ProtocolBuffers
import           Data.Time (NominalDiffTime)
import           Data.Time.Clock.POSIX
import           Data.UUID (UUID, fromByteString, toByteString)
import           Network.Connection (TLSSettings)

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Logging

--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------
-- | Represent a class of error where the user is not at fault. It could be
--   either the client or the server.
data InternalException
    = ConnectionClosedByServer
      -- ^ Happens when the server deliberately close the connection. This
      --   probably happens if the client didn't respect EventStore
      --   communication error. For instance, the client takes too much time to
      --   respond to a heartbeat request.
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception InternalException

--------------------------------------------------------------------------------
-- Event
--------------------------------------------------------------------------------
-- | Contains event information like its type and data. Only used for write
--   queries.
data Event
    = Event
      { eventType :: !Text
      , eventId   :: !(Maybe UUID)
      , eventData :: !EventData
      } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Create an 'Event' meant to be persisted.
createEvent :: Text       -- ^ Event type
            -> Maybe UUID -- ^ Event ID, generated if 'Nothing'
            -> EventData  -- ^ Event data
            -> Event
createEvent = Event

--------------------------------------------------------------------------------
-- | Holds event data.
data EventData
    = Json A.Value (Maybe A.Value)
    | Binary ByteString (Maybe ByteString)
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Maps 'Event' inner data type to an 'Int32' understandable by the server.
eventDataType :: EventData -> Int32
eventDataType (Json _ _) = 1
eventDataType _          = 0

--------------------------------------------------------------------------------
-- | Maps 'Event' inner metadata type to an 'Int32' understandable by the
--   server.
eventMetadataType :: EventData -> Int32
eventMetadataType _ = 0

--------------------------------------------------------------------------------
-- | Creates an event using JSON format
withJson :: ToJSON a => a -> EventData
withJson value = Json (toJSON value) Nothing

--------------------------------------------------------------------------------
-- | Creates an event using a binary format.
withBinary :: ByteString -> EventData
withBinary bs = Binary bs Nothing

--------------------------------------------------------------------------------
-- | Creates an event with metadata using JSON format.
withJsonAndMetadata :: (ToJSON a, ToJSON b) => a -> b -> EventData
withJsonAndMetadata value metadata =
    Json (toJSON value) (Just $ toJSON metadata)

--------------------------------------------------------------------------------
-- | Creates an event with metadata using binary format.
withBinaryAndMetadata :: ByteString -> ByteString -> EventData
withBinaryAndMetadata value metadata = Binary value (Just metadata)

--------------------------------------------------------------------------------
-- | Serializes 'EventData''s data to a raw 'ByteString'.
eventDataBytes :: EventData -> ByteString
eventDataBytes (Json value _)   = toStrict $ A.encode value
eventDataBytes (Binary value _) = value

--------------------------------------------------------------------------------
-- | Serializes 'EventData' metadata to a raw 'ByteString'.
eventMetadataBytes :: EventData -> Maybe ByteString
eventMetadataBytes (Json _ meta_m)   = fmap (toStrict . A.encode) meta_m
eventMetadataBytes (Binary _ meta_m) = meta_m

--------------------------------------------------------------------------------
-- Expected Version
--------------------------------------------------------------------------------
-- | Constants used for expected version control.
--
--   The use of expected version can be a bit tricky especially when discussing
--   idempotency assurances given by the EventStore.
--
--   The EventStore  will assure idempotency for all operations using any value
--   in 'ExpectedVersion' except for 'anyStream'. When using 'anyStream' the
--   EventStore will do its best to assure idempotency but will not guarantee
--   idempotency.
data ExpectedVersion
    = Any
    | NoStream
    | EmptyStream
    | Exact Int32
    | StreamExists
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Maps a 'ExpectedVersion' to an 'Int32' understandable by the server.
expVersionInt32 :: ExpectedVersion -> Int32
expVersionInt32 Any         = -2
expVersionInt32 NoStream    = -1
expVersionInt32 EmptyStream = -1
expVersionInt32 (Exact i)   = i
expVersionInt32 StreamExists = -4

--------------------------------------------------------------------------------
-- | This write should not conflict with anything and should always succeed.
anyVersion :: ExpectedVersion
anyVersion = Any

--------------------------------------------------------------------------------
-- | The stream being written to should not yet exist. If it does exist
--   treat that as a concurrency problem.
noStreamVersion :: ExpectedVersion
noStreamVersion = NoStream

--------------------------------------------------------------------------------
-- | The stream should exist and should be empty. If it does not exist or
--   is not empty, treat that as a concurrency problem.
emptyStreamVersion :: ExpectedVersion
emptyStreamVersion = EmptyStream

--------------------------------------------------------------------------------
-- | States that the last event written to the stream should have a
--   sequence number matching your expected value.
exactEventVersion :: Int32 -> ExpectedVersion
exactEventVersion i
    | i < 0     = error $ "expected version must be >= 0, but is " ++ show i
    | otherwise = Exact i

--------------------------------------------------------------------------------
-- | The stream should exist. If it or a metadata stream does not exist treat
--   that as a concurrency problem.
streamExists :: ExpectedVersion
streamExists = StreamExists

--------------------------------------------------------------------------------
-- EventStore Messages
--------------------------------------------------------------------------------
-- | Serializes form of an 'Event'.
data NewEvent
    = NewEvent
      { newEventId           :: Required 1 (Value ByteString)
      , newEventType         :: Required 2 (Value Text)
      , newEventDataType     :: Required 3 (Value Int32)
      , newEventMetadataType :: Required 4 (Value Int32)
      , newEventData         :: Required 5 (Value ByteString)
      , newEventMetadata     :: Optional 6 (Value ByteString)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode NewEvent

--------------------------------------------------------------------------------
-- | 'NewEvent' smart constructor.
newEvent :: Text             -- ^ Event type
         -> UUID             -- ^ Event ID
         -> Int32            -- ^ Data content type
         -> Int32            -- ^ Metadata content type
         -> ByteString       -- ^ Event data
         -> Maybe ByteString -- ^ Metadata
         -> NewEvent
newEvent evt_type evt_id data_type meta_type evt_data evt_meta =
    let uuid_bytes = toStrict $ toByteString evt_id
        new_evt    = NewEvent
                     { newEventId           = putField uuid_bytes
                     , newEventType         = putField evt_type
                     , newEventDataType     = putField data_type
                     , newEventMetadataType = putField meta_type
                     , newEventData         = putField evt_data
                     , newEventMetadata     = putField evt_meta
                     } in
    new_evt

--------------------------------------------------------------------------------
-- | Represents a serialized event coming from the server.
data EventRecord
    = EventRecord
      { eventRecordStreamId     :: Required 1  (Value Text)
      , eventRecordNumber       :: Required 2  (Value Int32)
      , eventRecordId           :: Required 3  (Value ByteString)
      , eventRecordType         :: Required 4  (Value Text)
      , eventRecordDataType     :: Required 5  (Value Int32)
      , eventRecordMetadataType :: Required 6  (Value Int32)
      , eventRecordData         :: Required 7  (Value ByteString)
      , eventRecordMetadata     :: Optional 8  (Value ByteString)
      , eventRecordCreated      :: Optional 9  (Value Int64)
      , eventRecordCreatedEpoch :: Optional 10 (Value Int64)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode EventRecord

--------------------------------------------------------------------------------
-- | Represents a serialized event representiong either an event or a link
--   event.
data ResolvedIndexedEvent
    = ResolvedIndexedEvent
      { resolvedIndexedRecord :: Optional 1 (Message EventRecord)
      , resolvedIndexedLink   :: Optional 2 (Message EventRecord)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode ResolvedIndexedEvent

--------------------------------------------------------------------------------
-- | Represents a serialized event sent by the server in a subscription context.
data ResolvedEventBuf
    = ResolvedEventBuf
      { resolvedEventBufEvent           :: Required 1 (Message EventRecord)
      , resolvedEventBufLink            :: Optional 2 (Message EventRecord)
      , resolvedEventBufCommitPosition  :: Required 3 (Value Int64)
      , resolvedEventBufPreparePosition :: Required 4 (Value Int64)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode ResolvedEventBuf

--------------------------------------------------------------------------------
-- Result
--------------------------------------------------------------------------------
-- | A structure referring to a potential logical record position in the
--   EventStore transaction file.
data Position
    = Position
      { positionCommit  :: !Int64 -- ^ Commit position of the record
      , positionPrepare :: !Int64 -- ^ Prepare position of the record
      }
    deriving Show

--------------------------------------------------------------------------------
instance Eq Position where
    Position ac aap == Position bc bp = ac == bc && aap == bp

--------------------------------------------------------------------------------
instance Ord Position where
    compare (Position ac aap) (Position bc bp) =
        if ac < bc || (ac == bc && aap < bp)
        then LT
        else if ac > bc || (ac == bc && aap > bp)
             then GT
             else EQ

--------------------------------------------------------------------------------
-- | Representing the start of the transaction file.
positionStart :: Position
positionStart = Position 0 0

--------------------------------------------------------------------------------
-- | Representing the end of the transaction file.
positionEnd :: Position
positionEnd = Position (-1) (-1)

--------------------------------------------------------------------------------
-- | Represents a previously written event.
data RecordedEvent
    = RecordedEvent
      { recordedEventStreamId :: !Text
        -- ^ The event stream that this event  belongs to.
      , recordedEventId :: !UUID
        -- ^ Unique identifier representing this event.
      , recordedEventNumber :: !Int32
        -- ^ Number of this event in the stream.
      , recordedEventType :: !Text
        -- ^ Type of this event.
      , recordedEventData :: !ByteString
        -- ^ Representing the data of this event.
      , recordedEventMetadata :: !(Maybe ByteString)
        -- ^ Representing the metadada associated with this event.
      , recordedEventIsJson :: !Bool
        -- ^ Indicates whether the content is internally marked as json.
      , recordedEventCreated :: !(Maybe UTCTime)
        -- ^ Representing when this event was created in the system.
      }
    deriving Show

--------------------------------------------------------------------------------
-- | Tries to parse JSON object from the given 'RecordedEvent'.
recordedEventDataAsJson :: A.FromJSON a => RecordedEvent -> Maybe a
recordedEventDataAsJson = A.decode . fromStrict . recordedEventData

--------------------------------------------------------------------------------
-- | Converts a raw 'Int64' into an 'UTCTime'
-- fromIntegral should be a no-op in GHC and allow eventstore to compile w GHCJS
-- GHCJS maps CTime to Int32 (cf PR https://github.com/YoEight/eventstore/pull/47)
toUTC :: Int64 -> UTCTime
toUTC = posixSecondsToUTCTime . (/1000) . realToFrac . CTime . fromIntegral

--------------------------------------------------------------------------------
-- | Constructs a 'RecordedEvent' from an 'EventRecord'.
newRecordedEvent :: EventRecord -> RecordedEvent
newRecordedEvent er = re
  where
    evt_id      = getField $ eventRecordId er
    evt_uuid    = fromJust $ fromByteString $ fromStrict evt_id
    data_type   = getField $ eventRecordDataType er
    epoch       = getField $ eventRecordCreatedEpoch er
    utc_created = fmap toUTC epoch

    re = RecordedEvent
         { recordedEventStreamId     = getField $ eventRecordStreamId er
         , recordedEventNumber       = getField $ eventRecordNumber er
         , recordedEventId           = evt_uuid
         , recordedEventType         = getField $ eventRecordType er
         , recordedEventData         = getField $ eventRecordData er
         , recordedEventMetadata     = getField $ eventRecordMetadata er
         , recordedEventIsJson       = data_type == 1
         , recordedEventCreated      = utc_created
         }

--------------------------------------------------------------------------------
-- | A structure representing a single event or an resolved link event.
data ResolvedEvent
    = ResolvedEvent
      { resolvedEventRecord :: !(Maybe RecordedEvent)
        -- ^ The event, or the resolved link event if this 'ResolvedEvent' is a
        --   link event.
      , resolvedEventLink :: !(Maybe RecordedEvent)
        -- ^ The link event if this 'ResolvedEvent' is a link event.
      , resolvedEventPosition :: !(Maybe Position)
        -- ^ Possible 'Position' of that event.
      }
    deriving Show

--------------------------------------------------------------------------------
-- | Constructs a 'ResolvedEvent' from a 'ResolvedIndexedEvent'.
newResolvedEvent :: ResolvedIndexedEvent -> ResolvedEvent
newResolvedEvent rie = re
  where
    record = getField $ resolvedIndexedRecord rie
    link   = getField $ resolvedIndexedLink rie
    re     = ResolvedEvent
             { resolvedEventRecord   = fmap newRecordedEvent record
             , resolvedEventLink     = fmap newRecordedEvent link
             , resolvedEventPosition = Nothing
             }

--------------------------------------------------------------------------------
-- | Constructs a 'ResolvedEvent' from a 'ResolvedEventBuf'.
newResolvedEventFromBuf :: ResolvedEventBuf -> ResolvedEvent
newResolvedEventFromBuf reb = re
  where
    record = Just $ newRecordedEvent $ getField $ resolvedEventBufEvent reb
    link   = getField $ resolvedEventBufLink reb
    com    = getField $ resolvedEventBufCommitPosition reb
    pre    = getField $ resolvedEventBufPreparePosition reb
    pos    = Position com pre
    re     = ResolvedEvent
             { resolvedEventRecord   = record
             , resolvedEventLink     = fmap newRecordedEvent link
             , resolvedEventPosition = Just pos
             }

--------------------------------------------------------------------------------
-- | Returns the event that was read or which triggered the subscription.
--
--   If this 'ResolvedEvent' represents a link event, the link will be the
--   original event, otherwise it will be the event.
resolvedEventOriginal :: ResolvedEvent -> RecordedEvent
resolvedEventOriginal (ResolvedEvent record link _) =
    let Just evt = link <|> record in evt

--------------------------------------------------------------------------------
-- | Tries to desarialize 'resolvedEventOriginal' data as JSON.
resolvedEventDataAsJson :: A.FromJSON a => ResolvedEvent -> Maybe a
resolvedEventDataAsJson = recordedEventDataAsJson . resolvedEventOriginal

--------------------------------------------------------------------------------
-- | Indicates whether this 'ResolvedEvent' is a resolved link event.
isEventResolvedLink :: ResolvedEvent -> Bool
isEventResolvedLink = isJust . resolvedEventLink

--------------------------------------------------------------------------------
-- | The stream name of the original event.
resolvedEventOriginalStreamId :: ResolvedEvent -> Text
resolvedEventOriginalStreamId = recordedEventStreamId . resolvedEventOriginal

--------------------------------------------------------------------------------
-- | The ID of the original event.
resolvedEventOriginalId :: ResolvedEvent -> UUID
resolvedEventOriginalId = recordedEventId . resolvedEventOriginal

--------------------------------------------------------------------------------
-- | Represents the direction of read operation (both from $all an usual
--   streams).
data ReadDirection
    = Forward  -- ^ From beginning to end
    | Backward -- ^ From end to beginning
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Flag
--------------------------------------------------------------------------------
-- | Indicates either a 'Package' contains 'Credentials' data or not.
data Flag
    = None
    | Authenticated
    deriving Show

--------------------------------------------------------------------------------
-- | Maps a 'Flag' into a 'Word8' understandable by the server.
flagWord8 :: Flag -> Word8
flagWord8 None          = 0x00
flagWord8 Authenticated = 0x01

--------------------------------------------------------------------------------
-- Credentials
--------------------------------------------------------------------------------
-- | Holds login and password information.
data Credentials
    = Credentials
      { credLogin    :: !ByteString
      , credPassword :: !ByteString
      }
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Creates a 'Credentials' given a login and a password.
credentials :: ByteString -- ^ Login
            -> ByteString -- ^ Password
            -> Credentials
credentials = Credentials

--------------------------------------------------------------------------------
-- Package
--------------------------------------------------------------------------------
-- | Represents a package exchanged between the client and the server.
data Package
    = Package
      { packageCmd         :: !Command
      , packageCorrelation :: !UUID
      , packageData        :: !ByteString
      , packageCred        :: !(Maybe Credentials)
      }
    deriving Show

--------------------------------------------------------------------------------
-- | Constructs a heartbeat response given the 'UUID' of heartbeat request.
heartbeatResponsePackage :: UUID -> Package
heartbeatResponsePackage uuid =
    Package
    { packageCmd         = 0x02
    , packageCorrelation = uuid
    , packageData        = ""
    , packageCred        = Nothing
    }

--------------------------------------------------------------------------------
-- Settings
--------------------------------------------------------------------------------
-- | Represents reconnection strategy.
data Retry
    = AtMost Int
    | KeepRetrying

--------------------------------------------------------------------------------
-- | Indicates how many times we should try to reconnect to the server. A value
--   less than or equal to 0 means no retry.
atMost :: Int -> Retry
atMost = AtMost

--------------------------------------------------------------------------------
-- | Indicates we should try to reconnect to the server until the end of the
--   Universe.
keepRetrying :: Retry
keepRetrying = KeepRetrying

--------------------------------------------------------------------------------
-- | Global 'Connection' settings
data Settings
    = Settings
      { s_heartbeatInterval    :: NominalDiffTime
      , s_heartbeatTimeout     :: NominalDiffTime
      , s_requireMaster        :: Bool
      , s_credentials          :: Maybe Credentials
      , s_retry                :: Retry
      , s_reconnect_delay_secs :: Int -- ^ In seconds
      , s_logger               :: Maybe (Log -> IO ())
      , s_ssl                  :: Maybe TLSSettings
      }

--------------------------------------------------------------------------------
-- | Default global settings.
--   s_heartbeatInterval    = 750 ms
--   s_heartbeatTimeout     = 1500 ms
--   s_requireMaster        = True
--   s_credentials          = Nothing
--   s_retry                = 'atMost' 3
--   s_reconnect_delay_secs = 3
--   s_logger               = Nothing
defaultSettings :: Settings
defaultSettings  = Settings
                   { s_heartbeatInterval    = msDiffTime 750  -- 750ms
                   , s_heartbeatTimeout     = msDiffTime 1500 -- 1500ms
                   , s_requireMaster        = True
                   , s_credentials          = Nothing
                   , s_retry                = atMost 3
                   , s_reconnect_delay_secs = 3
                   , s_logger               = Nothing
                   , s_ssl                  = Nothing
                   }

--------------------------------------------------------------------------------
-- | Default SSL settings based on 'defaultSettings'.
defaultSSLSettings :: TLSSettings -> Settings
defaultSSLSettings tls = defaultSettings { s_ssl = Just tls }

--------------------------------------------------------------------------------
-- | Triggers the logger callback if it has been set.
_settingsLog :: Settings -> Log -> IO ()
_settingsLog Settings{..} l =
    case s_logger of
        Just k -> k l
        _      -> return ()

--------------------------------------------------------------------------------
-- | Millisecond timespan
msDiffTime :: Float -> NominalDiffTime
msDiffTime i = fromRational $ toRational (i / 1000)

--------------------------------------------------------------------------------
-- | Represents an access control list for a stream.
data StreamACL
    = StreamACL
      { streamACLReadRoles :: ![Text]
        -- ^ Roles and users permitted to read the stream.
      , streamACLWriteRoles :: ![Text]
        -- ^ Roles and users permitted to write to the stream.
      , streamACLDeleteRoles :: ![Text]
        -- ^ Roles and users permitted to delete to the stream.
      , streamACLMetaReadRoles :: ![Text]
        -- ^ Roles and users permitted to read stream metadata.
      , streamACLMetaWriteRoles :: ![Text]
        -- ^ Roles and users permitted to write stream metadata.
      } deriving Show

-------------------------------------------------------------------------------
instance A.FromJSON StreamACL where
    parseJSON = parseStreamACL

-------------------------------------------------------------------------------
instance A.ToJSON StreamACL where
    toJSON = streamACLJSON

--------------------------------------------------------------------------------
-- | 'StreamACL' with no role or users whatsoever.
emptyStreamACL :: StreamACL
emptyStreamACL = StreamACL
                 { streamACLReadRoles      = []
                 , streamACLWriteRoles     = []
                 , streamACLDeleteRoles    = []
                 , streamACLMetaReadRoles  = []
                 , streamACLMetaWriteRoles = []
                 }

--------------------------------------------------------------------------------
-- | Represents stream metadata with strongly typed properties for system values
--   and a dictionary-like interface for custom values.
data StreamMetadata
    = StreamMetadata
      { streamMetadataMaxCount :: !(Maybe Int32)
        -- ^ The maximum number of events allowed in the stream.
      , streamMetadataMaxAge :: !(Maybe TimeSpan)
        -- ^ The maximum age of events allowed in the stream.
      , streamMetadataTruncateBefore :: !(Maybe Int32)
        -- ^ The event number from which previous events can be scavenged. This
        --   is used to implement soft-deletion of streams.
      , streamMetadataCacheControl :: !(Maybe TimeSpan)
        -- ^ The amount of time for which the stream head is cachable.
      , streamMetadataACL :: !StreamACL
        -- ^ The access control list for the stream.
      , streamMetadataCustom :: !Object
        -- ^ An enumerable of key-value pairs of keys to JSON text for
        --   user-provider metadata.
      } deriving Show

--------------------------------------------------------------------------------
-- | Gets a custom property value from metadata.
getCustomPropertyValue :: StreamMetadata -> Text -> Maybe A.Value
getCustomPropertyValue s k = lookup k obj
  where
    obj = streamMetadataCustom s

--------------------------------------------------------------------------------
-- | Get a custom property value from metadata.
getCustomProperty :: A.FromJSON a => StreamMetadata -> Text -> Maybe a
getCustomProperty s k = do
    v <- getCustomPropertyValue s k
    case A.fromJSON v of
        A.Error _   -> Nothing
        A.Success a -> return a

-------------------------------------------------------------------------------
instance A.FromJSON StreamMetadata where
    parseJSON = parseStreamMetadata

-------------------------------------------------------------------------------
instance A.ToJSON StreamMetadata where
    toJSON = streamMetadataJSON

--------------------------------------------------------------------------------
-- | 'StreamMetadata' with everything set to 'Nothing', using 'emptyStreamACL'
--   and an empty 'Object'.
emptyStreamMetadata :: StreamMetadata
emptyStreamMetadata = StreamMetadata
                      { streamMetadataMaxCount       = Nothing
                      , streamMetadataMaxAge         = Nothing
                      , streamMetadataTruncateBefore = Nothing
                      , streamMetadataCacheControl   = Nothing
                      , streamMetadataACL            = emptyStreamACL
                      , streamMetadataCustom         = mempty
                      }

--------------------------------------------------------------------------------
-- | Maps an 'Object' to a list of 'Pair' to ease the 'StreamMetadata'.
customMetaToPairs :: Object -> [Pair]
customMetaToPairs = fmap go . mapToList
  where
    go (k,v) = k .= v

--------------------------------------------------------------------------------
-- | Serialized a 'StreamACL' to 'Value' for serialization purpose.
streamACLJSON :: StreamACL -> A.Value
streamACLJSON StreamACL{..} =
    A.object [ p_readRoles      .= streamACLReadRoles
             , p_writeRoles     .= streamACLWriteRoles
             , p_deleteRoles    .= streamACLDeleteRoles
             , p_metaReadRoles  .= streamACLMetaReadRoles
             , p_metaWriteRoles .= streamACLMetaWriteRoles
             ]

--------------------------------------------------------------------------------
-- | Serialized a 'StreamMetadata' to 'Value' for serialization purpose.
streamMetadataJSON :: StreamMetadata -> A.Value
streamMetadataJSON StreamMetadata{..} =
    A.object $ [ p_maxAge         .= fmap toInt64 streamMetadataMaxAge
               , p_maxCount       .= streamMetadataMaxCount
               , p_truncateBefore .= streamMetadataTruncateBefore
               , p_cacheControl   .= fmap toInt64 streamMetadataCacheControl
               , p_acl            .= streamACLJSON streamMetadataACL
               ] ++ custPairs
  where
    custPairs = customMetaToPairs streamMetadataCustom

    toInt64 :: TimeSpan -> Int64
    toInt64 = truncate . totalSeconds

--------------------------------------------------------------------------------
-- Stream ACL Properties
--------------------------------------------------------------------------------
-- | Read ACL property.
p_readRoles :: Text
p_readRoles = "$r"

--------------------------------------------------------------------------------
-- | Write ACL property.
p_writeRoles :: Text
p_writeRoles = "$w"

--------------------------------------------------------------------------------
-- | Delete ACL property.
p_deleteRoles :: Text
p_deleteRoles = "$d"

--------------------------------------------------------------------------------
-- | Metadata read ACL property.
p_metaReadRoles :: Text
p_metaReadRoles = "$mr"

--------------------------------------------------------------------------------
-- | Metadata write ACL property.
p_metaWriteRoles :: Text
p_metaWriteRoles = "$mw"

--------------------------------------------------------------------------------
-- Internal MetaData Properties
--------------------------------------------------------------------------------
-- | Max age metadata property.
p_maxAge :: Text
p_maxAge = "$maxAge"

--------------------------------------------------------------------------------
-- | Max count metadata property.
p_maxCount :: Text
p_maxCount = "$maxCount"

--------------------------------------------------------------------------------
-- | truncated before metadata property.
p_truncateBefore :: Text
p_truncateBefore = "$tb"

--------------------------------------------------------------------------------
-- | Cache control metadata property.
p_cacheControl :: Text
p_cacheControl = "$cacheControl"

--------------------------------------------------------------------------------
-- | ACL metadata property.
p_acl :: Text
p_acl = "$acl"

--------------------------------------------------------------------------------
-- | Gathers every internal metadata properties into a 'Set'. It used to safely
--   'StreamMetadata' in JSON.
internalMetaProperties :: Set Text
internalMetaProperties =
    setFromList [ p_maxAge
                , p_maxCount
                , p_truncateBefore
                , p_cacheControl
                , p_acl
                ]

--------------------------------------------------------------------------------
-- | Only keeps the properties the users has set.
keepUserProperties :: Object -> Object
keepUserProperties = filterWithKey go
  where
    go k _ = notMember k internalMetaProperties

--------------------------------------------------------------------------------
-- | Parses a 'NominalDiffTime' from an 'Object' given a JSON property.
parseNominalDiffTime :: Text -> Object -> Parser (Maybe NominalDiffTime)
parseNominalDiffTime k m = fmap (fmap go) (m A..: k)
  where
    go i = (realToFrac $ CTime i)

--------------------------------------------------------------------------------
-- | Parses 'StreamACL'.
parseStreamACL :: A.Value -> Parser StreamACL
parseStreamACL (A.Object m) =
    StreamACL              <$>
    m A..: p_readRoles     <*>
    m A..: p_writeRoles    <*>
    m A..: p_deleteRoles   <*>
    m A..: p_metaReadRoles <*>
    m A..: p_metaWriteRoles
parseStreamACL _ = mzero

--------------------------------------------------------------------------------
-- | Parses 'StreamMetadata'.
parseStreamMetadata :: A.Value -> Parser StreamMetadata
parseStreamMetadata (A.Object m) =
    StreamMetadata                    <$>
    m A..: p_maxCount                 <*>
    parseTimeSpan p_maxAge            <*>
    m A..: p_truncateBefore           <*>
    parseTimeSpan p_cacheControl      <*>
    (m A..: p_acl >>= parseStreamACL) <*>
    pure (keepUserProperties m)
  where
    parseTimeSpan ::  Text -> Parser (Maybe TimeSpan)
    parseTimeSpan prop = do
        (secs :: Maybe Int64) <- m A..: prop
        return $ fmap (fromSeconds . realToFrac) secs
parseStreamMetadata _ = mzero

--------------------------------------------------------------------------------
-- Builder
--------------------------------------------------------------------------------
-- | Allows to build a structure using 'Monoid' functions.
type Builder a = Endo a

--------------------------------------------------------------------------------
-- | Build a structure given a 'Builder' and an initial value.
build :: a -> Builder a -> a
build a (Endo k) = k a

--------------------------------------------------------------------------------
-- | A 'Builder' applies to 'StreamACL'.
type StreamACLBuilder = Builder StreamACL

--------------------------------------------------------------------------------
-- | Sets role names with read permission for the stream.
setReadRoles :: [Text] -> StreamACLBuilder
setReadRoles xs = Endo $ \s -> s { streamACLReadRoles = xs }

--------------------------------------------------------------------------------
-- | Sets a single role name with read permission for the stream.
setReadRole :: Text -> StreamACLBuilder
setReadRole x = setReadRoles [x]

--------------------------------------------------------------------------------
-- | Sets role names with write permission for the stream.
setWriteRoles :: [Text] -> StreamACLBuilder
setWriteRoles xs = Endo $ \s -> s { streamACLWriteRoles = xs }

--------------------------------------------------------------------------------
-- | Sets a single role name with write permission for the stream.
setWriteRole :: Text -> StreamACLBuilder
setWriteRole x = setWriteRoles [x]

--------------------------------------------------------------------------------
-- | Sets role names with delete permission for the stream.
setDeleteRoles :: [Text] -> StreamACLBuilder
setDeleteRoles xs = Endo $ \s -> s { streamACLDeleteRoles = xs }

--------------------------------------------------------------------------------
-- | Sets a single role name with delete permission for the stream.
setDeleteRole :: Text -> StreamACLBuilder
setDeleteRole x = setDeleteRoles [x]

--------------------------------------------------------------------------------
-- | Sets role names with metadata read permission for the stream.
setMetaReadRoles :: [Text] -> StreamACLBuilder
setMetaReadRoles xs = Endo $ \s -> s { streamACLMetaReadRoles = xs }

--------------------------------------------------------------------------------
-- | Sets a single role name with metadata read permission for the stream.
setMetaReadRole :: Text -> StreamACLBuilder
setMetaReadRole x = setMetaReadRoles [x]

--------------------------------------------------------------------------------
-- | Sets role names with metadata write permission for the stream.
setMetaWriteRoles :: [Text] -> StreamACLBuilder
setMetaWriteRoles xs = Endo $ \s -> s { streamACLMetaWriteRoles = xs }

--------------------------------------------------------------------------------
-- | Sets a single role name with metadata write permission for the stream.
setMetaWriteRole :: Text -> StreamACLBuilder
setMetaWriteRole x = setMetaWriteRoles [x]

--------------------------------------------------------------------------------
-- | Builds a 'StreamACL' from a 'StreamACLBuilder'.
buildStreamACL :: StreamACLBuilder -> StreamACL
buildStreamACL b = modifyStreamACL b emptyStreamACL

--------------------------------------------------------------------------------
-- | Modifies a 'StreamACL' using a 'StreamACLBuilder'.
modifyStreamACL :: StreamACLBuilder -> StreamACL -> StreamACL
modifyStreamACL b acl = build acl b

--------------------------------------------------------------------------------
-- | A 'Builder' applies to 'StreamMetadata'.
type StreamMetadataBuilder = Builder StreamMetadata

--------------------------------------------------------------------------------
-- | Sets the maximum number of events allowed in the stream.
setMaxCount :: Int32 -> StreamMetadataBuilder
setMaxCount i = Endo $ \s -> s { streamMetadataMaxCount = Just i }

--------------------------------------------------------------------------------
-- | Sets the maximum age of events allowed in the stream.
setMaxAge :: TimeSpan -> StreamMetadataBuilder
setMaxAge d = Endo $ \s -> s { streamMetadataMaxAge = Just d }

--------------------------------------------------------------------------------
-- | Sets the event number from which previous events can be scavenged.
setTruncateBefore :: Int32 -> StreamMetadataBuilder
setTruncateBefore i = Endo $ \s -> s { streamMetadataTruncateBefore = Just i }

--------------------------------------------------------------------------------
-- | Sets the amount of time for which the stream head is cachable.
setCacheControl :: TimeSpan -> StreamMetadataBuilder
setCacheControl d = Endo $ \s -> s { streamMetadataCacheControl = Just d }

--------------------------------------------------------------------------------
-- | Overwrites any previous 'StreamACL' by the given one in a
--   'StreamMetadataBuilder'.
setACL :: StreamACL -> StreamMetadataBuilder
setACL a = Endo $ \s -> s { streamMetadataACL = a }

--------------------------------------------------------------------------------
-- | Updates a 'StreamMetadata''s 'StreamACL' given a 'StreamACLBuilder'.
modifyACL :: StreamACLBuilder -> StreamMetadataBuilder
modifyACL b = Endo $ \s ->
    let old = streamMetadataACL s
    in s { streamMetadataACL = modifyStreamACL b old }

--------------------------------------------------------------------------------
-- | Sets a custom metadata property.
setCustomProperty :: ToJSON a => Text -> a -> StreamMetadataBuilder
setCustomProperty k v = Endo $ \s ->
    let m  = streamMetadataCustom s
        m' = insertMap k (toJSON v) m in
     s { streamMetadataCustom = m' }

--------------------------------------------------------------------------------
-- | Builds a 'StreamMetadata' from a 'StreamMetadataBuilder'.
buildStreamMetadata :: StreamMetadataBuilder -> StreamMetadata
buildStreamMetadata b = modifyStreamMetadata b emptyStreamMetadata

--------------------------------------------------------------------------------
-- | Modifies a 'StreamMetadata' using a 'StreamMetadataBuilder'
modifyStreamMetadata :: StreamMetadataBuilder
                     -> StreamMetadata
                     -> StreamMetadata
modifyStreamMetadata b meta = build meta b

--------------------------------------------------------------------------------
-- | Represents stream metadata as a series of properties for system data and a
--   'StreamMetadata' object for user metadata.
data StreamMetadataResult
    = StreamMetadataResult
      { streamMetaResultStream :: !Text
        -- ^ The name of the stream.
      , streamMetaResultVersion :: !Int32
        -- ^ The version of the metadata format.
      , streamMetaResultData :: !StreamMetadata
        -- ^ A 'StreamMetadata' containing user-specified metadata.
      }
    | NotFoundStreamMetadataResult { streamMetaResultStream :: !Text }
      -- ^ When the stream is either not found or 'no stream'.
    | DeletedStreamMetadataResult { streamMetaResultStream :: !Text }
      -- ^ When the stream is soft-deleted.
    deriving Show

--------------------------------------------------------------------------------
-- | System supported consumer strategies for use with persistent subscriptions.
data SystemConsumerStrategy
    = DispatchToSingle
      -- ^ Distributes events to a single client until it is full. Then round
      --   robin to the next client.
    | RoundRobin
      -- ^ Distributes events to each client in a round robin fashion.
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | Maps a 'SystemConsumerStrategy' to a 'Text' understandable by the server.
strategyText :: SystemConsumerStrategy -> Text
strategyText DispatchToSingle = "DispatchToSingle"
strategyText RoundRobin       = "RoundRobin"

--------------------------------------------------------------------------------
-- | Tries to parse a 'SystemConsumerStrategy' given a raw 'Text'.
strategyFromText :: Text -> Maybe SystemConsumerStrategy
strategyFromText "DispatchToSingle" = Just DispatchToSingle
strategyFromText "RoundRobin"       = Just RoundRobin
strategyFromText _                  = Nothing

--------------------------------------------------------------------------------
-- | Gathers every persistent subscription property.
data PersistentSubscriptionSettings =
    PersistentSubscriptionSettings
    { psSettingsResolveLinkTos :: !Bool
      -- ^ Whether or not the persistent subscription should resolve linkTo
      --   events to their linked events.
    , psSettingsStartFrom :: !Int32
      -- ^ Where the subscription should start from (position).
    , psSettingsExtraStats :: !Bool
      -- ^ Whether or not in depth latency statistics should be tracked on this
      --   subscription.
    , psSettingsMsgTimeout :: !TimeSpan
      -- ^ The amount of time after which a message should be considered to be
      --   timeout and retried.
    , psSettingsMaxRetryCount :: !Int32
      -- ^ The maximum number of retries (due to timeout) before a message get
      --   considered to be parked.
    , psSettingsLiveBufSize :: !Int32
      -- ^ The size of the buffer listening to live messages as they happen.
    , psSettingsReadBatchSize :: !Int32
      -- ^ The number of events read at a time when paging in history.
    , psSettingsHistoryBufSize :: !Int32
      -- ^ The number  of events to cache when paging through history.
    , psSettingsCheckPointAfter :: !TimeSpan
      -- ^ The amount of time to try checkpoint after.
    , psSettingsMinCheckPointCount :: !Int32
      -- ^ The minimum number of messages to checkpoint.
    , psSettingsMaxCheckPointCount :: !Int32
      -- ^ The maximum number of message to checkpoint. If this number is
      --   reached, a checkpoint will be forced.
    , psSettingsMaxSubsCount :: !Int32
      -- ^ The maximum number of subscribers allowed.
    , psSettingsNamedConsumerStrategy :: !SystemConsumerStrategy
      -- ^ The strategy to use for distributing events to client consumers.
    } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | System default persistent subscription settings.
defaultPersistentSubscriptionSettings :: PersistentSubscriptionSettings
defaultPersistentSubscriptionSettings =
    PersistentSubscriptionSettings
    { psSettingsResolveLinkTos        = False
    , psSettingsStartFrom             = (-1)
    , psSettingsExtraStats            = False
    , psSettingsMsgTimeout            = fromSeconds 30
    , psSettingsMaxRetryCount         = 500
    , psSettingsLiveBufSize           = 500
    , psSettingsReadBatchSize         = 10
    , psSettingsHistoryBufSize        = 20
    , psSettingsCheckPointAfter       = fromSeconds 2
    , psSettingsMinCheckPointCount    = 10
    , psSettingsMaxCheckPointCount    = 1000
    , psSettingsMaxSubsCount          = 0
    , psSettingsNamedConsumerStrategy = RoundRobin
    }
