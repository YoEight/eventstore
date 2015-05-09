{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DataKinds          #-}
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
import Control.Applicative
import Control.Exception
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Int
import Data.Maybe
import Data.Typeable
import Data.Word
import Foreign.C.Types (CTime(..))
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
import           Control.Concurrent.Async hiding (link)
import qualified Data.Aeson as A
import           Data.ProtocolBuffers
import           Data.Text (Text)
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.UUID (UUID, fromByteString, toByteString)
import           System.Random

--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------
data InternalException
    = ConnectionClosedByServer
    | Stopped
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception InternalException

--------------------------------------------------------------------------------
data OperationException
    = WrongExpectedVersion Text ExpectedVersion -- ^ Stream and Expected Version
    | StreamDeleted Text                        -- ^ Stream
    | InvalidTransaction
    | AccessDenied Text                         -- ^ Stream
    | InvalidServerResponse Word8 Word8         -- ^ Expected, Found
    | ProtobufDecodingError String
    | ServerError (Maybe Text)                  -- ^ Reason
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception OperationException

--------------------------------------------------------------------------------
type OperationExceptional a = Either OperationException a

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
      }

--------------------------------------------------------------------------------
createEvent :: Text       -- ^ Event type
            -> Maybe UUID -- ^ Event ID, generated if 'Nothing'
            -> EventData  -- ^ Event data
            -> Event
createEvent = Event

--------------------------------------------------------------------------------
-- | Holds event data.
data EventData
    = Json A.Value (Maybe A.Value)

--------------------------------------------------------------------------------
eventDataType :: EventData -> Int32
eventDataType (Json _ _) = 1

--------------------------------------------------------------------------------
eventMetadataType :: EventData -> Int32
eventMetadataType _ = 0

--------------------------------------------------------------------------------
-- | Creates a event using JSON format
withJson :: A.Value -> EventData
withJson value = Json value Nothing

--------------------------------------------------------------------------------
-- | Create a event with metadata using JSON format
withJsonAndMetadata :: A.Value -> A.Value -> EventData
withJsonAndMetadata value metadata = Json value (Just metadata)

--------------------------------------------------------------------------------
eventDataBytes :: EventData -> ByteString
eventDataBytes (Json value _) = toStrict $ A.encode value

--------------------------------------------------------------------------------
eventMetadataBytes :: EventData -> Maybe ByteString
eventMetadataBytes (Json _ meta_m) = fmap (toStrict . A.encode) meta_m

--------------------------------------------------------------------------------
-- Expected Version
--------------------------------------------------------------------------------
-- | Constants used for expected version control.
--
--   The use of expected version can be a bit tricky especially when discussing
--   idempotency assurances given by the EventStore.
--
--   The EventStore  will assure idempotency for all operations using any value
--   in 'ExpectedVersion' except for 'anyStream'. When using 'anyStream' the EventStore will
--   do its best to assure idempotency but will not guarantee idempotency.
data ExpectedVersion
    = Any
    | NoStream
    | EmptyStream
    | Exact Int32
    deriving Show

--------------------------------------------------------------------------------
expVersionInt32 :: ExpectedVersion -> Int32
expVersionInt32 Any         = -2
expVersionInt32 NoStream    = -1
expVersionInt32 EmptyStream = 0
expVersionInt32 (Exact i)   = i

--------------------------------------------------------------------------------
-- | This write should not conflict with anything and should always succeed.
anyStream :: ExpectedVersion
anyStream = Any

--------------------------------------------------------------------------------
-- | The stream being written to should not yet exist. If it does exist
--   treat that as a concurrency problem.
noStream :: ExpectedVersion
noStream = NoStream

--------------------------------------------------------------------------------
-- | The stream should exist and should be empty. If it does not exist or
--   is not empty, treat that as a concurrency problem.
emptyStream :: ExpectedVersion
emptyStream =EmptyStream

--------------------------------------------------------------------------------
-- | States that the last event written to the stream should have a
--   sequence number matching your expected value.
exactStream :: Int32 -> ExpectedVersion
exactStream i
    | i < 0     = error $ "expected version must be >= 0, but is " ++ show i
    | otherwise = Exact i

--------------------------------------------------------------------------------
-- EventStore Messages
--------------------------------------------------------------------------------
data OpResult
    = OP_SUCCESS
    | OP_PREPARE_TIMEOUT
    | OP_COMMIT_TIMEOUT
    | OP_FORWARD_TIMEOUT
    | OP_WRONG_EXPECTED_VERSION
    | OP_STREAM_DELETED
    | OP_INVALID_TRANSACTION
    | OP_ACCESS_DENIED
    deriving (Eq, Enum, Show)

--------------------------------------------------------------------------------
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
newEvent :: Text             -- ^ Event type
         -> Maybe UUID       -- ^ Event ID
         -> Int32            -- ^ Data content type
         -> Int32            -- ^ Metadata content type
         -> ByteString       -- ^ Event data
         -> Maybe ByteString -- ^ Metadata
         -> IO NewEvent
newEvent evt_type evt_id data_type meta_type evt_data evt_meta = do
    new_uuid <- maybe randomIO return evt_id
    let uuid_bytes = toStrict $ toByteString new_uuid
        new_evt    = NewEvent
                     { newEventId           = putField uuid_bytes
                     , newEventType         = putField evt_type
                     , newEventDataType     = putField data_type
                     , newEventMetadataType = putField meta_type
                     , newEventData         = putField evt_data
                     , newEventMetadata     = putField evt_meta
                     }

    return new_evt

--------------------------------------------------------------------------------
data TransactionStart
    = TransactionStart
      { transactionStartStreamId        :: Required 1 (Value Text)
      , transactionStartExpectedVersion :: Required 2 (Value Int32)
      , transactionStartRequireMaster   :: Required 3 (Value Bool)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
newTransactionStart :: Text
                    -> Int32
                    -> Bool
                    -> TransactionStart
newTransactionStart stream_id exp_ver req_master =
    TransactionStart
    { transactionStartStreamId        = putField stream_id
    , transactionStartExpectedVersion = putField exp_ver
    , transactionStartRequireMaster   = putField req_master
    }

--------------------------------------------------------------------------------
instance Encode TransactionStart

--------------------------------------------------------------------------------
data TransactionStartCompleted
    = TransactionStartCompleted
      { transactionSCId      :: Required 1 (Value Int64)
      , transactionSCResult  :: Required 2 (Enumeration OpResult)
      , transactionSCMessage :: Optional 3 (Value Text)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode TransactionStartCompleted

--------------------------------------------------------------------------------
data TransactionWrite
    = TransactionWrite
      { transactionWriteId            :: Required 1 (Value Int64)
      , transactionWriteEvents        :: Repeated 2 (Message NewEvent)
      , transactionWriteRequireMaster :: Required 3 (Value Bool)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode TransactionWrite

--------------------------------------------------------------------------------
newTransactionWrite :: Int64 -> [NewEvent] -> Bool -> TransactionWrite
newTransactionWrite trans_id evts req_master =
    TransactionWrite
    { transactionWriteId            = putField trans_id
    , transactionWriteEvents        = putField evts
    , transactionWriteRequireMaster = putField req_master
    }

--------------------------------------------------------------------------------
data TransactionWriteCompleted
    = TransactionWriteCompleted
      { transactionWCId      :: Required 1 (Value Int64)
      , transactionWCResult  :: Required 2 (Enumeration OpResult)
      , transactionWCMessage :: Optional 3 (Value Text)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode TransactionWriteCompleted

--------------------------------------------------------------------------------
data TransactionCommit
    = TransactionCommit
      { transactionCommitId            :: Required 1 (Value Int64)
      , transactionCommitRequireMaster :: Required 2 (Value Bool)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode TransactionCommit

--------------------------------------------------------------------------------
newTransactionCommit :: Int64 -> Bool -> TransactionCommit
newTransactionCommit trans_id req_master =
    TransactionCommit
    { transactionCommitId = putField trans_id
    , transactionCommitRequireMaster = putField req_master
    }

--------------------------------------------------------------------------------
data TransactionCommitCompleted
    = TransactionCommitCompleted
      { transactionCCId              :: Required 1 (Value Int64)
      , transactionCCResult          :: Required 2 (Enumeration OpResult)
      , transactionCCMessage         :: Optional 3 (Value Text)
      , transactionCCFirstNumber     :: Required 4 (Value Int32)
      , transactionCCLastNumber      :: Required 5 (Value Int32)
      , transactionCCPreparePosition :: Optional 6 (Value Int64)
      , transactionCCCommitPosition  :: Optional 7 (Value Int64)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode TransactionCommitCompleted

--------------------------------------------------------------------------------
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
data ResolvedIndexedEvent
    = ResolvedIndexedEvent
      { resolvedIndexedRecord :: Optional 1 (Message EventRecord)
      , resolvedIndexedLink   :: Optional 2 (Message EventRecord)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode ResolvedIndexedEvent

--------------------------------------------------------------------------------
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
-- | Representing the start of the transaction file.
positionStart :: Position
positionStart = Position 0 0

--------------------------------------------------------------------------------
-- | Representing the end of the transaction file.
positionEnd :: Position
positionEnd = Position (-1) (-1)

--------------------------------------------------------------------------------
-- | Returned after writing to a stream.
data WriteResult
    = WriteResult
      { writeNextExpectedVersion :: !Int32
        -- ^ Next expected version of the stream.
      , writePosition :: !Position
        -- ^ 'Position' of the write.
      }
    deriving Show

--------------------------------------------------------------------------------
-- | Returned after deleting a stream. 'Position' of the write.
newtype DeleteResult
    = DeleteResult { deleteStreamPosition :: Position }
    deriving Show

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
toUTC :: Int64 -> UTCTime
toUTC = posixSecondsToUTCTime . (/1000) . realToFrac . CTime

--------------------------------------------------------------------------------
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
      }
    deriving Show

--------------------------------------------------------------------------------
newResolvedEvent :: ResolvedIndexedEvent -> ResolvedEvent
newResolvedEvent rie = re
  where
    record = getField $ resolvedIndexedRecord rie
    link   = getField $ resolvedIndexedLink rie
    re     = ResolvedEvent
             { resolvedEventRecord = fmap newRecordedEvent record
             , resolvedEventLink   = fmap newRecordedEvent link
             }

--------------------------------------------------------------------------------
newResolvedEventFromBuf :: ResolvedEventBuf -> ResolvedEvent
newResolvedEventFromBuf reb = re
  where
    record = Just $ newRecordedEvent $ getField $ resolvedEventBufEvent reb
    link   = getField $ resolvedEventBufLink reb
    re     = ResolvedEvent
             { resolvedEventRecord = record
             , resolvedEventLink   = fmap newRecordedEvent link
             }

--------------------------------------------------------------------------------
-- | Returns the event that was read or which triggered the subscription.
--
--   If this 'ResolvedEvent' represents a link event, the link will be the
--   original event, otherwise it will be the event.
resolvedEventOriginal :: ResolvedEvent -> Maybe RecordedEvent
resolvedEventOriginal (ResolvedEvent record link) =
    link <|> record

--------------------------------------------------------------------------------
-- | Indicates whether this 'ResolvedEvent' is a resolved link event.
eventResolved :: ResolvedEvent -> Bool
eventResolved = isJust . resolvedEventOriginal

--------------------------------------------------------------------------------
-- | The stream name of the original event.
resolvedEventOriginalStreamId :: ResolvedEvent -> Maybe Text
resolvedEventOriginalStreamId =
    fmap recordedEventStreamId . resolvedEventOriginal

--------------------------------------------------------------------------------
-- | Represents the direction of read operation (both from $all an usual
--   streams).
data ReadDirection
    = Forward  -- ^ From beginning to end
    | Backward -- ^ From end to beginning
    deriving Show

--------------------------------------------------------------------------------
-- Transaction
--------------------------------------------------------------------------------
-- | Represents a multi-request transaction with the EventStore.
data Transaction
    = Transaction
      { transactionId :: Int64
        -- ^ The ID of the transaction. This can be used to recover a
        -- transaction later.
      , transactionStreamId :: Text
        -- ^ The name of the stream.
      , transactionExpectedVersion :: ExpectedVersion
        -- ^ Expected version of the stream.
      , transactionCommit :: IO (Async WriteResult)
        -- ^ Asynchronously commits this transaction.
      , transactionSendEvents :: [Event] -> IO (Async ())
        -- ^ Asynchronously writes to a transaction in the EventStore.
      , transactionRollback :: IO ()
        -- ^ Rollback this transaction.
      }

--------------------------------------------------------------------------------
-- Flag
--------------------------------------------------------------------------------
data Flag
    = None
    | Authenticated
    deriving Show

--------------------------------------------------------------------------------
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
    deriving Show

--------------------------------------------------------------------------------
credentials :: ByteString -- ^ Login
            -> ByteString -- ^ Password
            -> Credentials
credentials = Credentials

--------------------------------------------------------------------------------
-- Package
--------------------------------------------------------------------------------
data Package
    = Package
      { packageCmd         :: !Word8
      , packageCorrelation :: !UUID
      , packageData        :: !ByteString
      , packageCred        :: !(Maybe Credentials)
      }
    deriving Show

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
      }

--------------------------------------------------------------------------------
-- | Default global settings.
defaultSettings :: Settings
defaultSettings = Settings
                  { s_heartbeatInterval    = msDiffTime 750  -- 750ms
                  , s_heartbeatTimeout     = msDiffTime 1500 -- 1500ms
                  , s_requireMaster        = True
                  , s_credentials          = Nothing
                  , s_retry                = atMost 3
                  , s_reconnect_delay_secs = 3
                  }

--------------------------------------------------------------------------------
-- | Millisecond timespan
msDiffTime :: Float -> NominalDiffTime
msDiffTime i = fromRational $ toRational (i / 1000)
