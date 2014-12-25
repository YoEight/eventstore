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
import Control.Applicative ((<|>))
import Control.Exception
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Int
import Data.Maybe
import Data.Typeable
import Data.Word
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
    | ServerError (Maybe Text)
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception OperationException

--------------------------------------------------------------------------------
type OperationExceptional a = Either OperationException a

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
    = Json A.Value (Maybe A.Value)

--------------------------------------------------------------------------------
eventDataType :: EventData -> Int32
eventDataType (Json _ _) = 1

--------------------------------------------------------------------------------
eventMetadataType :: EventData -> Int32
eventMetadataType _ = 0

--------------------------------------------------------------------------------
withJson :: A.Value -> EventData
withJson value = Json value Nothing

--------------------------------------------------------------------------------
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
data ExpectedVersion
    = Any         -- ^ Says that you should not conflict with anything
    | NoStream    -- ^ Stream should not exist when doing your write
    | EmptyStream -- ^ Stream should exist but be empty when doing the write
    deriving Show

--------------------------------------------------------------------------------
expVersionInt32 :: ExpectedVersion -> Int32
expVersionInt32 Any         = -2
expVersionInt32 NoStream    = -1
expVersionInt32 EmptyStream = 0

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
         -> Int32            -- ^ Data content type
         -> Int32            -- ^ Metadata content type
         -> ByteString       -- ^ Event data
         -> Maybe ByteString -- ^ Metadata
         -> IO NewEvent
newEvent evt_type data_type meta_type evt_data evt_meta = do
    new_uuid <- randomIO
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
data Position
    = Position
      { positionCommit  :: !Int64
      , positionPrepare :: !Int64
      }
    deriving Show

--------------------------------------------------------------------------------
data WriteResult
    = WriteResult
      { writeNextExpectedVersion :: !Int32
      , writePosition            :: !Position
      }
    deriving Show

--------------------------------------------------------------------------------
newtype DeleteResult
    = DeleteResult { deleteStreamPosition :: Position }
    deriving Show

--------------------------------------------------------------------------------
data RecordedEvent
    = RecordedEvent
      { recordedEventStreamId     :: !Text
      , recordedEventId           :: !UUID
      , recordedEventNumber       :: !Int32
      , recordedEventType         :: !Text
      , recordedEventData         :: !ByteString
      , recordedEventMetadata     :: !(Maybe ByteString)
      , recordedEventIsJson       :: !Bool
      , recordedEventCreated      :: !(Maybe UTCTime)
      , recordedEventCreatedEpoch :: !(Maybe Integer)
      }
    deriving Show

--------------------------------------------------------------------------------
newRecordedEvent :: EventRecord -> RecordedEvent
newRecordedEvent er = re
  where
    evt_id      = getField $ eventRecordId er
    evt_uuid    = fromJust $ fromByteString $ fromStrict evt_id
    data_type   = getField $ eventRecordDataType er
    created     = getField $ eventRecordCreated er
    epoch       = getField $ eventRecordCreatedEpoch er
    utc_created = fmap (posixSecondsToUTCTime . fromInteger . toInteger) created

    re = RecordedEvent
         { recordedEventStreamId     = getField $ eventRecordStreamId er
         , recordedEventNumber       = getField $ eventRecordNumber er
         , recordedEventId           = evt_uuid
         , recordedEventType         = getField $ eventRecordType er
         , recordedEventData         = getField $ eventRecordData er
         , recordedEventMetadata     = getField $ eventRecordMetadata er
         , recordedEventIsJson       = data_type == 1
         , recordedEventCreated      = utc_created
         , recordedEventCreatedEpoch = fmap toInteger epoch
         }

--------------------------------------------------------------------------------
data ResolvedEvent
    = ResolvedEvent
      { resolvedEventRecord :: !(Maybe RecordedEvent)
      , resolvedEventLink   :: !(Maybe RecordedEvent)
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
resolvedEventOriginal :: ResolvedEvent -> Maybe RecordedEvent
resolvedEventOriginal (ResolvedEvent record link) =
    link <|> record

--------------------------------------------------------------------------------
eventResolved :: ResolvedEvent -> Bool
eventResolved = isJust . resolvedEventOriginal

--------------------------------------------------------------------------------
resolvedEventOriginalStreamId :: ResolvedEvent -> Maybe Text
resolvedEventOriginalStreamId =
    fmap recordedEventStreamId . resolvedEventOriginal

--------------------------------------------------------------------------------
data ReadDirection
    = Forward
    | Backward
    deriving Show

--------------------------------------------------------------------------------
-- Transaction
--------------------------------------------------------------------------------
data Transaction
    = Transaction
      { transactionId              :: Int64
      , transactionStreamId        :: Text
      , transactionExpectedVersion :: ExpectedVersion
      , transactionCommit          :: IO (Async WriteResult)
      , transactionSendEvents      :: [Event] -> IO (Async ())
      , transactionRollback        :: IO ()
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
-- Package
--------------------------------------------------------------------------------
data Package
    = Package
      { packageCmd         :: !Word8
      , packageFlag        :: !Flag
      , packageCorrelation :: !UUID
      , packageData        :: !ByteString
      }
    deriving Show

--------------------------------------------------------------------------------
-- Settings
--------------------------------------------------------------------------------
-- | Global @ConnectionManager@ settings
data Settings
    = Settings
      { _heartbeatInterval :: NominalDiffTime
      , _heartbeatTimeout  :: NominalDiffTime
      , _requireMaster     :: Bool
      }

--------------------------------------------------------------------------------
defaultSettings :: Settings
defaultSettings = Settings
                  { _heartbeatInterval = msDiffTime 750  -- 750ms
                  , _heartbeatTimeout  = msDiffTime 1500 -- 1500ms
                  , _requireMaster     = True
                  }

--------------------------------------------------------------------------------
-- | Millisecond timespan
msDiffTime :: Float -> NominalDiffTime
msDiffTime i = fromRational $ toRational (i / 1000)
