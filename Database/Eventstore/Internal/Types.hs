{-# LANGUAGE    DeriveDataTypeable #-}
{-# LANGUAGE    DeriveGeneric      #-}
{-# LANGUAGE    DataKinds          #-}
{-# OPTIONS_GHC -fcontext-stack=26 #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.Eventstore.Internal.Types
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.Eventstore.Internal.Types where

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
import GHC.TypeLits

--------------------------------------------------------------------------------
import           Control.Concurrent.Async
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
    | InvalidServerResponse Command Command     -- ^ Expected, Found
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
data Operation
    = Operation
      { operationCreatePackage :: UUID    -> IO Package
      , operationInspect       :: Package -> IO Decision
      }

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
data WriteEvents
    = WriteEvents
      { writeStreamId        :: Required 1 (Value Text)
      , writeExpectedVersion :: Required 2 (Value Int32)
      , writeEvents          :: Repeated 3 (Message NewEvent)
      , writeRequireMaster   :: Required 4 (Value Bool)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode WriteEvents

--------------------------------------------------------------------------------
newWriteEvents :: Text        -- ^ Stream
               -> Int32       -- ^ Expected version
               -> [NewEvent]  -- ^ Events
               -> Bool        -- ^ Require master
               -> WriteEvents
newWriteEvents stream_id exp_ver evts req_master =
    WriteEvents
    { writeStreamId        = putField stream_id
    , writeExpectedVersion = putField exp_ver
    , writeEvents          = putField evts
    , writeRequireMaster   = putField req_master
    }

--------------------------------------------------------------------------------
data WriteEventsCompleted
    = WriteEventsCompleted
      { writeCompletedResult          :: Required 1 (Enumeration OpResult)
      , writeCompletedMessage         :: Optional 2 (Value Text)
      , writeCompletedFirstNumber     :: Required 3 (Value Int32)
      , writeCompletedLastNumber      :: Required 4 (Value Int32)
      , writeCompletedPreparePosition :: Optional 5 (Value Int64)
      , writeCompletedCommitPosition  :: Optional 6 (Value Int64)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode WriteEventsCompleted

--------------------------------------------------------------------------------
data DeleteStream
    = DeleteStream
      { deleteStreamId              :: Required 1 (Value Text)
      , deleteStreamExpectedVersion :: Required 2 (Value Int32)
      , deleteStreamRequireMaster   :: Required 3 (Value Bool)
      , deleteStreamHardDelete      :: Optional 4 (Value Bool)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode DeleteStream

--------------------------------------------------------------------------------
newDeleteStream :: Text
                -> Int32
                -> Bool
                -> Maybe Bool
                -> DeleteStream
newDeleteStream stream_id exp_ver req_master hard_delete =
    DeleteStream
    { deleteStreamId              = putField stream_id
    , deleteStreamExpectedVersion = putField exp_ver
    , deleteStreamRequireMaster   = putField req_master
    , deleteStreamHardDelete      = putField hard_delete
    }

--------------------------------------------------------------------------------
data DeleteStreamCompleted
    = DeleteStreamCompleted
      { deleteCompletedResult          :: Required 1 (Enumeration OpResult)
      , deleteCompletedMessage         :: Optional 2 (Value Text)
      , deleteCompletedPreparePosition :: Optional 3 (Value Int64)
      , deleteCompletedCommitPosition  :: Optional 4 (Value Int64)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode DeleteStreamCompleted

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
data ReadEvent
    = ReadEvent
      { readEventStreamId       :: Required 1 (Value Text)
      , readEventNumber         :: Required 2 (Value Int32)
      , readEventResolveLinkTos :: Required 3 (Value Bool)
      , readEventRequireMaster  :: Required 4 (Value Bool)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode ReadEvent

--------------------------------------------------------------------------------
newReadEvent :: Text -> Int32 -> Bool -> Bool -> ReadEvent
newReadEvent stream_id evt_num res_link_tos req_master =
    ReadEvent
    { readEventStreamId       = putField stream_id
    , readEventNumber         = putField evt_num
    , readEventResolveLinkTos = putField res_link_tos
    , readEventRequireMaster  = putField req_master
    }

--------------------------------------------------------------------------------
data ReadEventResult
    = RE_SUCCESS
    | RE_NOT_FOUND
    | RE_NO_STREAM
    | RE_STREAM_DELETED
    | RE_ERROR
    | RE_ACCESS_DENIED
    deriving (Eq, Enum, Show)

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
data ReadEventCompleted
    = ReadEventCompleted
      { readCompletedResult       :: Required 1 (Enumeration ReadEventResult)
      , readCompletedIndexedEvent :: Required 2 (Message ResolvedIndexedEvent)
      , readCompletedError        :: Optional 3 (Value Text)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode ReadEventCompleted

--------------------------------------------------------------------------------
data ReadStreamEvents
    = ReadStreamEvents
      { readStreamId             :: Required 1 (Value Text)
      , readStreamEventNumber    :: Required 2 (Value Int32)
      , readStreamMaxCount       :: Required 3 (Value Int32)
      , readStreamResolveLinkTos :: Required 4 (Value Bool)
      , readStreamRequireMaster  :: Required 5 (Value Bool)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
newReadStreamEvents :: Text
                    -> Int32
                    -> Int32
                    -> Bool
                    -> Bool
                    -> ReadStreamEvents
newReadStreamEvents stream_id evt_num max_c res_link_tos req_master =
    ReadStreamEvents
    { readStreamId             = putField stream_id
    , readStreamEventNumber    = putField evt_num
    , readStreamMaxCount       = putField max_c
    , readStreamResolveLinkTos = putField res_link_tos
    , readStreamRequireMaster  = putField req_master
    }

--------------------------------------------------------------------------------
instance Encode ReadStreamEvents

--------------------------------------------------------------------------------
data ReadStreamResult
    = RS_SUCCESS
    | RS_NO_STREAM
    | RS_STREAM_DELETED
    | RS_NOT_MODIFIED
    | RS_ERROR
    | RS_ACCESS_DENIED
    deriving (Eq, Enum, Show)

--------------------------------------------------------------------------------
data ReadStreamEventsCompleted
    = ReadStreamEventsCompleted
      { readSECEvents             :: Repeated 1 (Message ResolvedIndexedEvent)
      , readSECResult             :: Required 2 (Enumeration ReadStreamResult)
      , readSECNextNumber         :: Required 3 (Value Int32)
      , readSECLastNumber         :: Required 4 (Value Int32)
      , readSECEndOfStream        :: Required 5 (Value Bool)
      , readSECLastCommitPosition :: Required 6 (Value Int64)
      , readSECError              :: Optional 7 (Value Text)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode ReadStreamEventsCompleted

--------------------------------------------------------------------------------
data ReadAllEvents
    = ReadAllEvents
      { readAllEventsCommitPosition  :: Required 1 (Value Int64)
      , readAllEventsPreparePosition :: Required 2 (Value Int64)
      , readAllEventsMaxCount        :: Required 3 (Value Int32)
      , readAllEventsResolveLinkTos  :: Required 4 (Value Bool)
      , readAllEventsRequireMaster   :: Required 5 (Value Bool)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode ReadAllEvents

--------------------------------------------------------------------------------
newReadAllEvents :: Int64
                 -> Int64
                 -> Int32
                 -> Bool
                 -> Bool
                 -> ReadAllEvents
newReadAllEvents c_pos p_pos max_c res_link_tos req_master =
    ReadAllEvents
    { readAllEventsCommitPosition  = putField c_pos
    , readAllEventsPreparePosition = putField p_pos
    , readAllEventsMaxCount        = putField max_c
    , readAllEventsResolveLinkTos  = putField res_link_tos
    , readAllEventsRequireMaster   = putField req_master
    }

--------------------------------------------------------------------------------
data ReadAllResult
    = RA_SUCCESS
    | RA_NOT_MODIFIED
    | RA_ERROR
    | RA_ACCESS_DENIED
    deriving (Eq, Enum, Show)

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
data ReadAllEventsCompleted
    = ReadAllEventsCompleted
      { readAECCommitPosition      :: Required 1 (Value Int64)
      , readAECPreparePosition     :: Required 2 (Value Int64)
      , readAECEvents              :: Repeated 3 (Message ResolvedEventBuf)
      , readAECNextCommitPosition  :: Required 4 (Value Int64)
      , readAECNextPreparePosition :: Required 5 (Value Int64)
      , readAECResult              :: Optional 6 (Enumeration ReadAllResult)
      , readAECError               :: Optional 7 (Value Text)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode ReadAllEventsCompleted

--------------------------------------------------------------------------------
-- Result
--------------------------------------------------------------------------------
data Decision
    = DoNothing
    | EndOperation
    | Retry
    | Reconnection
    | Subscribed

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
data ReadResult
    = ReadResult
      { readResultStatus        :: !ReadEventResult
      , readResultStreamId      :: !Text
      , readResultEventNumber   :: !Int32
      , readResultResolvedEvent :: !(Maybe ResolvedEvent)
      }
    deriving Show

--------------------------------------------------------------------------------
newReadResult :: ReadEventResult
              -> Text
              -> Int32
              -> ResolvedIndexedEvent
              -> ReadResult
newReadResult status stream_id evt_num rie = rr
  where
    may_re =
        case status of
            RE_SUCCESS -> Just $ newResolvedEvent rie
            _          -> Nothing

    rr = ReadResult
         { readResultStatus        = status
         , readResultStreamId      = stream_id
         , readResultEventNumber   = evt_num
         , readResultResolvedEvent = may_re
         }

--------------------------------------------------------------------------------
data ReadDirection
    = Forward
    | Backward
    deriving Show

--------------------------------------------------------------------------------
data StreamEventsSlice
    = StreamEventsSlice
      { streamEventsSliceResult    :: !ReadStreamResult
      , streamEventsSliceStreamId  :: !Text
      , streamEventsSliceStart     :: !Int32
      , streamEventsSliceNext      :: !Int32
      , streamEventsSliceLast      :: !Int32
      , streamEventsSliceIsEOS     :: !Bool
      , streamEventsSliceEvents    :: ![ResolvedEvent]
      , streamEventsSliceDirection :: !ReadDirection
      }
    deriving Show

--------------------------------------------------------------------------------
newStreamEventsSlice :: Text
                     -> Int32
                     -> ReadDirection
                     -> ReadStreamEventsCompleted
                     -> StreamEventsSlice
newStreamEventsSlice stream_id start dir reco = ses
  where
    evts = getField $ readSECEvents reco

    ses = StreamEventsSlice
          { streamEventsSliceResult    = getField $ readSECResult reco
          , streamEventsSliceStreamId  = stream_id
          , streamEventsSliceStart     = start
          , streamEventsSliceNext      = getField $ readSECNextNumber reco
          , streamEventsSliceLast      = getField $ readSECLastNumber reco
          , streamEventsSliceIsEOS     = getField $ readSECEndOfStream reco
          , streamEventsSliceEvents    = fmap newResolvedEvent evts
          , streamEventsSliceDirection = dir
          }

--------------------------------------------------------------------------------
data AllEventsSlice
    = AllEventsSlice
      { allEventsSliceResult    :: !ReadAllResult
      , allEventsSliceFrom      :: !Position
      , allEventsSliceNext      :: !Position
      , allEventsSliceIsEOS     :: !Bool
      , allEventsSliceEvents    :: ![ResolvedEvent]
      , allEventsSliceDirection :: !ReadDirection
      }
    deriving Show

--------------------------------------------------------------------------------
newAllEventsSlice :: ReadDirection -> ReadAllEventsCompleted -> AllEventsSlice
newAllEventsSlice dir raec = aes
  where
    res      = fromMaybe RA_SUCCESS (getField $ readAECResult raec)
    evts     = fmap newResolvedEventFromBuf (getField $ readAECEvents raec)
    r_com    = getField $ readAECCommitPosition raec
    r_pre    = getField $ readAECPreparePosition raec
    r_n_com  = getField $ readAECNextCommitPosition raec
    r_n_pre  = getField $ readAECNextPreparePosition raec
    from_pos = Position r_com r_pre
    next_pos = Position r_n_com r_n_pre

    aes = AllEventsSlice
          { allEventsSliceResult    = res
          , allEventsSliceFrom      = from_pos
          , allEventsSliceNext      = next_pos
          , allEventsSliceIsEOS     = null evts
          , allEventsSliceEvents    = evts
          , allEventsSliceDirection = dir
          }

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
-- Command
--------------------------------------------------------------------------------
data Command
    = HeartbeatRequest
    | HeartbeatResponse
    | WriteEventsCmd
    | WriteEventsCompletedCmd
    | DeleteStreamCmd
    | DeleteStreamCompletedCmd
    | TransactionStartCmd
    | TransactionStartCompletedCmd
    | TransactionWriteCmd
    | TransactionWriteCompletedCmd
    | TransactionCommitCmd
    | TransactionCommitCompletedCmd
    | ReadEventCmd
    | ReadEventCompletedCmd
    | ReadStreamEventsForwardCmd
    | ReadStreamEventsForwardCompletedCmd
    | ReadStreamEventsBackwardCmd
    | ReadStreamEventsBackwardCompletedCmd
    | ReadAllEventsForwardCmd
    | ReadAllEventsForwardCompletedCmd
    | ReadAllEventsBackwardCmd
    | ReadAllEventsBackwardCompletedCmd
    -- | CreateChunk
    -- | BadRequest
    -- | NotHandled
    deriving (Eq, Show)

--------------------------------------------------------------------------------
cmdWord8 :: Command -> Word8
cmdWord8 cmd =
    case cmd of
        HeartbeatRequest                     -> 0x01
        HeartbeatResponse                    -> 0x02
        WriteEventsCmd                       -> 0x82
        WriteEventsCompletedCmd              -> 0x83
        DeleteStreamCmd                      -> 0x8A
        DeleteStreamCompletedCmd             -> 0x8B
        TransactionStartCmd                  -> 0x84
        TransactionStartCompletedCmd         -> 0x85
        TransactionWriteCmd                  -> 0x86
        TransactionWriteCompletedCmd         -> 0x87
        TransactionCommitCmd                 -> 0x88
        TransactionCommitCompletedCmd        -> 0x89
        ReadEventCmd                         -> 0xB0
        ReadEventCompletedCmd                -> 0xB1
        ReadStreamEventsForwardCmd           -> 0xB2
        ReadStreamEventsForwardCompletedCmd  -> 0xB3
        ReadStreamEventsBackwardCmd          -> 0xB4
        ReadStreamEventsBackwardCompletedCmd -> 0xB5
        ReadAllEventsForwardCmd              -> 0xB6
        ReadAllEventsForwardCompletedCmd     -> 0xB7
        ReadAllEventsBackwardCmd             -> 0xB8
        ReadAllEventsBackwardCompletedCmd    -> 0xB9
        -- CreateChunk       -> 0x12
        -- BadRequest        -> 0xF0
        -- NotHandled        -> 0xF1

--------------------------------------------------------------------------------
word8Cmd :: Word8 -> Maybe Command
word8Cmd wd =
    case wd of
        0x01 -> Just HeartbeatRequest
        0x02 -> Just HeartbeatResponse
        0x82 -> Just WriteEventsCmd
        0x83 -> Just WriteEventsCompletedCmd
        0x8A -> Just DeleteStreamCmd
        0x8B -> Just DeleteStreamCompletedCmd
        0x84 -> Just TransactionStartCmd
        0x85 -> Just TransactionStartCompletedCmd
        0x86 -> Just TransactionWriteCmd
        0x87 -> Just TransactionWriteCompletedCmd
        0x88 -> Just TransactionCommitCmd
        0x89 -> Just TransactionCommitCompletedCmd
        0xB0 -> Just ReadEventCmd
        0xB1 -> Just ReadEventCompletedCmd
        0xB2 -> Just ReadStreamEventsForwardCmd
        0xB3 -> Just ReadStreamEventsForwardCompletedCmd
        0xB4 -> Just ReadStreamEventsBackwardCmd
        0xB5 -> Just ReadStreamEventsBackwardCompletedCmd
        0xB6 -> Just ReadAllEventsForwardCmd
        0xB7 -> Just ReadAllEventsForwardCompletedCmd
        0xB8 -> Just ReadAllEventsBackwardCmd
        0xB9 -> Just ReadAllEventsBackwardCompletedCmd
        -- 0x12 -> Just CreateChunk
        -- 0xF0 -> Just BadRequest
        -- 0xF1 -> Just NotHandled
        _    -> Nothing

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
data Package
    = Package
      { packageCmd         :: !Command
      , packageFlag        :: !Flag
      , packageCorrelation :: !UUID
      , packageData        :: !ByteString
      }
    deriving Show

--------------------------------------------------------------------------------
data Msg
    = Reconnect
    | RecvPackage Package
    | RegisterOperation Operation
    | Notice String
    | Tick

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
