{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DataKinds          #-}
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
import Control.Exception
import Data.ByteString
import Data.ByteString.Lazy (toStrict)
import Data.Int
import Data.Typeable
import Data.Word
import GHC.Generics (Generic)
import GHC.TypeLits

--------------------------------------------------------------------------------
import qualified Data.Aeson as A
import           Data.ProtocolBuffers
import           Data.Text
import           Data.Time
import           Data.UUID
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
    -- | CreateChunk
    -- | BadRequest
    -- | NotHandled
    deriving (Eq, Show)

cmdWord8 :: Command -> Word8
cmdWord8 cmd =
    case cmd of
        HeartbeatRequest         -> 0x01
        HeartbeatResponse        -> 0x02
        WriteEventsCmd           -> 0x82
        WriteEventsCompletedCmd  -> 0x83
        DeleteStreamCmd          -> 0x8A
        DeleteStreamCompletedCmd -> 0x8B
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
