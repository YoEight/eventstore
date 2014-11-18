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
import Data.ProtocolBuffers
import Data.Text
import Data.Time
import Data.UUID
import System.Random

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
-- EventStore Messages
--------------------------------------------------------------------------------
data Request
    = WriteEventRequest WriteEvents

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
--------------------------------------------------------------------------------
data Command
    = HeartbeatRequest
    | HeartbeatResponse
    | WriteEventsCmd
    | WriteEventsCompletedCmd
    -- | CreateChunk
    -- | BadRequest
    -- | NotHandled
    deriving Show

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
    | SendPackage Package
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

--------------------------------------------------------------------------------
-- Binary utils
--------------------------------------------------------------------------------
cmdWord8 :: Command -> Word8
cmdWord8 cmd =
    case cmd of
        HeartbeatRequest        -> 0x01
        HeartbeatResponse       -> 0x02
        WriteEventsCmd          -> 0x82
        WriteEventsCompletedCmd -> 0x83
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
        -- 0x12 -> Just CreateChunk
        -- 0xF0 -> Just BadRequest
        -- 0xF1 -> Just NotHandled
        _    -> Nothing
