{-# LANGUAGE DeriveDataTypeable #-}
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
import Data.Typeable
import Data.Word

--------------------------------------------------------------------------------
import Data.Time
import Data.UUID

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
--------------------------------------------------------------------------------
data Flag
    = None
    | Authenticated
    deriving Show

--------------------------------------------------------------------------------
data Command
    = HeartbeatRequest
    | HeartbeatResponse
    | CreateChunk
    | BadRequest
    | NotHandled
    deriving Show

--------------------------------------------------------------------------------
data Package
    = Package
      { packageCmd         :: !Command
      , packageFlag        :: !Flag
      , packageCorrelation :: !UUID
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
      }

--------------------------------------------------------------------------------
defaultSettings :: Settings
defaultSettings = Settings
                  { _heartbeatInterval = msDiffTime 750  -- 750ms
                  , _heartbeatTimeout  = msDiffTime 1500 -- 1500ms
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
        HeartbeatRequest  -> 0x01
        HeartbeatResponse -> 0x02
        CreateChunk       -> 0x12
        BadRequest        -> 0xF0
        NotHandled        -> 0xF1

--------------------------------------------------------------------------------
word8Cmd :: Word8 -> Maybe Command
word8Cmd wd =
    case wd of
        0x01 -> Just HeartbeatRequest
        0x02 -> Just HeartbeatResponse
        0x12 -> Just CreateChunk
        0xF0 -> Just BadRequest
        0xF1 -> Just NotHandled
        _    -> Nothing
