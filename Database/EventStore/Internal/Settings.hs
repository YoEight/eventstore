--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Settings
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Settings where

--------------------------------------------------------------------------------
import Network.Connection (TLSSettings)

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Prelude

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
      { s_heartbeatInterval :: NominalDiffTime
      , s_heartbeatTimeout  :: NominalDiffTime
      , s_requireMaster     :: Bool
      , s_credentials       :: Maybe Credentials
      , s_retry             :: Retry
      , s_reconnect_delay   :: NominalDiffTime
      , s_ssl               :: Maybe TLSSettings
      , s_loggerType        :: LogType
      , s_loggerFilter      :: LoggerFilter
      , s_loggerDetailed    :: Bool
      , s_operationTimeout  :: NominalDiffTime
      , s_operationRetry    :: Retry
      }

--------------------------------------------------------------------------------
-- | Default global settings.
--   s_heartbeatInterval = 750 ms
--   s_heartbeatTimeout  = 1500 ms
--   s_requireMaster     = True
--   s_credentials       = Nothing
--   s_retry             = 'atMost' 3
--   s_reconnect_delay   = 3 seconds
--   s_ssl               = Nothing
--   s_loggerType        = LogNone
--   s_loggerFilter      = LoggerLevel LevelInfo
--   s_loggerDetailed    = False
--   s_operationTimeout  = 10 seconds
--   s_operationRetry    = 'atMost' 3
defaultSettings :: Settings
defaultSettings  = Settings
                   { s_heartbeatInterval = msDiffTime 750  -- 750ms
                   , s_heartbeatTimeout  = msDiffTime 1500 -- 1500ms
                   , s_requireMaster     = True
                   , s_credentials       = Nothing
                   , s_retry             = atMost 3
                   , s_reconnect_delay   = 3
                   , s_ssl               = Nothing
                   , s_loggerType        = LogNone
                   , s_loggerFilter      = LoggerLevel LevelInfo
                   , s_loggerDetailed    = False
                   , s_operationTimeout  = 10 -- secs
                   , s_operationRetry    = atMost 3
                   }

--------------------------------------------------------------------------------
-- | Default SSL settings based on 'defaultSettings'.
defaultSSLSettings :: TLSSettings -> Settings
defaultSSLSettings tls = defaultSettings { s_ssl = Just tls }

--------------------------------------------------------------------------------
-- | Millisecond timespan
msDiffTime :: Float -> NominalDiffTime
msDiffTime n = fromRational $ toRational (n / 1000)
