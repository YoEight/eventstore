{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Connection
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Connection
    ( Connection
    , ConnectionException(..)
    , HostName
    , PortNumber
    , connUUID
    , connClose
    , connSend
    , connRecv
    , connIsClosed
    , newConnection
    ) where

--------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import qualified Data.ByteString as B
import           Data.Foldable
import           Data.Typeable

--------------------------------------------------------------------------------
import           Data.UUID
import           Data.UUID.V4
import           Network
import qualified Network.Connection as C

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Types
import Database.EventStore.Logging

--------------------------------------------------------------------------------
-- | Type of connection issue that can arise during the communication with the
--   server.
data ConnectionException
    = MaxAttempt
      -- ^ The max reconnection attempt threshold has been reached.
    | ClosedConnection
      -- ^ Use of a close 'Connection'.
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception ConnectionException

--------------------------------------------------------------------------------
data In a where
    Id    :: In UUID
    Close :: In ()
    Send  :: B.ByteString -> In ()
    Recv  :: Int -> In B.ByteString

--------------------------------------------------------------------------------
-- | Internal representation of a connection with the server.
data Connection =
    Connection
    { _var    :: TMVar State
    , _ctx    :: C.ConnectionContext
    , _params :: C.ConnectionParams
    , _setts  :: Settings
    }

--------------------------------------------------------------------------------
data State
    = Offline
    | Online !UUID !C.Connection
    | Closed

--------------------------------------------------------------------------------
-- | Creates a new 'Connection'.
newConnection :: Settings -> HostName -> PortNumber -> IO Connection
newConnection setts host port = do
    var <- newTMVarIO Offline
    ctx <- C.initConnectionContext
    let ps = regularConnectionParams host port
    return $ Connection var ctx ps setts

--------------------------------------------------------------------------------
-- | Builds a regular (meaning not using SSL/TLS) 'C.ConnectionParams'.
regularConnectionParams :: HostName -> PortNumber -> C.ConnectionParams
regularConnectionParams host port =
    C.ConnectionParams
    { C.connectionHostname  = host
    , C.connectionPort      = port
    , C.connectionUseSecure = Nothing
    , C.connectionUseSocks  = Nothing
    }

--------------------------------------------------------------------------------
-- | Gets current 'Connection' 'UUID'.
connUUID :: Connection -> IO UUID
connUUID conn = execute conn Id

--------------------------------------------------------------------------------
-- | Closes the 'Connection'. It will not retry to reconnect after that call. it
--   means a new 'Connection' has to be created. 'ClosedConnection' exception
--   will be raised if the same 'Connection' object is used after a 'connClose'
--   call.
connClose :: Connection -> IO ()
connClose conn = execute conn Close

--------------------------------------------------------------------------------
-- | Writes 'ByteString' into the buffer.
connSend :: Connection -> B.ByteString -> IO ()
connSend conn b = execute conn (Send b)

--------------------------------------------------------------------------------
-- | Asks the requested amount of bytes from the 'handle'.
connRecv :: Connection -> Int -> IO B.ByteString
connRecv conn i = execute conn (Recv i)

--------------------------------------------------------------------------------
-- | Returns True if the connection is in closed state.
connIsClosed :: Connection -> STM Bool
connIsClosed Connection{..} = do
    r <- readTMVar _var
    case r of
        Closed -> return True
        _      -> return False

--------------------------------------------------------------------------------
-- | Get a new 'C.Connection' and a 'UUID'. It handle automatic connection when
--   the 'Connection' State is 'Offline.'
getHandle :: Connection -> IO (UUID, C.Connection)
getHandle Connection{..} = do
    r <- atomically $ do
        s <- takeTMVar _var
        case s of
            Offline -> return Nothing
            Online u cn -> do
                putTMVar _var s
                return $ Just (u, cn)
            Closed -> throwSTM ClosedConnection
    case r of
        Nothing -> do
            st@(Online u cn) <- newState _setts _ctx _params
            atomically $ putTMVar _var st
            return (u, cn)
        Just t -> return t

--------------------------------------------------------------------------------
closeHandle :: Connection -> IO ()
closeHandle Connection{..} = do
    r <- atomically $ do
        s <- takeTMVar _var
        case s of
            Online _ cn -> do
                putTMVar _var Closed
                return $ Just cn
            _ -> putTMVar _var Closed >> return Nothing
    traverse_ C.connectionClose r

--------------------------------------------------------------------------------
-- | Main connection logic. It will automatically reconnect to the server when
--   a exception occured while the 'Handle' is accessed.
execute :: forall a. Connection -> In a -> IO a
execute conn i = do
    let catcher e =
            case fromException e of
                Just (_ :: ConnectionException) -> throwIO e
                _                               -> execute conn i
    (u, cn) <- getHandle conn
    handle catcher $
        case i of
            Id       -> return u
            Close    -> closeHandle conn
            Send b   -> C.connectionPut cn b
            Recv siz -> C.connectionGet cn siz

--------------------------------------------------------------------------------
newState :: Settings -> C.ConnectionContext -> C.ConnectionParams -> IO State
newState sett ctx params =
    case s_retry sett of
        AtMost n ->
            let loop i = do
                    _settingsLog sett (Info $ Connecting i)
                    catch (connect sett ctx params) $ \(_ :: SomeException) ->
                        do threadDelay delay
                           if n <= i
                               then do
                                   _settingsLog sett
                                              $ Error
                                              $ MaxAttemptConnectionReached i
                                   throwIO MaxAttempt
                               else loop (i + 1) in
             loop 1
        KeepRetrying ->
            let endlessly i = do
                    _settingsLog sett (Info $ Connecting i)
                    catch (connect sett ctx params) $ \(_ :: SomeException) ->
                        threadDelay delay >> endlessly (i + 1) in
             endlessly (1 :: Int)
  where
    delay = s_reconnect_delay_secs sett * secs

--------------------------------------------------------------------------------
secs :: Int
secs = 1000000

--------------------------------------------------------------------------------
connect :: Settings -> C.ConnectionContext -> C.ConnectionParams -> IO State
connect sett ctx ps = do
    cn <- C.connectTo ctx ps
    uuid <- nextRandom
    _settingsLog sett (Info $ Connected uuid)
    return $ Online uuid cn
