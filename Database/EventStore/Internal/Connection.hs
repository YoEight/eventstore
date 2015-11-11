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
    , connUUID
    , connClose
    , connFlush
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
import           System.IO

--------------------------------------------------------------------------------
import Data.UUID
import Data.UUID.V4
import Network

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Types
import Database.EventStore.Logging

--------------------------------------------------------------------------------
-- | Type of connection issue that can arise during the communication with the
--   server.
data ConnectionException
    = MaxAttempt HostName Int Int
      -- ^ The max reconnection attempt threshold has been reached. Holds a
      --   'HostName', the port used and the given threshold.
    | ClosedConnection
      -- ^ Use of a close 'Connection'.
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception ConnectionException

--------------------------------------------------------------------------------
data In a where
    Id       :: In UUID
    Close    :: In ()
    Flush    :: In ()
    Send     :: B.ByteString -> In ()
    Recv     :: Int -> In B.ByteString

--------------------------------------------------------------------------------
-- | Internal representation of a connection with the server.
data Connection =
    Connection
    { _var   :: TMVar State
    , _host  :: HostName
    , _port  :: Int
    , _setts :: Settings
    }

--------------------------------------------------------------------------------
data State
    = Offline
    | Online !UUID !Handle
    | Closed

--------------------------------------------------------------------------------
-- | Creates a new 'Connection'.
newConnection :: Settings -> HostName -> Int -> IO Connection
newConnection setts host port = do
    var <- newTMVarIO Offline
    return $ Connection var host port setts

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
-- | Flushes the current buffer.
connFlush :: Connection -> IO ()
connFlush conn = execute conn Flush

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
-- | Get a new 'Handle' and a 'UUID' from the 'Connection'. It handle automatic
--   connection when the 'Connection' State is 'Offline.'
getHandle :: Connection -> IO (UUID, Handle)
getHandle Connection{..} = do
    r <- atomically $ do
        s <- takeTMVar _var
        case s of
            Offline -> return Nothing
            Online u hdl -> do
                putTMVar _var s
                return $ Just (u, hdl)
            Closed -> putTMVar _var s >> throwSTM ClosedConnection
    case r of
        Nothing -> do
            res <- newState _setts _host _port
            case res of
                Left e -> do
                    let AtMost n = s_retry _setts
                    _settingsLog _setts $ Error $ MaxAttemptConnectionReached n
                    atomically $ putTMVar _var Closed
                    throwIO e
                Right st -> do
                    let Online u hdl = st
                    atomically $ putTMVar _var st
                    return (u, hdl)
        Just t -> return t

--------------------------------------------------------------------------------
closeHandle :: Connection -> IO ()
closeHandle Connection{..} = do
    r <- atomically $ do
        s <- takeTMVar _var
        case s of
            Online _ hdl -> do
                putTMVar _var Closed
                return $ Just hdl
            _ -> putTMVar _var Closed >> return Nothing
    traverse_ hClose r

--------------------------------------------------------------------------------
-- | Main connection logic. It will automatically reconnect to the server when
--   a exception occured while the 'Handle' is accessed.
execute :: forall a. Connection -> In a -> IO a
execute conn i = do
    let catcher e =
            case fromException e of
                Just (_ :: ConnectionException) -> throwIO e
                _                               -> execute conn i
    (u, h) <- getHandle conn
    handle catcher $
        case i of
            Id       -> return u
            Close    -> closeHandle conn
            Flush    -> hFlush h
            Send b   -> B.hPut h b
            Recv siz -> B.hGet h siz

--------------------------------------------------------------------------------
newState :: Settings -> HostName -> Int -> IO (Either ConnectionException State)
newState sett host port =
    case s_retry sett of
        AtMost n ->
            let loop i = do
                    _settingsLog sett (Info $ Connecting i)
                    let action = fmap Right $ connect sett host port
                    catch action $ \(_ :: SomeException) -> do
                        threadDelay delay
                        if n <= i
                            then return $ Left $  MaxAttempt host port n
                            else loop (i + 1) in
             loop 1
        KeepRetrying ->
            let endlessly i = do
                    _settingsLog sett (Info $ Connecting i)
                    let action = fmap Right $ connect sett host port
                    catch action $ \(_ :: SomeException) ->
                        threadDelay delay >> endlessly (i + 1) in
             endlessly (1 :: Int)
  where
    delay = s_reconnect_delay_secs sett * secs

--------------------------------------------------------------------------------
secs :: Int
secs = 1000000

--------------------------------------------------------------------------------
connect :: Settings -> HostName -> Int -> IO State
connect sett host port = do
    hdl <- connectTo host (PortNumber $ fromIntegral port)
    hSetBuffering hdl NoBuffering
    uuid <- nextRandom
    regularConnection sett hdl uuid

--------------------------------------------------------------------------------
regularConnection :: Settings -> Handle -> UUID -> IO State
regularConnection sett h uuid = do
    _settingsLog sett (Info $ Connected uuid)
    return $ Online uuid h
