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
    , connUUID
    , connClose
    , connFlush
    , connSend
    , connRecv
    , newConnection
    ) where

--------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception
import qualified Data.ByteString as B
import           Data.IORef
import           System.IO

--------------------------------------------------------------------------------
import Data.UUID
import Data.UUID.V4
import Network

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Types
import Database.EventStore.Logging

--------------------------------------------------------------------------------
data ConnectionException
    = MaxAttempt HostName Int Int -- ^ HostName Port MaxAttempt's value.
    | ClosedConnection            -- ^ Use of a close 'Connection'.
    deriving Show

--------------------------------------------------------------------------------
instance Exception ConnectionException

--------------------------------------------------------------------------------
data In a where
    Id    :: In UUID
    Close :: In ()
    Flush :: In ()
    Send  :: B.ByteString -> In ()
    Recv  :: Int -> In B.ByteString

--------------------------------------------------------------------------------
data Connection =
    Connection
    { _sem   :: QSem
    , _ref   :: IORef State
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
    sem <- newQSem 1
    ref <- newIORef Offline
    return $ Connection sem ref host port setts

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
-- | Get a new 'Handle' and a 'UUID' from the 'Connection'. It handle automatic
--   connection when the 'Connection' State is 'Offline.'
getHandle :: Connection -> IO (UUID, Handle)
getHandle Connection{..} = do
    waitQSem _sem
    s <- readIORef _ref
    r <- case s of
        Offline -> do
            ns@(Online u hdl) <- newState _setts _host _port
            writeIORef _ref ns
            return (u, hdl)
        Online u hdl -> return (u, hdl)
        Closed       -> throwIO ClosedConnection
    signalQSem _sem
    return r

--------------------------------------------------------------------------------
closeHandle :: Connection -> IO ()
closeHandle Connection{..} = do
    waitQSem _sem
    writeIORef _ref Closed
    signalQSem _sem

--------------------------------------------------------------------------------
-- | Main connection logic. It will automatically reconnect to the server when
--   a exception occured while the 'Handle' is accessed.
execute :: forall a. Connection -> In a -> IO a
execute conn i = do
    (u, h) <- getHandle conn
    go u h
  where
    go :: UUID -> Handle -> IO a
    go u h =
        let catcher e =
                case fromException e of
                    Just (MaxAttempt{}) -> throwIO e
                    _                   -> execute conn i in
        handle catcher $
            case i of
                Id       -> return u
                Close    -> closeHandle conn
                Flush    -> hFlush h
                Send b   -> B.hPut h b
                Recv siz -> B.hGet h siz

--------------------------------------------------------------------------------
newState :: Settings -> HostName -> Int -> IO State
newState sett host port =
    case s_retry sett of
        AtMost n ->
            let loop i = do
                    _settingsLog sett (Info $ Connecting i)
                    catch (connect sett host port) $ \(_ :: SomeException) -> do
                        threadDelay delay
                        if n <= i
                            then do
                                _settingsLog sett
                                             $ Error
                                             $ MaxAttemptConnectionReached i
                                throwIO $ MaxAttempt host port n
                            else loop (i + 1) in
             loop 1
        KeepRetrying ->
            let endlessly i = do
                    _settingsLog sett (Info $ Connecting i)
                    catch (connect sett host port) $ \(_ :: SomeException) ->
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
