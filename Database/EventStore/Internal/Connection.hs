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
import           Data.Foldable (for_)
import           Data.IORef
import           Data.Typeable
import           System.IO
import           Text.Printf

--------------------------------------------------------------------------------
import Data.Serialize
import Data.UUID
import Data.UUID.V4
import Network

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Discovery
import Database.EventStore.Internal.Types
import Database.EventStore.Logging

--------------------------------------------------------------------------------
-- | Type of connection issue that can arise during the communication with the
--   server.
data ConnectionException
    = MaxAttemptConnectionReached
      -- ^ The max reconnection attempt threshold has been reached.
    | ClosedConnection
      -- ^ Use of a close 'Connection'.
    | WrongPackageFraming
      -- ^ TCP package sent by the server had a wrong framing.
    | PackageParsingError String
      -- ^ Server sent a malformed TCP package.
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception ConnectionException

--------------------------------------------------------------------------------
data In a where
    Id    :: In UUID
    Close :: In ()
    Send  :: Package -> In ()
    Recv  :: In Package

--------------------------------------------------------------------------------
-- | Internal representation of a connection with the server.
data Connection =
    Connection
    { _var   :: TMVar State
    , _last  :: IORef (Maybe EndPoint)
    , _disc  :: Discovery
    , _setts :: Settings
    }

--------------------------------------------------------------------------------
data State
    = Offline
    | Online !UUID !Handle
    | Closed

--------------------------------------------------------------------------------
-- | Creates a new 'Connection'.
newConnection :: Settings -> Discovery -> IO Connection
newConnection setts disc = do
    var <- newTMVarIO Offline
    ref <- newIORef Nothing
    return $ Connection var ref disc setts

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
-- | Sends 'Package' to the server.
connSend :: Connection -> Package -> IO ()
connSend conn pkg = execute conn (Send pkg)

--------------------------------------------------------------------------------
-- | Asks the requested amount of bytes from the 'handle'.
connRecv :: Connection -> IO Package
connRecv conn = execute conn Recv

--------------------------------------------------------------------------------
-- | Returns True if the connection is in closed state.
connIsClosed :: Connection -> STM Bool
connIsClosed Connection{..} = do
    r <- readTMVar _var
    case r of
        Closed -> return True
        _      -> return False

--------------------------------------------------------------------------------
-- | Main connection logic. It will automatically reconnect to the server when
--   a exception occured while the 'Handle' is accessed.
execute :: forall a. Connection -> In a -> IO a
execute Connection{..} i = do
    res <- atomically $ do
        s <- takeTMVar _var
        case s of
            Offline      -> return $ Right Nothing
            Online u hdl -> return $ Right $ Just (u, hdl)
            Closed       -> return $ Left ClosedConnection
    case i of
        Close ->
            case res of
                Left _ -> atomically $ putTMVar _var Closed
                Right Nothing       -> atomically $ putTMVar _var Closed
                Right (Just (_, h)) -> do
                    hClose h
                    atomically $ putTMVar _var Closed
        other ->
            case res of
                Left e -> do
                    atomically $ putTMVar _var Closed
                    throwIO e
                Right alt -> do
                    sres <- case alt of
                        Nothing     -> newState _setts _last _disc
                        Just (u, h) -> return $ Right $ Online u h
                    case sres of
                        Left e -> do
                            atomically $ putTMVar _var Closed
                            throwIO e
                        Right s -> do
                            atomically $ putTMVar _var s
                            let Online u h = s
                            case other of
                                Id       -> return u
                                Send pkg -> send h pkg
                                Recv     -> recv h
                                Close    -> error "impossible execute"

--------------------------------------------------------------------------------
newState :: Settings
         -> IORef (Maybe EndPoint)
         -> Discovery
         -> IO (Either ConnectionException State)
newState sett ref disc =
    case s_retry sett of
        AtMost n ->
            let loop i = do
                    _settingsLog sett (Info $ Connecting i)
                    let action = do
                            old     <- readIORef ref
                            ept_opt <- runDiscovery disc old
                            case ept_opt of
                                Nothing -> do
                                    threadDelay delay
                                    if n <= i
                                        then return $
                                             Left MaxAttemptConnectionReached
                                        else loop (i + 1)
                                Just ept -> do
                                    let host = endPointIp ept
                                        port = endPointPort ept
                                    st <- connect sett host port
                                    writeIORef ref (Just ept)
                                    return $ Right st
                    catch action $ \(_ :: SomeException) -> do
                        threadDelay delay
                        if n <= i
                            then return $
                                 Left MaxAttemptConnectionReached
                            else loop (i + 1) in
             loop 1
        KeepRetrying ->
            let endlessly i = do
                    _settingsLog sett (Info $ Connecting i)
                    let action = do
                            old     <- readIORef ref
                            ept_opt <- runDiscovery disc old
                            case ept_opt of
                                Nothing  -> threadDelay delay
                                            >> endlessly (i + 1)
                                Just ept -> do
                                    let host = endPointIp ept
                                        port = endPointPort ept
                                    st <- connect sett host port
                                    writeIORef ref (Just ept)
                                    return $ Right st
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

--------------------------------------------------------------------------------
-- Binary operations
--------------------------------------------------------------------------------
recv :: Handle -> IO Package
recv h = do
    header_bs <- B.hGet h 4
    case runGet getLengthPrefix header_bs of
        Left _              -> throwIO WrongPackageFraming
        Right length_prefix -> do
            bs <- B.hGet h length_prefix
            case runGet getPackage bs of
                Left e    -> throwIO $ PackageParsingError e
                Right pkg -> return pkg

--------------------------------------------------------------------------------
send :: Handle -> Package -> IO ()
send  h pkg = B.hPut h bs >> hFlush h
  where
    bs = runPut $ putPackage pkg

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------
-- | Serializes a 'Package' into raw bytes.
putPackage :: Package -> Put
putPackage pack = do
    putWord32le length_prefix
    putWord8 (packageCmd pack)
    putWord8 flag_word8
    putLazyByteString corr_bytes
    for_ cred_m $ \(Credentials login passw) -> do
        putWord8 $ fromIntegral $ B.length login
        putByteString login
        putWord8 $ fromIntegral $ B.length passw
        putByteString passw
    putByteString pack_data
  where
    pack_data     = packageData pack
    cred_len      = maybe 0 credSize cred_m
    length_prefix = fromIntegral (B.length pack_data + mandatorySize + cred_len)
    cred_m        = packageCred pack
    flag_word8    = maybe 0x00 (const 0x01) cred_m
    corr_bytes    = toByteString $ packageCorrelation pack

--------------------------------------------------------------------------------
credSize :: Credentials -> Int
credSize (Credentials login passw) = B.length login + B.length passw + 2

--------------------------------------------------------------------------------
-- | The minimun size a 'Package' should have. It's basically a command byte,
--   correlation bytes ('UUID') and a 'Flag' byte.
mandatorySize :: Int
mandatorySize = 18

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------
getLengthPrefix :: Get Int
getLengthPrefix = fmap fromIntegral getWord32le

--------------------------------------------------------------------------------
getPackage :: Get Package
getPackage = do
    cmd  <- getWord8
    flg  <- getFlag
    col  <- getUUID
    cred <- getCredentials flg
    rest <- remaining
    dta  <- getBytes rest

    let pkg = Package
              { packageCmd         = cmd
              , packageCorrelation = col
              , packageData        = dta
              , packageCred        = cred
              }

    return pkg

--------------------------------------------------------------------------------
getFlag :: Get Flag
getFlag = do
    wd <- getWord8
    case wd of
        0x00 -> return None
        0x01 -> return Authenticated
        _    -> fail $ printf "TCP: Unhandled flag value 0x%x" wd

--------------------------------------------------------------------------------
getCredEntryLength :: Get Int
getCredEntryLength = fmap fromIntegral getWord8

--------------------------------------------------------------------------------
getCredentials :: Flag -> Get (Maybe Credentials)
getCredentials None = return Nothing
getCredentials _ = do
    loginLen <- getCredEntryLength
    login    <- getBytes loginLen
    passwLen <- getCredEntryLength
    passw    <- getBytes passwLen
    return $ Just $ credentials login passw

--------------------------------------------------------------------------------
getUUID :: Get UUID
getUUID = do
    bs <- getLazyByteString 16
    case fromByteString bs of
        Just uuid -> return uuid
        _         -> fail "TCP: Wrong UUID format"
