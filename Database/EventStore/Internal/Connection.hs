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
    ( InternalConnection
    , ConnectionException(..)
    , connUUID
    , connClose
    , connSend
    , connRecv
    , connIsClosed
    , newConnection
    ) where

--------------------------------------------------------------------------------
import Text.Printf

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Serialize
import Data.UUID
import Data.UUID.V4
import Network.Connection

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
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
data InternalConnection =
    InternalConnection
    { _var    :: TMVar State
    , _last   :: IORef (Maybe EndPoint)
    , _disc   :: Discovery
    , _setts  :: Settings
    , _ctx    :: ConnectionContext
    }

--------------------------------------------------------------------------------
data State
    = Offline
    | Online !UUID !Connection
    | Closed

--------------------------------------------------------------------------------
-- | Creates a new 'InternalConnection'.
newConnection :: Settings -> Discovery -> IO InternalConnection
newConnection setts disc = do
    ctx <- initConnectionContext
    var <- newTMVarIO Offline
    ref <- newIORef Nothing
    return $ InternalConnection var ref disc setts ctx

--------------------------------------------------------------------------------
-- | Gets current 'InternalConnection' 'UUID'.
connUUID :: InternalConnection -> IO UUID
connUUID conn = execute conn Id

--------------------------------------------------------------------------------
-- | Closes the 'InternalConnection'. It will not retry to reconnect after that
--   call. it means a new 'InternalConnection' has to be created.
--   'ClosedConnection' exception will be raised if the same
--   'InternalConnection' object is used after a 'connClose' call.
connClose :: InternalConnection -> IO ()
connClose conn = execute conn Close

--------------------------------------------------------------------------------
-- | Sends 'Package' to the server.
connSend :: InternalConnection -> Package -> IO ()
connSend conn pkg = execute conn (Send pkg)

--------------------------------------------------------------------------------
-- | Asks the requested amount of bytes from the 'handle'.
connRecv :: InternalConnection -> IO Package
connRecv conn = execute conn Recv

--------------------------------------------------------------------------------
-- | Returns True if the connection is in closed state.
connIsClosed :: InternalConnection -> STM Bool
connIsClosed InternalConnection{..} = do
    r <- readTMVar _var
    case r of
        Closed -> return True
        _      -> return False

--------------------------------------------------------------------------------
-- | Main connection logic. It will automatically reconnect to the server when
--   a exception occured while the 'Handle' is accessed.
execute :: forall a. InternalConnection -> In a -> IO a
execute InternalConnection{..} i = do
    res <- atomically $ do
        s <- takeTMVar _var
        case s of
            Offline      -> return $ Right Nothing
            Online u con -> return $ Right $ Just (u, con)
            Closed       -> return $ Left ClosedConnection
    case i of
        Close ->
            case res of
                Left _ -> atomically $ putTMVar _var Closed
                Right Nothing         -> atomically $ putTMVar _var Closed
                Right (Just (_, con)) -> do
                    connectionClose con
                    atomically $ putTMVar _var Closed
        other ->
            case res of
                Left e -> do
                    atomically $ putTMVar _var Closed
                    throwIO e
                Right alt -> do
                    sres <- case alt of
                        Nothing     -> newState _setts _ctx _last _disc
                        Just (u, h) -> return $ Right $ Online u h
                    case sres of
                        Left e -> do
                            atomically $ putTMVar _var Closed
                            throwIO e
                        Right s -> do
                            atomically $ putTMVar _var s
                            let Online u con = s
                            case other of
                                Id       -> return u
                                Send pkg -> send con pkg
                                Recv     -> recv con
                                Close    -> error "impossible execute"

--------------------------------------------------------------------------------
newState :: Settings
         -> ConnectionContext
         -> IORef (Maybe EndPoint)
         -> Discovery
         -> IO (Either ConnectionException State)
newState sett ctx ref disc =
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
                                    st <- connect sett ctx host port
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
                                    st <- connect sett ctx host port
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
connect :: Settings -> ConnectionContext -> String -> Int -> IO State
connect sett ctx host port = do
    let params = ConnectionParams host (fromIntegral port) (s_ssl sett) Nothing
    conn <- connectTo ctx params
    uuid <- nextRandom
    _settingsLog sett (Info $ Connected uuid)
    return $ Online uuid conn

--------------------------------------------------------------------------------
-- Binary operations
--------------------------------------------------------------------------------
recv :: Connection -> IO Package
recv con = do
    header_bs <- connectionGet con 4
    case runGet getLengthPrefix header_bs of
        Left _              -> throwIO WrongPackageFraming
        Right length_prefix -> do
            bs <- connectionGet con length_prefix
            case runGet getPackage bs of
                Left e    -> throwIO $ PackageParsingError e
                Right pkg -> return pkg

--------------------------------------------------------------------------------
send :: Connection -> Package -> IO ()
send  con pkg = connectionPut con bs
  where
    bs = runPut $ putPackage pkg

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------
-- | Serializes a 'Package' into raw bytes.
putPackage :: Package -> Put
putPackage pkg = do
    putWord32le length_prefix
    putWord8 (cmdWord8 $ packageCmd pkg)
    putWord8 flag_word8
    putLazyByteString corr_bytes
    for_ cred_m $ \(Credentials login passw) -> do
        putWord8 $ fromIntegral $ olength login
        putByteString login
        putWord8 $ fromIntegral $ olength passw
        putByteString passw
    putByteString pack_data
  where
    pack_data     = packageData pkg
    cred_len      = maybe 0 credSize cred_m
    length_prefix = fromIntegral (olength pack_data + mandatorySize + cred_len)
    cred_m        = packageCred pkg
    flag_word8    = maybe 0x00 (const 0x01) cred_m
    corr_bytes    = toByteString $ packageCorrelation pkg

--------------------------------------------------------------------------------
credSize :: Credentials -> Int
credSize (Credentials login passw) = olength login + olength passw + 2

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
              { packageCmd         = Command cmd
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
