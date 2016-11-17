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
    , connForceReconnect
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
import Database.EventStore.Internal.EndPoint
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
    ForceReconnect :: NodeEndPoints -> In ()

--------------------------------------------------------------------------------
-- | Represents connection logic action to carry out.
data Status a where
    Noop :: Status ()
    WithConnection :: UUID -> Connection -> In a -> Status a
    CreateConnection :: In a -> Status a
    Errored :: ConnectionException -> Status a

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
-- | Forces reconnection on given node.
connForceReconnect :: InternalConnection -> NodeEndPoints -> IO ()
connForceReconnect conn = execute conn . ForceReconnect

--------------------------------------------------------------------------------
-- Connection Logic
--------------------------------------------------------------------------------
onlineLogic :: forall a. TMVar State
            -> UUID
            -> Connection
            -> In a
            -> STM (Status a)
onlineLogic var uuid conn input =
    let status = WithConnection uuid conn input
        state =
            case input of
                Close -> Closed
                _ -> Online uuid conn in
    status <$ putTMVar var state

--------------------------------------------------------------------------------
offlineLogic :: forall a. TMVar State -> In a -> STM (Status a)
offlineLogic var Close = Noop <$ putTMVar var Closed
offlineLogic _ other = return $ CreateConnection other

--------------------------------------------------------------------------------
closedLogic :: forall a. TMVar State -> In a -> STM (Status a)
closedLogic var input = do
    putTMVar var Closed
    case input of
        Close -> return Noop
        _ -> return $ Errored ClosedConnection

--------------------------------------------------------------------------------
connectionLogic :: forall a. TMVar State -> In a -> STM (Status a)
connectionLogic var input = do
    state <- takeTMVar var
    case state of
        Online uuid conn -> onlineLogic var uuid conn input
        Offline -> offlineLogic var input
        Closed -> closedLogic var input

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
handleInput :: forall a. UUID -> Connection -> In a -> IO a
handleInput _ conn (Send pkg) = send conn pkg
handleInput _ conn Recv = recv conn
handleInput uuid _ Id = return uuid
handleInput _ conn Close = liftIO $ connectionClose conn
handleInput _ _ ForceReconnect{} = return ()

--------------------------------------------------------------------------------
-- | Main connection logic. It will automatically reconnect to the server when
--   a exception occured while the 'Handle' is accessed.
execute :: forall a. InternalConnection -> In a -> IO a
execute iconn input = do
    res <- atomically $ connectionLogic (_var iconn) input

    case res of
        Noop -> return ()
        Errored e -> throwIO e
        WithConnection uuid conn op -> handleInput uuid conn op
        CreateConnection op -> do
            (uuid, conn) <-
                case op of
                    ForceReconnect node -> openConnection iconn (Just node)
                    _ -> openConnection iconn Nothing
            atomically $ putTMVar (_var iconn) (Online uuid conn)
            handleInput uuid conn op

--------------------------------------------------------------------------------
reachedMaxAttempt :: Retry -> Int -> Bool
reachedMaxAttempt KeepRetrying _ = False
reachedMaxAttempt (AtMost n) cur = n <= cur

--------------------------------------------------------------------------------
isSsl :: Settings -> Bool
isSsl = isJust . s_ssl

--------------------------------------------------------------------------------
openConnection :: InternalConnection
               -> Maybe NodeEndPoints
               -> IO (UUID, Connection)
openConnection InternalConnection{..} nodeM = attempt 1
  where
    delay = s_reconnect_delay_secs _setts * secs

    handleFailure trialCount = do
        threadDelay delay
        when (reachedMaxAttempt (s_retry _setts) trialCount) $ do
            atomically $ putTMVar _var Closed
            throwIO MaxAttemptConnectionReached
        attempt (trialCount + 1)

    attempt trialCount = do
        _settingsLog _setts (Info $ Connecting trialCount)
        case nodeM of
            Just node -> do
                let ept = if isSsl _setts
                          then let Just pt = secureEndPoint node
                               in pt
                          else tcpEndPoint node

                    host = endPointIp ept
                    port = endPointPort ept
                res <- tryAny $ connect _setts _ctx host port
                case res of
                    Left _ -> handleFailure trialCount
                    Right st -> st <$ writeIORef _last (Just ept)
            Nothing -> do
                old <- readIORef _last
                ept_opt <- runDiscovery _disc old
                case ept_opt of
                    Nothing -> handleFailure trialCount
                    Just ept -> do
                        let host = endPointIp ept
                            port = endPointPort ept
                        res <- tryAny $ connect _setts _ctx host port
                        case res of
                            Left _ -> handleFailure trialCount
                            Right st -> st <$ writeIORef _last (Just ept)

--------------------------------------------------------------------------------
secs :: Int
secs = 1000000

--------------------------------------------------------------------------------
connect :: Settings
        -> ConnectionContext
        -> String
        -> Int
        -> IO (UUID, Connection)
connect sett ctx host port = do
    let params = ConnectionParams host (fromIntegral port) (s_ssl sett) Nothing
    conn <- connectTo ctx params
    uuid <- nextRandom
    _settingsLog sett (Info $ Connected uuid)
    return  (uuid, conn)

--------------------------------------------------------------------------------
-- Binary operations
--------------------------------------------------------------------------------
recv :: Connection -> IO Package
recv con = do
    header_bs <- connectionGetExact con 4
    case runGet getLengthPrefix header_bs of
        Left _              -> throwIO WrongPackageFraming
        Right length_prefix -> do
            bs <- connectionGetExact con length_prefix
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
