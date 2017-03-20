{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.ConnectionManager
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.ConnectionManager
  ( connectionManager ) where

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
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Discovery
import Database.EventStore.Internal.EndPoint
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Stage
  = Init
  | Connecting
  | Connected CurrentConnection
  | Closed

--------------------------------------------------------------------------------
data StartConnect = StartConnect

--------------------------------------------------------------------------------
data EstablishConnection = EstablishConnection EndPoint

--------------------------------------------------------------------------------
data ConnectionEstablished = ConnectionEstablished CurrentConnection

--------------------------------------------------------------------------------
data PackageArrived = PackageArrived Package

--------------------------------------------------------------------------------
data CurrentConnection =
  CurrentConnection { _endpoint  :: EndPoint
                    , _connectId :: UUID
                    , _connect   :: Connection
                    }

--------------------------------------------------------------------------------
data Internal =
  Internal { _setts    :: Settings
           , _disc     :: Discovery
           , _logger   :: Logger
           , _logMgr   :: LogManager
           , _mainBus  :: Bus
           , _queue    :: TQueue Package
           , _stage    :: TVar Stage
           , _last     :: TVar (Maybe EndPoint)
           , _ctx      :: ConnectionContext
           , _sending  :: TVar Bool
           }

--------------------------------------------------------------------------------
connectionManager :: LogManager -> Settings -> Discovery -> Bus -> IO ()
connectionManager logMgr setts disc mainBus = do
  let logger     = getLogger "ConnectionManager" logMgr
      mkInternal = Internal setts disc logger logMgr mainBus
  internal <- mkInternal <$> newTQueueIO
                         <*> newTVarIO Init
                         <*> newTVarIO Nothing
                         <*> initConnectionContext
                         <*> newTVarIO False

  subscribe mainBus (onInit internal)
  subscribe mainBus (onSend internal)
  subscribe mainBus (onForceReconnect internal)
  subscribe mainBus (onStartConnect internal)
  subscribe mainBus (onEstablish internal)
  subscribe mainBus (onEstablished internal)
  subscribe mainBus (onArrived internal)

--------------------------------------------------------------------------------
onInit :: Internal -> SystemInit -> IO ()
onInit Internal{..} _ = do
  publish _mainBus StartConnect
  publish _mainBus (Initialized ConnectionManager)

--------------------------------------------------------------------------------
onStartConnect :: Internal -> StartConnect -> IO ()
onStartConnect i@Internal{..} _ = do
  atomically $ writeTVar _stage Connecting
  discover i

--------------------------------------------------------------------------------
discover :: Internal -> IO ()
discover Internal{..} = do
  _ <- fork $ do
    old     <- readTVarIO _last
    outcome <- tryAny $ runDiscovery _disc old
    case outcome of
      Left e -> do
        let msg = "Failed to resolve TCP end point to which to connect: {}"
        logFormat _logger Error msg (Only $ Shown e)
        publish _mainBus (FatalException e)
      Right opt ->
        case opt of
          Nothing -> do
            let msg = "Failed to resolve TCP end point to which to connect"
            logMsg _logger Error msg
            publish _mainBus FatalCondition
          Just endpoint -> do
            publish _mainBus (EstablishConnection endpoint)

  return ()

--------------------------------------------------------------------------------
onEstablish :: Internal -> EstablishConnection -> IO ()
onEstablish Internal{..} (EstablishConnection ept) = do
  _ <- fork $ do
    let params = ConnectionParams host port (s_ssl _setts) Nothing
    outcome <- tryAny $ connectTo _ctx params
    case outcome of
      Left _ -> return ()
      Right conn -> do
        cid <- nextRandom
        let cur = CurrentConnection ept cid conn
        atomically $ writeTVar _last (Just ept)
        publish _mainBus (ConnectionEstablished cur)

  return ()
  where
    host = endPointIp ept
    port = fromIntegral $ endPointPort ept

--------------------------------------------------------------------------------
onEstablished :: Internal -> ConnectionEstablished -> IO ()
onEstablished i@Internal{..} (ConnectionEstablished connection) = do
  atomically $ writeTVar _stage (Connected connection)
  logFormat _logger Info "Connection established on {}." (Only $ Shown ept)
  _ <- fork $ receiving i
  return ()
  where
    ept = _endpoint connection

--------------------------------------------------------------------------------
onSend :: Internal -> TcpSend -> IO ()
onSend i@Internal{..} (TcpSend pkg) = do
  stage <- readTVarIO _stage
  case stage of
    Init ->
      logMsg _logger Warn "Could not send Package at this stage. discarding."
    Closed ->
      logMsg _logger Warn "Connection already closed. Package discarded."
    Connecting ->  atomically $ writeTQueue _queue pkg
    Connected _ -> do
      canSend <- atomically $ do
        stage2  <- readTVar _stage
        case stage2 of
          Connecting -> False <$ writeTQueue _queue pkg
          Connected _ -> do
            running <- readTVar _sending
            writeTQueue _queue pkg
            when (not running) $ writeTVar _sending True
            return $ not running
          _ -> return False

      when canSend $ do
        _ <- fork $ sending i
        return ()

--------------------------------------------------------------------------------
onForceReconnect :: Internal -> ForceReconnect -> IO ()
onForceReconnect Internal{..} (ForceReconnect node) = do
  let ept = if isJust $ s_ssl _setts
            then let Just pt = secureEndPoint node in pt
            else tcpEndPoint node

  publish _mainBus (EstablishConnection ept)

--------------------------------------------------------------------------------
onArrived :: Internal -> PackageArrived -> IO ()
onArrived Internal{..} (PackageArrived pkg) =
  if packageCmd pkg == 0x01
  then do
    let resp = heartbeatResponsePackage $ packageCorrelation pkg
    publish _mainBus (TcpSend resp)
  else publish _mainBus (PackageReceived pkg)

--------------------------------------------------------------------------------
receiving :: Internal -> IO ()
receiving i@Internal{..} = withPkg i $ \pkg -> do
  publish _mainBus (PackageArrived pkg)
  _ <- fork $ receiving i
  return ()

--------------------------------------------------------------------------------
sending :: Internal -> IO ()
sending Internal{..} = loop
  where
    loop = do
      outcome <- atomically $ do
        stage <- readTVar _stage
        case stage of
          Connected connection -> do
            msg <- tryReadTQueue _queue
            case msg of
              Just pkg -> return (Just (connection, pkg))
              Nothing  -> do
                writeTVar _sending False
                return Nothing
          _ -> do
            writeTVar _sending False
            return Nothing

      for_ outcome $ \(cur, pkg) -> do
        connectionPut (_connect cur) (runPut $ putPackage pkg)
        loop

--------------------------------------------------------------------------------
withPkg :: Internal -> (Package -> IO ()) -> IO ()
withPkg Internal{..} callback = do
  stage <- atomically $ readTVar _stage
  case stage of
    Connected cur -> do
      outcome <- tryAny $ connectionGetExact (_connect cur) 4
      case outcome of
        Left _         -> return ()
        Right headerBs -> do
          case runGet getLengthPrefix headerBs of
            Left _ -> do
              logMsg _logger Fatal "Wrong package framing. exiting..."
              publish _mainBus FatalCondition
            Right length_prefix -> do
              bs <- connectionGetExact (_connect cur) length_prefix
              case runGet getPackage bs of
                Left _ -> do
                  logMsg _logger Fatal "Error when parsing a Package. exiting..."
                  publish _mainBus FatalCondition
                Right pkg -> callback pkg
    _ -> return ()

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
