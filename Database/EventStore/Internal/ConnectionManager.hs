{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}
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
import Data.Typeable
import Text.Printf

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.UUID
import Data.UUID.V4
import Data.Time

--------------------------------------------------------------------------------
import           Database.EventStore.Internal.Command
import           Database.EventStore.Internal.Communication
import           Database.EventStore.Internal.Connection
import           Database.EventStore.Internal.Discovery
import           Database.EventStore.Internal.EndPoint
import           Database.EventStore.Internal.Logger
import           Database.EventStore.Internal.Messaging
import qualified Database.EventStore.Internal.OperationManager as Operation
import           Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Stage
  = Init
  | Connecting Attempts ConnectingState
  | Connected Connection
  | Closed

--------------------------------------------------------------------------------
instance Show Stage where
  show Init = "Init"
  show (Connecting a s) = "Connecting: " ++ show (a, s)
  show Connected{}      = "Connected"
  show Closed           = "Closed"

--------------------------------------------------------------------------------
data ConnectingState
  = Reconnecting
  | EndpointDiscovery
  | ConnectionEstablishing
  deriving Show

--------------------------------------------------------------------------------
data Attempts =
  Attempts { attemptCount     :: !Int
           , attemptLastStart :: !UTCTime
           } deriving Show

--------------------------------------------------------------------------------
freshAttempt :: IO Attempts
freshAttempt = Attempts 1 <$> getCurrentTime

--------------------------------------------------------------------------------
data UnableToConnect = UnableToConnect deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception UnableToConnect

--------------------------------------------------------------------------------
data StartConnect = StartConnect deriving Typeable

--------------------------------------------------------------------------------
data EstablishConnection = EstablishConnection EndPoint deriving Typeable

--------------------------------------------------------------------------------
data ConnectionEstablished =
  ConnectionEstablished EndPoint Connection deriving Typeable

--------------------------------------------------------------------------------
data CloseConnection = CloseConnection (Maybe SomeException) deriving Typeable

--------------------------------------------------------------------------------
data Tick = Tick deriving Typeable

--------------------------------------------------------------------------------
timerPeriod :: Duration
timerPeriod = msDuration 200

--------------------------------------------------------------------------------
data Internal =
  Internal { _setts    :: Settings
           , _disc     :: Discovery
           , _logger   :: Logger
           , _logMgr   :: LogManager
           , _mainBus  :: Hub
           , _builder  :: ConnectionBuilder
           , _queue    :: TQueue Package
           , _stage    :: TVar Stage
           , _last     :: TVar (Maybe EndPoint)
           , _sending  :: TVar Bool
           , _opMgr    :: Operation.Manager
           }

--------------------------------------------------------------------------------
connectionManager :: LogManager
                  -> Settings
                  -> ConnectionBuilder
                  -> Discovery
                  -> Hub
                  -> IO ()
connectionManager logMgr setts builder disc mainBus = do
  let logger     = getLogger "ConnectionManager" logMgr
      mkInternal = Internal setts disc logger logMgr mainBus builder
  internal <- mkInternal <$> newTQueueIO
                         <*> newTVarIO Init
                         <*> newTVarIO Nothing
                         <*> newTVarIO False
                         <*> Operation.new logMgr setts

  subscribe mainBus (onInit internal)
  subscribe mainBus (onStartConnect internal)
  subscribe mainBus (onEstablish internal)
  subscribe mainBus (onEstablished internal)
  subscribe mainBus (onArrived internal)
  subscribe mainBus (onShutdown internal)
  subscribe mainBus (onCloseConnection internal)
  subscribe mainBus (onTick internal)
  subscribe mainBus (onSubmitOperation internal)

  publish mainBus (NewTimer Tick timerPeriod False)

--------------------------------------------------------------------------------
onInit :: Internal -> SystemInit -> IO ()
onInit Internal{..} _ = do
  publish _mainBus StartConnect
  publish _mainBus (Initialized ConnectionManager)

--------------------------------------------------------------------------------
onStartConnect :: Internal -> StartConnect -> IO ()
onStartConnect i@Internal{..} _ = do
  att <- freshAttempt
  atomically $ writeTVar _stage (Connecting att Reconnecting)
  discover i

--------------------------------------------------------------------------------
onCloseConnection :: Internal -> CloseConnection -> IO ()
onCloseConnection Internal{..} (CloseConnection reason) = do
  att <- freshAttempt

  case reason of
    Just e ->
      logFormat _logger Error "Connection closed, reason: {}" (Only $ Shown e)
    _ ->
      logMsg _logger Error "Connection closed"

  outcome <- atomically $ do
    stage <- readTVar _stage
    writeTVar _stage Closed
    case stage of
      Connected conn -> do
        writeTVar _stage (Connecting att Reconnecting)
        return $ Just conn
      _ -> return Nothing

  traverse_ dispose outcome

--------------------------------------------------------------------------------
discover :: Internal -> IO ()
discover Internal{..} = do
  canProceed <- atomically $ do
    stage <- readTVar _stage
    case stage of
      Connecting att state ->
        case state of
          Reconnecting -> do
            writeTVar _stage (Connecting att EndpointDiscovery)
            return True
          _ -> return False
      _ -> return False

  when canProceed $ do
    _ <- fork $ do
      old     <- readTVarIO _last
      outcome <- tryAny $ runDiscovery _disc old
      case outcome of
        Left e -> do
          let msg = "Failed to resolve TCP end point to which to connect: {}"
          logFormat _logger Error msg (Only $ Shown e)
          publish _mainBus (CloseConnection $ Just e)
        Right opt ->
          case opt of
            Nothing -> do
              let msg = "Failed to resolve TCP end point to which to connect"
              logMsg _logger Error msg
              publish _mainBus (CloseConnection Nothing)
            Just endpoint -> do
              publish _mainBus (EstablishConnection endpoint)

    return ()

--------------------------------------------------------------------------------
onEstablish :: Internal -> EstablishConnection -> IO ()
onEstablish Internal{..} (EstablishConnection ept) = do
  canProceed <- atomically $ do
    stage <- readTVar _stage
    case stage of
      Connecting att state ->
        case state of
          EndpointDiscovery -> do
            writeTVar _stage (Connecting att ConnectionEstablishing)
            return True
          _ -> return False
      _ -> return False

  when canProceed $ do
    _ <- fork $ do
      outcome <- tryAny $ connect _builder ept
      case outcome of
        Left _ -> atomically $ do
          stage <- readTVar _stage
          case stage of
            Connecting att _ -> writeTVar _stage (Connecting att Reconnecting)
            _                -> return ()
        Right conn -> do
          cid <- nextRandom
          atomically $ writeTVar _last (Just ept)
          publish _mainBus (ConnectionEstablished ept conn)

    return ()

--------------------------------------------------------------------------------
onEstablished :: Internal -> ConnectionEstablished -> IO ()
onEstablished i@Internal{..} (ConnectionEstablished ept connection) = do
  valid <- atomically $ do
    stage <- readTVar _stage
    case stage of
      Connecting{} -> do
        writeTVar _stage (Connected connection)
        return True
      _ -> return False

  when valid $
    logFormat _logger Info "Connection established on {}." (Only $ Shown ept)

--------------------------------------------------------------------------------
forceReconnect :: Internal -> NodeEndPoints -> IO ()
forceReconnect Internal{..} node = do
  let ept = if isJust $ s_ssl _setts
            then let Just pt = secureEndPoint node in pt
            else tcpEndPoint node

  att <- freshAttempt
  atomically $ writeTVar _stage (Connecting att EndpointDiscovery)
  publish _mainBus (EstablishConnection ept)

--------------------------------------------------------------------------------
onArrived :: Internal -> PackageArrived -> IO ()
onArrived i@Internal{..} (PackageArrived conn pkg) =
  case packageCmd pkg of
    cmd | cmd == heartbeatRequestCmd
          -> let respPkg = heartbeatResponsePackage $ packageCorrelation pkg in
             enqueuePackage conn respPkg
        | cmd == badRequestCmd && packageCorrelation pkg == nil
          -> do let reason = packageDataAsText pkg
                    msg = "Connection-wide BadRequest received. \
                          \ Too dangerous to continue: " <> fold reason
                shutdown i
                publish _mainBus (FatalCondition msg)
        | otherwise
          -> Operation.handle _opMgr pkg >>= \case
               Nothing  -> return ()
               Just dec ->
                 case dec of
                   Operation.Handled        -> return ()
                   Operation.Reconnect node -> forceReconnect i node

--------------------------------------------------------------------------------
shutdown :: Internal -> IO ()
shutdown Internal{..} = do
  logMsg _logger Info "Shutting down..."
  atomically $ writeTVar _stage Closed

  let cleaning = do
        outcome <- atomically $ tryReadTQueue _queue
        for_ outcome $ \pkg -> do
          when (packageCmd pkg /= heartbeatRequestCmd) $
            publish _mainBus (PackageReceived pkg)

          cleaning

  cleaning

--------------------------------------------------------------------------------
onShutdown :: Internal -> SystemShutdown -> IO ()
onShutdown i@Internal{..} _ = do
  done <- atomically $
    readTVar _stage >>= \case
      Closed -> return True
      _      -> return False

  unless done (shutdown i)
  publish _mainBus (ServiceTerminated ConnectionManager)

--------------------------------------------------------------------------------
data TickOutcome
  = TickClose
  | TickNoop
  | TickProceed ConnectingState
  | TickConnected Connection

--------------------------------------------------------------------------------
onTick :: Internal -> Tick -> IO ()
onTick i@Internal{..} _ = do
  now <- getCurrentTime

  outcome <- atomically $ do
    stage <- readTVar _stage
    case stage of
      Connecting att Reconnecting -> do
        let count   = attemptCount att
            started = attemptLastStart att
            delay   = s_reconnect_delay _setts

        if diffUTCTime now started >= delay
          then
            let proceeding = do
                  let newAtt = att { attemptCount     = count + 1
                                   , attemptLastStart = now }
                  writeTVar _stage (Connecting newAtt Reconnecting)
                  return $ TickProceed Reconnecting in
            case s_retry _setts of
              AtMost maxTrial
                | count <= maxTrial -> proceeding
                | otherwise         -> return TickClose
              KeepRetrying -> proceeding
          else return TickNoop
      Connected cur -> return (TickConnected cur)
      _             -> return TickNoop

  case outcome of
    TickClose -> do
      publish _mainBus CloseConnection
      publish _mainBus SystemShutdown
    TickProceed state ->
      case state of
        Reconnecting -> do
          logMsg _logger Info "Try reconnecting..."
          discover i
        _ -> return ()
    TickConnected conn -> do
      Operation.check _opMgr conn
    _ -> return ()

--------------------------------------------------------------------------------
onSubmitOperation :: Internal -> SubmitOperation -> IO ()
onSubmitOperation Internal{..} (SubmitOperation cb op) = do
  stage <- atomically $ readTVar _stage
  case stage of
    Closed ->
      logMsg _logger Warn "Connection closed but we received an operation request"
    _ -> return ()
