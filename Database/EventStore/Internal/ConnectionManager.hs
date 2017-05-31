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
import qualified Database.EventStore.Internal.SubscriptionManager as Subscription
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
           , _subMgr   :: Subscription.Manager
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
                         <*> Subscription.new logMgr setts

  subscribe mainBus (onInit internal)
  subscribe mainBus (onArrived internal)
  subscribe mainBus (onShutdown internal)
  subscribe mainBus (onTick internal)
  subscribe mainBus (onSubmitOperation internal)
  subscribe mainBus (onConnectionError internal)
  subscribe mainBus (onSubmitSubscription internal)

  publish mainBus (NewTimer Tick timerPeriod False)

--------------------------------------------------------------------------------
onInit :: Internal -> SystemInit -> IO ()
onInit i@Internal{..} _ = do
  startConnect i
  publish _mainBus (Initialized ConnectionManager)

--------------------------------------------------------------------------------
startConnect :: Internal -> IO ()
startConnect i@Internal{..} = do
  att <- freshAttempt
  atomically $ writeTVar _stage (Connecting att Reconnecting)
  discover i

--------------------------------------------------------------------------------
onCloseConnection :: Internal -> CloseConnection -> IO ()
onCloseConnection i@Internal{..} (CloseConnection reason) = do
  att <- freshAttempt

  case reason of
    Just e ->
      logFormat _logger Error "Connection error, reason: {}" (Only $ Shown e)
    _ ->
      logMsg _logger Error "Connection error"

  outcome <- atomically $ do
    stage <- readTVar _stage
    writeTVar _stage Closed
    case stage of
      Connected conn -> do
        writeTVar _stage (Connecting att Reconnecting)
        return $ Just conn
      _ -> return Nothing

  traverse_ (closeConnection i) outcome

--------------------------------------------------------------------------------
discover :: Internal -> IO ()
discover i@Internal{..} = do
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
          let msg = "Failed to resolve TCP end point to which to connect: {}."
          logFormat _logger Error msg (Only $ Shown e)
          onCloseConnection i (CloseConnection $ Just e)
        Right opt ->
          case opt of
            Nothing -> do
              let msg = "Failed to resolve TCP end point to which to connect."
              logMsg _logger Error msg
              onCloseConnection i (CloseConnection Nothing)
            Just endpoint -> establish i endpoint

    return ()

--------------------------------------------------------------------------------
onEstablish :: Internal -> EstablishConnection -> IO ()
onEstablish i (EstablishConnection ept) = establish i ept

--------------------------------------------------------------------------------
establish :: Internal -> EndPoint -> IO ()
establish i@Internal{..} ept = do
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
          established i ept conn

    return ()


--------------------------------------------------------------------------------
established :: Internal -> EndPoint -> Connection -> IO ()
established i@Internal{..} ept connection = do
  valid <- atomically $ do
    stage <- readTVar _stage
    case stage of
      Connecting{} -> do
        writeTVar _stage (Connected connection)
        return True
      _ -> return False

  when valid $ do
    let msg = "New connection {} established on [{}]."
    logFormat _logger Info msg ( Shown $ connectionId connection
                               , Shown ept
                               )

--------------------------------------------------------------------------------
forceReconnect :: Internal -> NodeEndPoints -> IO ()
forceReconnect i@Internal{..} node = do
  let ept = if isJust $ s_ssl _setts
            then let Just pt = secureEndPoint node in pt
            else tcpEndPoint node

  att     <- freshAttempt
  outcome <- atomically $
    readTVar _stage >>= \case
      Connected conn -> do
        if connectionEndPoint conn /= ept
          then do
            writeTVar _stage (Connecting att EndpointDiscovery)
            return (Just conn)
          else return Nothing
      _ -> return Nothing

  for_ outcome $ \conn -> do
    let msg = "Connection {}: going to reconnect to [{}], current [{}]."
    logFormat _logger Info msg ( Shown $ connectionId conn
                               , Shown ept
                               , Shown $ connectionEndPoint conn
                               )
    closeConnection i conn
    establish i ept

--------------------------------------------------------------------------------
closeConnection :: Internal -> Connection -> IO ()
closeConnection Internal{..} conn = do
  dispose conn
  let msg = "Connection {} is closed."
  logFormat _logger Info msg (Only $ Shown $ connectionId conn)

--------------------------------------------------------------------------------
arrived :: Internal -> Connection -> Package -> IO ()
arrived i@Internal{..} conn pkg =
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
               Nothing  -> Subscription.handle _subMgr pkg >>= \case
                 Nothing -> do
                   let msg = "Unknown package correlation id."
                   logMsg _logger Warn msg
                 Just dec ->
                   case dec of
                     Subscription.Handled        -> return ()
                     Subscription.Reconnect node -> forceReconnect i node
               Just dec ->
                 case dec of
                   Operation.Handled        -> return ()
                   Operation.Reconnect node -> forceReconnect i node

--------------------------------------------------------------------------------
onArrived :: Internal -> PackageArrived -> IO ()
onArrived i (PackageArrived conn pkg) = arrived i conn pkg

--------------------------------------------------------------------------------
shutdown :: Internal -> IO ()
shutdown i@Internal{..} = do
  logMsg _logger Info "Shutting down..."
  mConn <- atomically $ do
    stage <- readTVar _stage
    writeTVar _stage Closed
    case stage of
      Connected conn -> return (Just conn)
      _              -> return Nothing

  traverse_ (closeConnection i) mConn

  let cleaning = do
        outcome <- atomically $ tryReadTQueue _queue
        for_ outcome $ \pkg -> do
          when (packageCmd pkg /= heartbeatRequestCmd) $
            arrived i dumbConnection pkg

          cleaning

  cleaning
  Operation.cleanup _opMgr

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
      onCloseConnection i (CloseConnection Nothing)
      publish _mainBus SystemShutdown
    TickProceed state ->
      case state of
        Reconnecting -> do
          logMsg _logger Info "Try reconnecting..."
          discover i
        _ -> return ()
    TickConnected conn -> do
      Operation.check _opMgr conn
      Subscription.check _subMgr conn
    _ -> return ()

--------------------------------------------------------------------------------
onSubmitOperation :: Internal -> SubmitOperation -> IO ()
onSubmitOperation Internal{..} (SubmitOperation cb op) = do
  stage <- atomically $ readTVar _stage
  case stage of
    Closed ->
      logMsg _logger Warn "Connection closed but we received an operation request"
    Connected conn -> Operation.submit _opMgr op cb (Just conn)
    _              -> Operation.submit _opMgr op cb Nothing

--------------------------------------------------------------------------------
onConnectionError :: Internal -> ConnectionError -> IO ()
onConnectionError i@Internal{..} (ConnectionError conn e) = do
  let msg = "Connection error: reason {}"
  logFormat _logger Error msg (Only $ Shown e)
  closeConnection i conn

--------------------------------------------------------------------------------
onSubmitSubscription :: Internal -> SubmitSubscription -> IO ()
onSubmitSubscription Internal{..} cmd = do
  stage <- atomically $ readTVar _stage
  case stage of
    Closed ->
      logMsg _logger Warn "Connection closed but we received an operation request"
    Connected conn -> Subscription.submit _subMgr (Just conn) cmd
    _               -> Subscription.submit _subMgr Nothing cmd