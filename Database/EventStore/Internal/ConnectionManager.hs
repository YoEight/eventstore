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
import           Database.EventStore.Internal.Callback
import           Database.EventStore.Internal.Command
import           Database.EventStore.Internal.Communication
import           Database.EventStore.Internal.Connection
import           Database.EventStore.Internal.Discovery
import           Database.EventStore.Internal.EndPoint
import           Database.EventStore.Internal.Logger
import           Database.EventStore.Internal.Messaging
import           Database.EventStore.Internal.Operation
import qualified Database.EventStore.Internal.OperationManager as Operation
import           Database.EventStore.Internal.Stopwatch
import qualified Database.EventStore.Internal.SubscriptionManager as Subscription
import           Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Stage
  = Init
  | Connecting Attempts ConnectingState
  | Connected
  | Closed

--------------------------------------------------------------------------------
instance Show Stage where
  show Init = "Init"
  show (Connecting a s) = "Connecting: " ++ show (a, s)
  show Connected        = "Connected"
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
           , attemptLastStart :: !NominalDiffTime
           } deriving Show

--------------------------------------------------------------------------------
freshAttempt :: Stopwatch -> IO Attempts
freshAttempt = fmap (Attempts 1) . stopwatchElapsed

--------------------------------------------------------------------------------
data UnableToConnect = UnableToConnect deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception UnableToConnect

--------------------------------------------------------------------------------
data ConnectionMaxAttemptReached = ConnectionMaxAttemptReached
  deriving Typeable

--------------------------------------------------------------------------------
instance Show ConnectionMaxAttemptReached where
  show _ = "Reconnection limit reached."

--------------------------------------------------------------------------------
instance Exception ConnectionMaxAttemptReached

--------------------------------------------------------------------------------
data StartConnect = StartConnect deriving Typeable

--------------------------------------------------------------------------------
data EstablishConnection = EstablishConnection EndPoint deriving Typeable

--------------------------------------------------------------------------------
data Tick = Tick deriving Typeable

--------------------------------------------------------------------------------
timerPeriod :: Duration
timerPeriod = msDuration 200

--------------------------------------------------------------------------------
data Internal =
  Internal { _setts     :: Settings
           , _disc      :: Discovery
           , _logger    :: Logger
           , _logMgr    :: LogManager
           , _mainBus   :: Hub
           , _builder   :: ConnectionBuilder
           , _stage     :: MVar Stage
           , _last      :: IORef (Maybe EndPoint)
           , _sending   :: TVar Bool
           , _opMgr     :: Operation.Manager
           , _subMgr    :: Subscription.Manager
           , _conn      :: MVar Connection
           , _stopwatch :: Stopwatch
           , _lastCheck :: IORef NominalDiffTime
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

  stopwatch    <- newStopwatch
  timeoutCheck <- stopwatchElapsed stopwatch
  internal <- mkInternal <$> newMVar Init
                         <*> newIORef Nothing
                         <*> newTVarIO False
                         <*> Operation.new logMgr setts
                         <*> Subscription.new logMgr setts
                         <*> newEmptyMVar
                         <*> return stopwatch
                         <*> newIORef timeoutCheck

  subscribe mainBus (onInit internal)
  subscribe mainBus (onEstablish internal)
  subscribe mainBus (onEstablished internal)
  subscribe mainBus (onArrived internal)
  subscribe mainBus (onSubmitOperation internal)
  subscribe mainBus (onConnectionError internal)
  subscribe mainBus (onConnectionClosed internal)
  subscribe mainBus (onShutdown internal)
  subscribe mainBus (onTick internal)
  subscribe mainBus (onSendPackage internal)

  publish mainBus (NewTimer Tick timerPeriod False)

--------------------------------------------------------------------------------
onInit :: Internal -> SystemInit -> IO ()
onInit i@Internal{..} _ = do
  startConnect i
  publish _mainBus (Initialized ConnectionManager)

--------------------------------------------------------------------------------
startConnect :: Internal -> IO ()
startConnect i@Internal{..} =
  takeMVar _stage >>= \case
    Init -> do
      atts <- freshAttempt _stopwatch
      putMVar _stage (Connecting atts Reconnecting)
      discover i
    s -> putMVar _stage s

--------------------------------------------------------------------------------
discover :: Internal -> IO ()
discover Internal{..} =
  takeMVar _stage >>= \case
    Connecting att p ->
      case p of
        Reconnecting{} -> do
          putMVar _stage (Connecting att EndpointDiscovery)
          old <- readIORef _last
          _   <- fork $
              tryAny (runDiscovery _disc old) >>= \case
                Left e    -> do
                  conn <- readMVar _conn
                  let msg = "Failed to resolve TCP endpoint to which to \
                            \connect {}."
                  logFormat _logger Warn msg (Only $ Shown e)
                  publish _mainBus (ConnectionError conn e)
                Right opt ->
                  case opt of
                    Nothing -> do
                      let msg = "Failed to resolve TCP endpoint to which to \
                                \connect."
                      logMsg _logger Warn msg
                    Just ept -> publish _mainBus (EstablishConnection ept)
          return ()
        _ -> putMVar _stage (Connecting att p)
    s -> putMVar _stage s

--------------------------------------------------------------------------------
establish :: Internal -> EndPoint -> IO ()
establish Internal{..} ept = do
  logFormat _logger Debug "Establish tcp connection on [{}]" (Only $ Shown ept)
  takeMVar _stage >>= \case
    Connecting att s ->
      case s of
        EndpointDiscovery -> do
          putMVar _stage (Connecting att ConnectionEstablishing)
          conn <- connect _builder ept
          putMVar _conn conn
        state -> putMVar _stage (Connecting att state)
    stage -> putMVar _stage stage

--------------------------------------------------------------------------------
established :: Internal -> Connection -> IO ()
established Internal{..} conn =
  takeMVar _stage >>= \case
    stage@Connecting{} -> do
      mConn <- tryReadMVar _conn
      if all ((== cid) . connectionId) mConn
        then do
          let msg = "TCP connection established: id: {}, endpoint {}."
          logFormat _logger Debug msg (Shown cid, Shown ept)
          putMVar _stage Connected
        else putMVar _stage stage
    stage -> putMVar _stage stage
  where
    cid = connectionId conn
    ept = connectionEndPoint conn

--------------------------------------------------------------------------------
onEstablished :: Internal -> ConnectionEstablished -> IO ()
onEstablished i (ConnectionEstablished conn) = established i conn

--------------------------------------------------------------------------------
closeConnection :: Exception e => Internal -> e -> IO ()
closeConnection self@Internal{..} cause = do
  logFormat _logger Debug "CloseConnection: {}" (Only $ Shown cause)
  _ <- swapMVar _stage Closed
  Operation.cleanup _opMgr
  Subscription.cleanup _subMgr
  traverse_ (closeTcpConnection self cause) =<< tryTakeMVar _conn
  logFormat _logger Info "CloseConnection: connection cleanup done for [{}]" (Only $ Shown cause)
  publish _mainBus (FatalException cause)

--------------------------------------------------------------------------------
closeTcpConnection :: Exception e => Internal -> e -> Connection -> IO ()
closeTcpConnection Internal{..} cause conn = do
  let cid = connectionId conn
  logFormat _logger Debug "CloseTcpConnection: connection [{}]. Cause: {}"
    (Shown cid, Shown cause)

  dispose conn

  logFormat _logger Debug "CloseTcpConnection: connection [{}] disposed."
    (Only $ Shown cid)

  readMVar _stage >>= \case
    Closed -> return ()
    stage  -> do
      -- TODO - Purge subscriptions here.
      att <-
        case stage of
          Connecting old _ -> return old
          _                -> freshAttempt _stopwatch

      _   <- swapMVar _stage (Connecting att Reconnecting)
      return ()

--------------------------------------------------------------------------------
data ForceReconnect = ForceReconnect EndPoint deriving (Typeable, Show)

--------------------------------------------------------------------------------
instance Exception ForceReconnect

--------------------------------------------------------------------------------
forceReconnect :: Internal -> NodeEndPoints -> IO Bool
forceReconnect self@Internal{..} node = do
  let ept = if isJust $ s_ssl _setts
            then let Just pt = secureEndPoint node in pt
            else tcpEndPoint node

  conn <- readMVar _conn
  if connectionEndPoint conn /= ept
    then do
      closeTcpConnection self (ForceReconnect ept) conn
      att <- freshAttempt _stopwatch
      putMVar _stage (Connecting att EndpointDiscovery)
      let msg = "Connection {}: going to reconnect to [{}], current [{}]."
      logFormat _logger Info msg ( Shown $ connectionId conn
                                 , Shown ept
                                 , Shown $ connectionEndPoint conn
                                 )
      establish self ept
      return True
  else return False

--------------------------------------------------------------------------------
onEstablish :: Internal -> EstablishConnection -> IO ()
onEstablish i (EstablishConnection ept) = establish i ept

--------------------------------------------------------------------------------
onTick :: Internal -> Tick -> IO ()
onTick self@Internal{..} _ =
  takeMVar _stage >>= \case
    stage@(Connecting Attempts{..} s) -> do
      elapsed <- stopwatchElapsed _stopwatch
      case s of
        Reconnecting
          | elapsed - attemptLastStart >= s_reconnect_delay _setts
            -> do
              let retries = attemptCount + 1
                  att     = Attempts retries elapsed
              putMVar _stage (Connecting att Reconnecting)
              case s_retry _setts of
                AtMost n
                  | attemptCount <= n -> retryConnection attemptCount
                  | otherwise -> maxAttemptReached
                KeepRetrying -> retryConnection attemptCount
          | otherwise -> putMVar _stage stage
        _ -> putMVar _stage stage
    stage@Connected -> do
      elapsed           <- stopwatchElapsed _stopwatch
      timeoutCheckStart <- readIORef _lastCheck

      when (elapsed - timeoutCheckStart >= s_operationTimeout _setts) $ do
        conn <- readMVar _conn
        Operation.check _opMgr conn
        Subscription.check _subMgr conn

      putMVar _stage stage
    stage -> putMVar _stage stage
  where
    maxAttemptReached = do
      closeConnection self ConnectionMaxAttemptReached
      publish _mainBus (FatalException ConnectionMaxAttemptReached)

    retryConnection i = do
      logFormat _logger Debug "Checking reconnection... (attempt {})" (Only i)
      discover self

--------------------------------------------------------------------------------
onArrived :: Internal -> PackageArrived -> IO ()
onArrived self@Internal{..} (PackageArrived conn pkg@Package{..}) = do
  stage <- takeMVar _stage

  if validStage stage
    then do
      knownConn <- readMVar _conn
      if knownConn == conn
        then do
          logFormat _logger Debug "Package received:  {}" (Only $ Shown pkg)
          handlePackage stage
        else do
          logFormat _logger Debug "Package IGNORED: {}" (Only $ Shown pkg)
          putMVar _stage stage
    else do
      logFormat _logger Debug "Package IGNORED: {}" (Only $ Shown pkg)
      putMVar _stage stage
  where
    validStage Connecting{} = True
    validStage Connected    = True
    validStage _            = False

    heartbeatResponse = heartbeatResponsePackage packageCorrelation

    handlePackage stage
      | packageCmd == heartbeatRequestCmd = do
        enqueuePackage conn heartbeatResponse
        putMVar _stage stage
      | otherwise =
        Operation.handle _opMgr pkg >>= \case
          Nothing -> do
            logFormat _logger Warn "Package not handled: {}" (Only $ Shown pkg)
            putMVar _stage stage
          Just decision ->
            case decision of
              Operation.Handled        -> putMVar _stage stage
              Operation.Reconnect node -> do
                done <- forceReconnect self node
                unless done $
                  putMVar _stage stage

--------------------------------------------------------------------------------
onConnectionError :: Internal -> ConnectionError -> IO ()
onConnectionError i@Internal{..} (ConnectionError conn e) = do
  sameConnection <- maybe False (== conn) <$> tryReadMVar _conn
  closed         <- isClosed <$> readMVar _stage

  when (sameConnection && not closed) $ do
    let msg = "TCP connection [{}] error. Cause: [{}]"
    logFormat _logger Error msg (Shown cid, Shown e)
    closeConnection i e
  where
    cid = connectionId conn

    isClosed Closed = True
    isClosed _      = False

--------------------------------------------------------------------------------
onConnectionClosed :: Internal -> ConnectionClosed -> IO ()
onConnectionClosed self@Internal{..} (ConnectionClosed conn cause) = do
  sameConnection <- maybe False (== conn) <$> tryReadMVar _conn
  closed         <- isClosed <$> readMVar _stage

  when (sameConnection && not closed) $ do
    _ <- takeMVar _conn
    closeTcpConnection self cause conn
  where
    isClosed Closed = True
    isClosed _      = False

--------------------------------------------------------------------------------
onShutdown :: Internal -> SystemShutdown -> IO ()
onShutdown Internal{..} _ = do
  logMsg _logger Debug "Shutting down..."
  _ <- swapMVar _stage Closed
  Operation.cleanup _opMgr
  Subscription.cleanup _subMgr
  traverse_ dispose =<< tryTakeMVar _conn
  logMsg _logger Debug "Shutdown properly."
  publish _mainBus (ServiceTerminated ConnectionManager)

--------------------------------------------------------------------------------
onSubmitOperation :: Internal -> SubmitOperation -> IO ()
onSubmitOperation Internal{..} (SubmitOperation callback op) =
  readMVar _stage >>= \case
    Closed -> reject callback Aborted
    _      -> Operation.submit _opMgr op callback =<< tryReadMVar _conn

--------------------------------------------------------------------------------
onSendPackage :: Internal -> SendPackage -> IO ()
onSendPackage Internal{..} (SendPackage pkg) =
  traverse_ sending =<< tryReadMVar _conn
  where
    sending conn = enqueuePackage conn pkg