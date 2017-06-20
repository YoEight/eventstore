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

--------------------------------------------------------------------------------
import ClassyPrelude
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
import           Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Stage
  = Init
  | Connecting Attempts ConnectingState
  | Connected Connection
  | Closed

--------------------------------------------------------------------------------
instance Show Stage where
  show Init             = "Init"
  show (Connecting a s) = "Connecting: " ++ show (a, s)
  show (Connected c)    = "Connected on" ++ show c
  show Closed           = "Closed"

--------------------------------------------------------------------------------
data ConnectingState
  = Reconnecting
  | EndpointDiscovery
  | ConnectionEstablishing Connection
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
newtype CloseConnection = CloseConnection SomeException
  deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception CloseConnection

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
           , _stage     :: IORef Stage
           , _last      :: IORef (Maybe EndPoint)
           , _sending   :: TVar Bool
           , _opMgr     :: Operation.Manager
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
  stageRef <- newIORef Init
  let knownConn  = Operation.KnownConnection $ lookingUpConnection stageRef
      logger     = getLogger "ConnectionManager" logMgr
      mkInternal = Internal setts disc logger logMgr mainBus builder stageRef

  stopwatch    <- newStopwatch
  timeoutCheck <- stopwatchElapsed stopwatch
  internal <- mkInternal <$> newIORef Nothing
                         <*> newTVarIO False
                         <*> Operation.new logMgr setts knownConn
                         <*> return stopwatch
                         <*> newIORef timeoutCheck

  subscribe mainBus (onInit internal)
  subscribe mainBus (onEstablish internal)
  subscribe mainBus (onEstablished internal)
  subscribe mainBus (onArrived internal)
  subscribe mainBus (onSubmitOperation internal)
  subscribe mainBus (onConnectionError internal)
  subscribe mainBus (onConnectionClosed internal)
  subscribe mainBus (onCloseConnection internal)
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
  readIORef _stage >>= \case
    Init -> do
      atts <- freshAttempt _stopwatch
      atomicWriteIORef _stage (Connecting atts Reconnecting)
      discover i
    _ -> return ()

--------------------------------------------------------------------------------
discover :: Internal -> IO ()
discover Internal{..} =
  readIORef _stage >>= \case
    Connecting att p ->
      case p of
        Reconnecting{} -> do
          atomicWriteIORef _stage (Connecting att EndpointDiscovery)
          old <- readIORef _last
          _   <- fork $
              tryAny (runDiscovery _disc old) >>= \case
                Left e    -> do
                  let msg = "Failed to resolve TCP endpoint to which to \
                            \connect {}."
                  logFormat _logger Error msg (Only $ Shown e)
                  publish _mainBus (CloseConnection e)
                Right opt ->
                  case opt of
                    Nothing -> do
                      let msg = "Failed to resolve TCP endpoint to which to \
                                \connect."
                      logMsg _logger Warn msg
                    Just ept -> publish _mainBus (EstablishConnection ept)
          return ()
        _ -> return ()
    s -> return ()

--------------------------------------------------------------------------------
establish :: Internal -> EndPoint -> IO ()
establish Internal{..} ept = do
  logFormat _logger Debug "Establish tcp connection on [{}]" (Only $ Shown ept)
  readIORef _stage >>= \case
    Connecting att s ->
      case s of
        EndpointDiscovery -> do
          conn <- connect _builder ept
          atomicWriteIORef _stage (Connecting att (ConnectionEstablishing conn))
        _ -> return ()
    _ -> return ()

--------------------------------------------------------------------------------
established :: Internal -> Connection -> IO ()
established Internal{..} conn =
  readIORef _stage >>= \case
    Connecting _ (ConnectionEstablishing known) -> do
      when (conn == known) $ do
        let msg = "TCP connection established: {}."
        logFormat _logger Debug msg (Only $ Shown conn)
        atomicWriteIORef _stage (Connected conn)
    _ -> return ()

--------------------------------------------------------------------------------
onEstablished :: Internal -> ConnectionEstablished -> IO ()
onEstablished i (ConnectionEstablished conn) = established i conn

--------------------------------------------------------------------------------
closeConnection :: Exception e => Internal -> e -> IO ()
closeConnection self@Internal{..} cause = do
  logFormat _logger Debug "CloseConnection: {}" (Only $ Shown cause)
  mConn <- lookupConnectionAndSwitchToClosed self
  Operation.cleanup _opMgr
  traverse_ (closeTcpConnection self cause) mConn
  logFormat _logger Info "CloseConnection: connection cleanup done for [{}]"
    (Only $ Shown cause)
  publish _mainBus (FatalException cause)

--------------------------------------------------------------------------------
lookupConnectionAndSwitchToClosed :: Internal -> IO (Maybe Connection)
lookupConnectionAndSwitchToClosed self@Internal{..} = do
  outcome <- lookupConnection self
  atomicWriteIORef _stage Closed
  return outcome

--------------------------------------------------------------------------------
closeTcpConnection :: Exception e => Internal -> e -> Connection -> IO ()
closeTcpConnection Internal{..} cause conn = do
  let cid = connectionId conn
  logFormat _logger Debug "CloseTcpConnection: connection [{}]. Cause: {}"
    (Shown cid, Shown cause)

  dispose conn

  logFormat _logger Debug "CloseTcpConnection: connection [{}] disposed."
    (Only $ Shown cid)

  readIORef _stage >>= \case
    Closed -> return ()
    stage  -> do
      att <-
        case stage of
          Connecting old _ -> return old
          _                -> freshAttempt _stopwatch
      atomicWriteIORef _stage (Connecting att Reconnecting)

--------------------------------------------------------------------------------
data ForceReconnect = ForceReconnect EndPoint deriving (Typeable, Show)

--------------------------------------------------------------------------------
instance Exception ForceReconnect

--------------------------------------------------------------------------------
forceReconnect :: Internal -> NodeEndPoints -> IO ()
forceReconnect self@Internal{..} node = do
  let ept = if isJust $ s_ssl _setts
            then let Just pt = secureEndPoint node in pt
            else tcpEndPoint node

  Connected conn <- readIORef _stage
  when (connectionEndPoint conn /= ept) $ do
    closeTcpConnection self (ForceReconnect ept) conn
    att <- freshAttempt _stopwatch
    atomicWriteIORef _stage (Connecting att EndpointDiscovery)
    let msg = "Connection {}: going to reconnect to [{}], current [{}]."
    logFormat _logger Info msg ( Shown $ connectionId conn
                               , Shown ept
                               , Shown $ connectionEndPoint conn
                               )
    establish self ept

--------------------------------------------------------------------------------
onEstablish :: Internal -> EstablishConnection -> IO ()
onEstablish i (EstablishConnection ept) = establish i ept

--------------------------------------------------------------------------------
onTick :: Internal -> Tick -> IO ()
onTick self@Internal{..} _ =
  readIORef _stage >>= \case
    Connecting Attempts{..} s
      | onGoingConnection s -> do
        elapsed <- stopwatchElapsed _stopwatch
        if elapsed - attemptLastStart >= s_reconnect_delay _setts
          then do
            let retries = attemptCount + 1
                att     = Attempts retries elapsed
            atomicWriteIORef _stage (Connecting att Reconnecting)
            case s_retry _setts of
              AtMost n
                | attemptCount <= n -> retryConnection attemptCount
                | otherwise -> maxAttemptReached
              KeepRetrying -> retryConnection attemptCount
          else return ()
      | otherwise -> return ()
    Connected conn -> do
      elapsed           <- stopwatchElapsed _stopwatch
      timeoutCheckStart <- readIORef _lastCheck

      when (elapsed - timeoutCheckStart >= s_operationTimeout _setts) $ do
        Operation.check _opMgr conn
        atomicWriteIORef _lastCheck elapsed
    _ -> return ()
  where
    onGoingConnection Reconnecting             = True
    onGoingConnection ConnectionEstablishing{} = True
    onGoingConnection _                        = False

    maxAttemptReached = do
      closeConnection self ConnectionMaxAttemptReached
      publish _mainBus (FatalException ConnectionMaxAttemptReached)

    retryConnection i = do
      logFormat _logger Debug "Checking reconnection... (attempt {})" (Only i)
      discover self

--------------------------------------------------------------------------------
onArrived :: Internal -> PackageArrived -> IO ()
onArrived self@Internal{..} (PackageArrived conn pkg@Package{..}) = do
  withConnection $ \knownConn -> do
    if knownConn == conn
      then do
        logFormat _logger Debug "Package received:  {}" (Only $ Shown pkg)
        handlePackage
      else do
        logFormat _logger Debug "Package IGNORED: {}" (Only $ Shown pkg)
  where
    withConnection :: (Connection -> IO ()) -> IO ()
    withConnection k = do
      readIORef _stage >>= \case
        Connecting _ (ConnectionEstablishing conn) -> k conn
        Connected conn -> k conn
        _ -> logFormat _logger Debug "Package IGNORED: {}" (Only $ Shown pkg)

    heartbeatResponse = heartbeatResponsePackage packageCorrelation

    handlePackage
      | packageCmd == heartbeatRequestCmd =
        enqueuePackage conn heartbeatResponse
      | otherwise =
        Operation.handle _opMgr pkg >>= \case
          Nothing -> do
            logFormat _logger Warn "Package not handled: {}" (Only $ Shown pkg)
          Just decision ->
            case decision of
              Operation.Handled        -> return ()
              Operation.Reconnect node -> forceReconnect self node

--------------------------------------------------------------------------------
isSameConnection :: Internal -> Connection -> IO Bool
isSameConnection Internal{..} conn = go <$> readIORef _stage
  where
    go (Connected known)                             = known == conn
    go (Connecting _ (ConnectionEstablishing known)) = known == conn
    go _                                             = False

--------------------------------------------------------------------------------
onConnectionError :: Internal -> ConnectionError -> IO ()
onConnectionError i@Internal{..} (ConnectionError conn e) =
  whenM (isSameConnection i conn) $ do
    let msg = "TCP connection [{}] error. Cause: [{}]"
    logFormat _logger Error msg (Shown conn, Shown e)
    closeConnection i e

--------------------------------------------------------------------------------
onConnectionClosed :: Internal -> ConnectionClosed -> IO ()
onConnectionClosed self@Internal{..} (ConnectionClosed conn cause) =
  whenM (isSameConnection self conn) $
    closeTcpConnection self cause conn

--------------------------------------------------------------------------------
onShutdown :: Internal -> SystemShutdown -> IO ()
onShutdown self@Internal{..} _ = do
  logMsg _logger Debug "Shutting down..."
  mConn <- lookupConnectionAndSwitchToClosed self
  Operation.cleanup _opMgr
  traverse_ dispose mConn
  logMsg _logger Debug "Shutdown properly."
  publish _mainBus (ServiceTerminated ConnectionManager)

--------------------------------------------------------------------------------
onSubmitOperation :: Internal -> SubmitOperation -> IO ()
onSubmitOperation Internal{..} (SubmitOperation callback op) =
  readIORef _stage >>= \case
    Closed -> reject callback Aborted
    _      -> Operation.submit _opMgr op callback

--------------------------------------------------------------------------------
onCloseConnection :: Internal -> CloseConnection -> IO ()
onCloseConnection self e = closeConnection self e

--------------------------------------------------------------------------------
lookupConnection :: Internal -> IO (Maybe Connection)
lookupConnection Internal{..} = lookingUpConnection _stage

--------------------------------------------------------------------------------
lookingUpConnection :: IORef Stage -> IO (Maybe Connection)
lookingUpConnection ref = go <$> readIORef ref
  where
    go (Connected conn)                             = Just conn
    go (Connecting _ (ConnectionEstablishing conn)) = Just conn
    go _                                            = Nothing

--------------------------------------------------------------------------------
onSendPackage :: Internal -> SendPackage -> IO ()
onSendPackage self (SendPackage pkg) =
  traverse_ sending =<< lookupConnection self
  where
    sending conn = enqueuePackage conn pkg