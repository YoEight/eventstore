{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE ViewPatterns       #-}
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
import Control.Monad.Reader
import Data.Time

--------------------------------------------------------------------------------
import           Database.EventStore.Internal.Callback
import           Database.EventStore.Internal.Command
import           Database.EventStore.Internal.Communication
import           Database.EventStore.Internal.Connection
import           Database.EventStore.Internal.Control
import           Database.EventStore.Internal.Discovery
import           Database.EventStore.Internal.EndPoint
import           Database.EventStore.Internal.Logger
import           Database.EventStore.Internal.Operation
import           Database.EventStore.Internal.Operation.Authenticate (newAuthenticatePkg)
import           Database.EventStore.Internal.Operation.Identify (newIdentifyPkg)
import qualified Database.EventStore.Internal.OperationManager as Operation
import           Database.EventStore.Internal.Prelude
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
  show (Connecting a s) = "Connecting: " <> show (a, s)
  show (Connected c)    = "Connected on" <> show c
  show Closed           = "Closed"

--------------------------------------------------------------------------------
data ConnectingState
  = Reconnecting
  | EndpointDiscovery
  | ConnectionEstablishing Connection
  | Authentication UUID NominalDiffTime Connection
  | Identification UUID NominalDiffTime Connection
  deriving Show

--------------------------------------------------------------------------------
data Attempts =
  Attempts { attemptCount     :: !Int
           , attemptLastStart :: !NominalDiffTime
           } deriving Show

--------------------------------------------------------------------------------
freshAttempt :: Stopwatch -> EventStore Attempts
freshAttempt = fmap (Attempts 1) . stopwatchElapsed

--------------------------------------------------------------------------------
data ConnectionMaxAttemptReached = ConnectionMaxAttemptReached
  deriving Typeable

--------------------------------------------------------------------------------
instance Show ConnectionMaxAttemptReached where
  show _ = "Reconnection limit reached."

--------------------------------------------------------------------------------
instance Exception ConnectionMaxAttemptReached

--------------------------------------------------------------------------------
data IdentificationTimeout = IdentificationTimeout deriving Typeable

--------------------------------------------------------------------------------
instance Show IdentificationTimeout where
  show _ = "Timed out waiting for client to be identified."

--------------------------------------------------------------------------------
instance Exception IdentificationTimeout

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
data HeartbeatStage = Interval | Timeout

--------------------------------------------------------------------------------
data HeartbeatTracker =
  HeartbeatTracker { _pkgNum         :: !Integer
                   , _heartbeatStage :: !HeartbeatStage
                   , _startedSince   :: !NominalDiffTime
                   }

--------------------------------------------------------------------------------
newHeartbeatTracker :: MonadBaseControl IO m
                    => Stopwatch
                    -> m (IORef HeartbeatTracker)
newHeartbeatTracker =
  newIORef . HeartbeatTracker 0 Interval <=< stopwatchElapsed

--------------------------------------------------------------------------------
initHeartbeatTracker :: Internal -> EventStore ()
initHeartbeatTracker Internal{..} = do
  elapsed <- stopwatchElapsed _stopwatch
  pkgNum  <- readIORef _lastPkgNum
  let tracker = HeartbeatTracker pkgNum Interval elapsed
  atomicWriteIORef _tracker tracker

--------------------------------------------------------------------------------
data Internal =
  Internal { _disc          :: Discovery
           , _builder       :: ConnectionBuilder
           , _stage         :: IORef Stage
           , _last          :: IORef (Maybe EndPoint)
           , _sending       :: TVar Bool
           , _opMgr         :: Operation.Manager
           , _stopwatch     :: Stopwatch
           , _lastCheck     :: IORef NominalDiffTime
           , _lastConnected :: IORef Bool
           , _tracker       :: IORef HeartbeatTracker
           , _lastPkgNum    :: IORef Integer
           }

--------------------------------------------------------------------------------
incrPackageNumber :: Internal -> EventStore ()
incrPackageNumber Internal{..} = do
  atomicModifyIORef' _lastPkgNum $ \n -> (n + 1, ())
  monitorIncrPkgCount

--------------------------------------------------------------------------------
connectionManager :: ConnectionBuilder
                  -> Discovery
                  -> Hub
                  -> IO ()
connectionManager builder disc mainBus = do
  stageRef <- newIORef Init
  let mkInternal = Internal disc builder stageRef
      connRef    = ConnectionRef $ lookingUpConnectionWhenConnected stageRef

  stopwatch    <- newStopwatch
  timeoutCheck <- stopwatchElapsed stopwatch
  internal <- mkInternal <$> newIORef Nothing
                         <*> newTVarIO False
                         <*> Operation.new connRef
                         <*> return stopwatch
                         <*> newIORef timeoutCheck
                         <*> newIORef False
                         <*> newHeartbeatTracker stopwatch
                         <*> newIORef 0

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

--------------------------------------------------------------------------------
onInit :: Internal -> SystemInit -> EventStore ()
onInit self@Internal{..} _ = do
  publish (NewTimer Tick timerPeriod False)
  startConnect self

--------------------------------------------------------------------------------
startConnect :: Internal -> EventStore ()
startConnect self@Internal{..} =
  readIORef _stage >>= \case
    Init -> do
      atts <- freshAttempt _stopwatch
      atomicWriteIORef _stage (Connecting atts Reconnecting)
      discover self
    _ -> return ()

--------------------------------------------------------------------------------
discover :: Internal -> EventStore ()
discover Internal{..} =
  readIORef _stage >>= \case
    Connecting att p ->
      case p of
        Reconnecting{} -> do
          atomicWriteIORef _stage (Connecting att EndpointDiscovery)
          old <- readIORef _last
          _   <- fork $
              tryAny (runDiscovery _disc old) >>= \case
                Left e -> do
                  $logError
                    [i| Failed to resolve TCP endpoint to which to connect #{e}.|]
                  publish (CloseConnection e)
                Right opt ->
                  case opt of
                    Nothing -> do
                      $logWarn
                        "Failed to resolve TCP endpoint to which to connect."
                    Just ept -> publish (EstablishConnection ept)
          return ()
        _ -> return ()
    _ -> return ()

--------------------------------------------------------------------------------
establish :: Internal -> EndPoint -> EventStore ()
establish Internal{..} ept = do
  $(logDebug) [i|Establish tcp connection on [#{ept}]|]
  readIORef _stage >>= \case
    Connecting att s ->
      case s of
        EndpointDiscovery -> do
          conn <- connect _builder ept
          connected <- atomicModifyIORef' _lastConnected $ \c -> (True, c)
          unless connected $
            publish (Initialized ConnectionManager)
          atomicWriteIORef _stage (Connecting att (ConnectionEstablishing conn))
        _ -> return ()
    _ -> return ()

--------------------------------------------------------------------------------
established :: Internal -> Connection -> EventStore ()
established self@Internal{..} conn =
  readIORef _stage >>= \case
    Connecting att (ConnectionEstablishing known) -> do
      when (conn == known) $ do
        $logDebug [i|TCP connection established: #{conn}.|]
        setts <- getSettings
        case s_defaultUserCredentials setts of
          Just cred -> authenticate self att conn cred
          Nothing   -> identifyClient self att known
    _ -> return ()

--------------------------------------------------------------------------------
authenticate :: Internal
             -> Attempts
             -> Connection
             -> Credentials
             -> EventStore ()
authenticate Internal{..} att conn cred = do
  pkg     <- newAuthenticatePkg cred
  elapsed <- stopwatchElapsed _stopwatch
  let authCorr = packageCorrelation pkg

  atomicWriteIORef _stage (Connecting att (Authentication authCorr elapsed conn))
  enqueuePackage conn pkg

--------------------------------------------------------------------------------
identifyClient :: Internal -> Attempts -> Connection -> EventStore ()
identifyClient Internal{..} att conn = do
  setts <- getSettings
  uuid  <- newUUID
  let defName  = [i|ES-#{uuid}|]
      connName = fromMaybe defName (s_defaultConnectionName setts)

  pkg     <- newIdentifyPkg clientVersion connName
  elapsed <- stopwatchElapsed _stopwatch
  let idCorr = packageCorrelation pkg

  atomicWriteIORef _stage (Connecting att (Identification idCorr elapsed conn))
  enqueuePackage conn pkg
  where
    clientVersion = 1

--------------------------------------------------------------------------------
clientIdentified :: Internal -> EventStore ()
clientIdentified self@Internal{..} =
  readIORef _stage >>= \case
    Connecting _ (Identification _ _ conn) -> do
      $logDebug [i|TCP connection identified: #{conn}.|]
      atomicWriteIORef _stage (Connected conn)
      initHeartbeatTracker self

      -- HACK: It can happen the user submitted operations before the connection was
      -- available. Those operations are only check on every 's_operationTimeout'
      -- ms. This could lead the first operation to take time before gettings.
      -- FIXME: We might consider doing that hack only if it's the first time
      -- we connect with the server.
      Operation.check _opMgr
    _ -> pure ()

--------------------------------------------------------------------------------
onEstablished :: Internal -> ConnectionEstablished -> EventStore ()
onEstablished self (ConnectionEstablished conn) = established self conn

--------------------------------------------------------------------------------
closeConnection :: Exception e => Internal -> e -> EventStore ()
closeConnection self@Internal{..} cause = do
  $logDebug [i|CloseConnection: #{cause}.|]
  mConn <- lookupConnectionAndSwitchToClosed self
  Operation.cleanup _opMgr
  traverse_ (closeTcpConnection self cause) mConn
  $logInfo [i|CloseConnection: connection cleanup done for [#{cause}].|]
  publish (FatalException cause)

--------------------------------------------------------------------------------
lookupConnectionAndSwitchToClosed :: Internal -> EventStore (Maybe Connection)
lookupConnectionAndSwitchToClosed self@Internal{..} = do
  outcome <- lookupConnection self
  atomicWriteIORef _stage Closed
  return outcome

--------------------------------------------------------------------------------
closeTcpConnection :: Exception e => Internal -> e -> Connection -> EventStore ()
closeTcpConnection Internal{..} cause conn = do
  let cid = connectionId conn
  $logDebug [i|CloseTcpConnection: connection [#{cid}]. Cause: #{cause}.|]
  dispose conn
  $logDebug [i|CloseTcpConnection: connection [#{cid}] disposed.|]

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
forceReconnect :: Internal -> NodeEndPoints -> EventStore ()
forceReconnect self@Internal{..} node = do
  setts <- getSettings
  let ept = if isJust $ s_ssl setts
            then let Just pt = secureEndPoint node in pt
            else tcpEndPoint node

  Connected conn <- readIORef _stage
  when (connectionEndPoint conn /= ept) $ do
    monitorIncrForceReconnect
    closeTcpConnection self (ForceReconnect ept) conn
    att <- freshAttempt _stopwatch

    -- We update the last pkg number to nullify the current heartbeat tracking.
    atomicModifyIORef' _lastPkgNum $ \cur -> (cur + 1, ())

    atomicWriteIORef _stage (Connecting att EndpointDiscovery)
    $logInfo [i|#{conn}: going to reconnect to #{ept}.|]
    establish self ept

--------------------------------------------------------------------------------
onEstablish :: Internal -> EstablishConnection -> EventStore ()
onEstablish self (EstablishConnection ept) = establish self ept

--------------------------------------------------------------------------------
onTick :: Internal -> Tick -> EventStore ()
onTick self@Internal{..} _ = do
  setts <- getSettings
  readIORef _stage >>= \case
    (onGoingConnection -> Just Attempts{..}) -> do
      elapsed <- stopwatchElapsed _stopwatch
      when (elapsed - attemptLastStart >= s_reconnect_delay setts) $ do
        let retries = attemptCount + 1
            att     = Attempts retries elapsed
        atomicWriteIORef _stage (Connecting att Reconnecting)
        case s_retry setts of
          AtMost n
            | attemptCount <= n -> retryConnection attemptCount
            | otherwise -> maxAttemptReached
          KeepRetrying -> retryConnection attemptCount

    (pendingAuthenticate -> Just (started, att, conn)) -> do
      elapsed <- stopwatchElapsed _stopwatch
      when (elapsed - started >= s_operationTimeout setts) $ do
        $logWarn "Authentication timed out."
        identifyClient self att conn

    (pendingIdentification -> Just started) -> do
      elapsed <- stopwatchElapsed _stopwatch
      when (elapsed - started >= s_operationTimeout setts) $
        -- We close the current connection and let the reconnection process
        -- to take over.
        traverse_ (closeTcpConnection self IdentificationTimeout)
            =<< lookupConnection self

    (defaultConnecting -> True) -> manageHeartbeats self

    Connected _ -> do
      elapsed           <- stopwatchElapsed _stopwatch
      timeoutCheckStart <- readIORef _lastCheck

      when (elapsed - timeoutCheckStart >= s_operationTimeout setts) $ do
        Operation.check _opMgr
        atomicWriteIORef _lastCheck elapsed

      manageHeartbeats self

    _ -> return ()
  where
    onGoingConnection (Connecting att Reconnecting)             = Just att
    onGoingConnection (Connecting att ConnectionEstablishing{}) = Just att
    onGoingConnection _                                         = Nothing

    pendingIdentification (Connecting _ (Identification _ started _)) = Just started
    pendingIdentification _                                           = Nothing

    pendingAuthenticate (Connecting a (Authentication _ started c)) = Just (started, a, c)
    pendingAuthenticate _                                           = Nothing

    defaultConnecting Connecting{} = True
    defaultConnecting _            = False

    maxAttemptReached = do
      closeConnection self ConnectionMaxAttemptReached
      publish (FatalException ConnectionMaxAttemptReached)

    retryConnection cnt = do
      $logDebug [i|Checking reconnection... (attempt #{cnt}).|]
      discover self

--------------------------------------------------------------------------------
data ServerHeartbeatTimeout = ServerHeartbeatTimeout deriving Typeable

--------------------------------------------------------------------------------
instance Show ServerHeartbeatTimeout where
  show _ = "Server connection has heartbeat timeout"

--------------------------------------------------------------------------------
instance Exception ServerHeartbeatTimeout

--------------------------------------------------------------------------------
manageHeartbeats :: Internal -> EventStore ()
manageHeartbeats self@Internal{..} = traverse_ go =<< lookupConnection self
  where
    go conn = do
      elapsed <- stopwatchElapsed _stopwatch
      pkgNum  <- readIORef _lastPkgNum
      tracker <- readIORef _tracker
      setts   <- getSettings

      let interval    = s_heartbeatInterval setts
          timeout     = s_heartbeatTimeout setts
          initTracker = tracker
                        { _heartbeatStage = Interval
                        , _startedSince   = elapsed
                        , _pkgNum         = pkgNum
                        }

      if _pkgNum tracker /= pkgNum
        then atomicWriteIORef _tracker initTracker
        else
          case _heartbeatStage tracker of
            Interval
              | elapsed - _startedSince tracker >= interval -> do
                uuid <- freshUUID
                let pkg        = heartbeatRequestPackage uuid
                    newTracker = tracker
                                 { _heartbeatStage = Timeout
                                 , _startedSince   = elapsed
                                 , _pkgNum         = pkgNum
                                 }
                enqueuePackage conn pkg
                atomicWriteIORef _tracker newTracker
              | otherwise -> return ()
            Timeout
              | elapsed - _startedSince tracker >= timeout -> do
                monitorIncrHeartbeatTimeouts
                $logInfo [i|Closing #{conn} due to HEARTBEAT TIMEOUT at pkgNum #{pkgNum}|]
                closeTcpConnection self ServerHeartbeatTimeout conn
              | otherwise -> return ()

--------------------------------------------------------------------------------
onArrived :: Internal -> PackageArrived -> EventStore ()
onArrived self@Internal{..} (PackageArrived conn pkg@Package{..}) = do
  -- FIXME - We can merge those two `readIORef` calls into one.
  -- It's done on 2019's refactoring.
  cur <- readIORef _stage
  unless (closedOrInit cur) $
    incrPackageNumber self
  -- /FIXME

  readIORef _stage >>= \case
    (onAuthentication -> Just att) -> do
      when (packageCmd == notAuthenticatedCmd) $
        $logWarn "Not authenticated."

      identifyClient self att conn

    (onIdentification -> True) ->
      clientIdentified self

    (runningConnection -> True) -> do
      $logDebug [i|Package received:  #{pkg}.|]
      handlePackage

    _ -> $logDebug [i|Package IGNORED: #{pkg}.|]

  where
    onIdentification (Connecting _ (Identification u _ _)) =
      packageCorrelation == u && packageCmd == clientIdentifiedCmd
    onIdentification _ = False

    onAuthentication (Connecting a (Authentication u _ _)) =
      if packageCorrelation == u && (packageCmd == authenticatedCmd || packageCmd == notAuthenticatedCmd)
      then Just a
      else Nothing
    onAuthentication _ = Nothing

    runningConnection (Connecting _ (ConnectionEstablishing c)) = conn == c
    runningConnection (Connecting _ (Authentication _ _ c)) = conn == c
    runningConnection (Connecting _ (Identification _ _ c)) = conn == c
    runningConnection (Connected c) = conn == c
    runningConnection _ = False

    heartbeatResponse = heartbeatResponsePackage packageCorrelation

    handlePackage
      | packageCmd == heartbeatResponseCmd = return ()
      | packageCmd == heartbeatRequestCmd =
        enqueuePackage conn heartbeatResponse
      | otherwise =
        Operation.handle _opMgr pkg >>= \case
          Nothing       -> $logWarn [i|Package not handled: #{pkg}|]
          Just decision ->
            case decision of
              Operation.Handled        -> return ()
              Operation.Reconnect node -> forceReconnect self node

    closedOrInit = \case
      Init -> True
      Closed -> True
      _ -> False

--------------------------------------------------------------------------------
isSameConnection :: Internal -> Connection -> EventStore Bool
isSameConnection Internal{..} conn = go <$> readIORef _stage
  where
    go (Connected known)                             = known == conn
    go (Connecting _ (ConnectionEstablishing known)) = known == conn
    go _                                             = False

--------------------------------------------------------------------------------
onConnectionError :: Internal -> ConnectionError -> EventStore ()
onConnectionError self@Internal{..} (ConnectionError conn e) =
  whenM (isSameConnection self conn) $ do
    $logError [i|TCP #{conn} error. Cause: #{e}.|]
    closeConnection self e

--------------------------------------------------------------------------------
onConnectionClosed :: Internal -> ConnectionClosed -> EventStore ()
onConnectionClosed self@Internal{..} (ConnectionClosed conn cause) =
  whenM (isSameConnection self conn) $ do
    closeTcpConnection self cause conn
    monitorIncrConnectionDrop

--------------------------------------------------------------------------------
onShutdown :: Internal -> SystemShutdown -> EventStore ()
onShutdown self@Internal{..} _ = do
  $logDebug "Shutting down..."
  mConn <- lookupConnectionAndSwitchToClosed self
  Operation.cleanup _opMgr
  traverse_ dispose mConn
  $logDebug "Shutdown properly."
  publish (ServiceTerminated ConnectionManager)

--------------------------------------------------------------------------------
onSubmitOperation :: Internal -> SubmitOperation -> EventStore ()
onSubmitOperation Internal{..} (SubmitOperation callback op) =
  readIORef _stage >>= \case
    Closed -> reject callback Aborted
    _      -> Operation.submit _opMgr op callback

--------------------------------------------------------------------------------
onCloseConnection :: Internal -> CloseConnection -> EventStore ()
onCloseConnection self e = closeConnection self e

--------------------------------------------------------------------------------
lookupConnection :: Internal -> EventStore (Maybe Connection)
lookupConnection Internal{..} = lookingUpConnection _stage

--------------------------------------------------------------------------------
lookingUpConnection :: IORef Stage -> EventStore (Maybe Connection)
lookingUpConnection ref = go <$> readIORef ref
  where
    go (Connected conn)                             = Just conn
    go (Connecting _ (ConnectionEstablishing conn)) = Just conn
    go _                                            = Nothing

--------------------------------------------------------------------------------
lookingUpConnectionWhenConnected :: IORef Stage -> EventStore (Maybe Connection)
lookingUpConnectionWhenConnected = fmap go . readIORef
  where
    go (Connected conn) = Just conn
    go _                = Nothing

--------------------------------------------------------------------------------
onSendPackage :: Internal -> SendPackage -> EventStore ()
onSendPackage self (SendPackage pkg) =
  traverse_ sending =<< lookupConnection self
  where
    sending conn = enqueuePackage conn pkg
