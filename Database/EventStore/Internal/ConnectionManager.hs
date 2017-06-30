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
import Control.Monad.Reader
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
  Internal { _disc          :: Discovery
           , _mainBus       :: Hub
           , _builder       :: ConnectionBuilder
           , _stage         :: IORef Stage
           , _last          :: IORef (Maybe EndPoint)
           , _sending       :: TVar Bool
           , _opMgr         :: Operation.Manager
           , _stopwatch     :: Stopwatch
           , _lastCheck     :: IORef NominalDiffTime
           , _lastConnected :: IORef Bool
           }

--------------------------------------------------------------------------------
connectionManager :: ConnectionBuilder
                  -> Discovery
                  -> Hub
                  -> IO ()
connectionManager builder disc mainBus = do
  stageRef <- newIORef Init
  let mkInternal = Internal disc mainBus builder stageRef
      connRef    = ConnectionRef $ lookingUpConnection stageRef

  stopwatch    <- newStopwatch
  timeoutCheck <- stopwatchElapsed stopwatch
  internal <- mkInternal <$> newIORef Nothing
                         <*> newTVarIO False
                         <*> Operation.new connRef
                         <*> return stopwatch
                         <*> newIORef timeoutCheck
                         <*> newIORef False

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
onInit :: Internal -> SystemInit -> EventStore ()
onInit self@Internal{..} _ = startConnect self

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
              tryAny (liftIO $ runDiscovery _disc old) >>= \case
                Left e -> do
                  $(logError)
                    [i| Failed to resolve TCP endpoint to which to connect #{e}.|]
                  publish _mainBus (CloseConnection e)
                Right opt ->
                  case opt of
                    Nothing -> do
                      $(logWarn)
                        "Failed to resolve TCP endpoint to which to connect."
                    Just ept -> publish _mainBus (EstablishConnection ept)
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
            publish _mainBus (Initialized ConnectionManager)
          atomicWriteIORef _stage (Connecting att (ConnectionEstablishing conn))
        _ -> return ()
    _ -> return ()

--------------------------------------------------------------------------------
established :: Internal -> Connection -> EventStore ()
established Internal{..} conn =
  readIORef _stage >>= \case
    Connecting _ (ConnectionEstablishing known) -> do
      when (conn == known) $ do
        $(logDebug) [i|TCP connection established: #{conn}.|]
        atomicWriteIORef _stage (Connected conn)
    _ -> return ()

--------------------------------------------------------------------------------
onEstablished :: Internal -> ConnectionEstablished -> EventStore ()
onEstablished self (ConnectionEstablished conn) = established self conn

--------------------------------------------------------------------------------
closeConnection :: Exception e => Internal -> e -> EventStore ()
closeConnection self@Internal{..} cause = do
  $(logDebug) [i|CloseConnection: #{cause}.|]
  mConn <- lookupConnectionAndSwitchToClosed self
  Operation.cleanup _opMgr
  traverse_ (closeTcpConnection self cause) mConn
  $(logInfo) [i|CloseConnection: connection cleanup done for [#{cause}].|]
  publish _mainBus (FatalException cause)

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
  $(logDebug) [i|CloseTcpConnection: connection [#{cid}]. Cause: #{cause}.|]
  dispose conn
  $(logDebug) [i|CloseTcpConnection: connection [#{cid}] disposed.|]

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
    closeTcpConnection self (ForceReconnect ept) conn
    att <- freshAttempt _stopwatch
    atomicWriteIORef _stage (Connecting att EndpointDiscovery)
    $(logInfo) [i|#{conn}: going to reconnect to #{ept}.|]
    establish self ept

--------------------------------------------------------------------------------
onEstablish :: Internal -> EstablishConnection -> EventStore ()
onEstablish self (EstablishConnection ept) = establish self ept

--------------------------------------------------------------------------------
onTick :: Internal -> Tick -> EventStore ()
onTick self@Internal{..} _ = do
  setts <- getSettings
  readIORef _stage >>= \case
    Connecting Attempts{..} s
      | onGoingConnection s -> do
        elapsed <- stopwatchElapsed _stopwatch
        if elapsed - attemptLastStart >= s_reconnect_delay setts
          then do
            let retries = attemptCount + 1
                att     = Attempts retries elapsed
            atomicWriteIORef _stage (Connecting att Reconnecting)
            case s_retry setts of
              AtMost n
                | attemptCount <= n -> retryConnection attemptCount
                | otherwise -> maxAttemptReached
              KeepRetrying -> retryConnection attemptCount
          else return ()
      | otherwise -> return ()
    Connected _ -> do
      elapsed           <- stopwatchElapsed _stopwatch
      timeoutCheckStart <- readIORef _lastCheck

      when (elapsed - timeoutCheckStart >= s_operationTimeout setts) $ do
        Operation.check _opMgr
        atomicWriteIORef _lastCheck elapsed
    _ -> return ()
  where
    onGoingConnection Reconnecting             = True
    onGoingConnection ConnectionEstablishing{} = True
    onGoingConnection _                        = False

    maxAttemptReached = do
      closeConnection self ConnectionMaxAttemptReached
      publish _mainBus (FatalException ConnectionMaxAttemptReached)

    retryConnection cnt = do
      $(logDebug) [i|Checking reconnection... (attempt #{cnt}).|]
      discover self

--------------------------------------------------------------------------------
onArrived :: Internal -> PackageArrived -> EventStore ()
onArrived self@Internal{..} (PackageArrived conn pkg@Package{..}) = do
  withConnection $ \knownConn -> do
    if knownConn == conn
      then do
        $(logDebug) [i|Package received:  #{pkg}.|]
        handlePackage
      else do
        $(logDebug) [i|Package IGNORED: #{pkg}.|]
  where
    withConnection :: (Connection -> EventStore ()) -> EventStore ()
    withConnection k = do
      readIORef _stage >>= \case
        Connecting _ (ConnectionEstablishing c) -> k c
        Connected c -> k c
        _ -> $(logDebug) [i|Package IGNORED: #{pkg}|]

    heartbeatResponse = heartbeatResponsePackage packageCorrelation

    handlePackage
      | packageCmd == heartbeatRequestCmd =
        enqueuePackage conn heartbeatResponse
      | otherwise =
        Operation.handle _opMgr pkg >>= \case
          Nothing       -> $(logWarn) [i|Package not handled: #{pkg}|]
          Just decision ->
            case decision of
              Operation.Handled        -> return ()
              Operation.Reconnect node -> forceReconnect self node

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
    $(logError) [i|TCP #{conn} error. Cause: #{e}.|]
    closeConnection self e

--------------------------------------------------------------------------------
onConnectionClosed :: Internal -> ConnectionClosed -> EventStore ()
onConnectionClosed self@Internal{..} (ConnectionClosed conn cause) =
  whenM (isSameConnection self conn) $
    closeTcpConnection self cause conn

--------------------------------------------------------------------------------
onShutdown :: Internal -> SystemShutdown -> EventStore ()
onShutdown self@Internal{..} _ = do
  $(logDebug) "Shutting down..."
  mConn <- lookupConnectionAndSwitchToClosed self
  Operation.cleanup _opMgr
  traverse_ dispose mConn
  $(logDebug) "Shutdown properly."
  publish _mainBus (ServiceTerminated ConnectionManager)

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
onSendPackage :: Internal -> SendPackage -> EventStore ()
onSendPackage self (SendPackage pkg) =
  traverse_ sending =<< lookupConnection self
  where
    sending conn = enqueuePackage conn pkg