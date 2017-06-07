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
  Internal { _setts    :: Settings
           , _disc     :: Discovery
           , _logger   :: Logger
           , _logMgr   :: LogManager
           , _mainBus  :: Hub
           , _builder  :: ConnectionBuilder
           , _stage    :: MVar Stage
           , _last     :: IORef (Maybe EndPoint)
           , _sending  :: TVar Bool
           , _opMgr    :: Operation.Manager
           , _subMgr   :: Subscription.Manager
           , _conn     :: MVar Connection
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
  internal <- mkInternal <$> newMVar Init
                         <*> newIORef Nothing
                         <*> newTVarIO False
                         <*> Operation.new logMgr setts
                         <*> Subscription.new logMgr setts
                         <*> newEmptyMVar

  subscribe mainBus (onInit internal)
  subscribe mainBus (onEstablish internal)
  subscribe mainBus (onEstablished internal)
  subscribe mainBus (onArrived internal)
  subscribe mainBus (onCloseConnection internal)
  subscribe mainBus (onConnectionError internal)
  subscribe mainBus (onTick internal)

  -- subscribe mainBus (onArrived internal)
  -- subscribe mainBus (onShutdown internal)
  -- subscribe mainBus (onTick internal)
  -- subscribe mainBus (onSubmitOperation internal)
  -- subscribe mainBus (onConnectionError internal)
  -- subscribe mainBus (onSubmitSubscription internal)

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
      atts <- freshAttempt
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
                  let msg = "Failed to resolve TCP endpoint to which to \
                            \connect {}."
                  logFormat _logger Warn msg (Only $ Shown e)
                  publish _mainBus (CloseConnection e)
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
closeConnection i@Internal{..} cause = do
  logFormat _logger Debug "CloseConnection: {}" (Only $ Shown cause)
  prev <- previousAttempt <$> swapMVar _stage Closed
  Operation.cleanup _opMgr
  Subscription.cleanup _subMgr

  closeTcpConnection i prev cause
  logFormat _logger Info "CloseConnection: connection cleanup done for [{}]" (Only $ Shown cause)
  where
    previousAttempt (Connecting att _) = Just att
    previousAttempt _                  = Nothing

--------------------------------------------------------------------------------
closeTcpConnection :: Exception e => Internal -> Maybe Attempts -> e -> IO ()
closeTcpConnection Internal{..} prev cause = traverse_ closing =<< tryTakeMVar _conn
  where
    closing conn = do
      let cid = connectionId conn
      logFormat _logger Debug "CloseTcpConnection: connection [{}]. Cause: {}"
        (Shown cid, Shown cause)
      dispose conn
      logFormat _logger Debug "CloseTcpConnection: connection [{}] disposed."
        (Only $ Shown cid)

      att <- maybe freshAttempt return prev
      _   <- swapMVar _stage (Connecting att Reconnecting)
      return ()

--------------------------------------------------------------------------------
onCloseConnection :: Internal -> CloseConnection -> IO ()
onCloseConnection i (CloseConnection e) = closeConnection i e

--------------------------------------------------------------------------------
onEstablish :: Internal -> EstablishConnection -> IO ()
onEstablish i (EstablishConnection ept) = establish i ept

--------------------------------------------------------------------------------
onTick :: Internal -> Tick -> IO ()
onTick i@Internal{..} _ =
  takeMVar _stage >>= \case
    stage@(Connecting Attempts{..} s) -> do
      now <- getCurrentTime
      case s of
        Reconnecting
          | diffUTCTime now attemptLastStart >= s_reconnect_delay _setts
            -> do
              let retries = attemptCount + 1
                  att     = Attempts retries now
              putMVar _stage (Connecting att Reconnecting)
              case s_retry _setts of
                AtMost n
                  | attemptCount <= n -> do
                    let msg = "Checking reconnection... (attempt {})"
                    logFormat _logger Debug msg (Only $ Shown attemptCount)
                    discover i
                  | otherwise -> closeConnection i ConnectionMaxAttemptReached
                KeepRetrying -> closeConnection i ConnectionMaxAttemptReached
          | otherwise -> putMVar _stage stage
        _ -> putMVar _stage stage
    stage@Connected -> do
      conn <- readMVar _conn
      Operation.check _opMgr conn
      Subscription.check _subMgr conn
      putMVar _stage stage
    stage -> putMVar _stage stage

--------------------------------------------------------------------------------
onArrived :: Internal -> PackageArrived -> IO ()
onArrived Internal{..} (PackageArrived conn pkg@Package{..}) = do
  stage     <- takeMVar _stage
  knownConn <- readMVar _conn

  when (validStage stage && knownConn == conn) $ do
    logFormat _logger Debug "Package received: command {}"
      (Only $ Shown packageCmd)

    handlePackage

  putMVar _stage stage

  where
    validStage Connecting{} = True
    validStage Connected    = True
    validStage _            = False

    heartbeatResponse = heartbeatResponsePackage packageCorrelation

    handlePackage
      | packageCmd == heartbeatRequestCmd =
        enqueuePackage conn heartbeatResponse
      | otherwise = return ()

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

-- --------------------------------------------------------------------------------
-- onCloseConnection :: Internal -> CloseConnection -> IO ()
-- onCloseConnection i@Internal{..} (CloseConnection reason) = do
--   att <- freshAttempt

--   case reason of
--     Just e ->
--       logFormat _logger Error "Connection error, reason: {}" (Only $ Shown e)
--     _ ->
--       logMsg _logger Error "Connection error"

--   outcome <- atomically $ do
--     stage <- readTVar _stage
--     writeTVar _stage Closed
--     case stage of
--       Connected conn -> do
--         writeTVar _stage (Connecting att Reconnecting)
--         return $ Just conn
--       _ -> return Nothing

--   traverse_ (closeConnection i) outcome

-- --------------------------------------------------------------------------------
-- discover :: Internal -> IO ()
-- discover i@Internal{..} = do
--   canProceed <- atomically $ do
--     stage <- readTVar _stage
--     case stage of
--       Connecting att state ->
--         case state of
--           Reconnecting -> do
--             writeTVar _stage (Connecting att EndpointDiscovery)
--             return True
--           _ -> return False
--       _ -> return False

--   when canProceed $ do
--     _ <- fork $ do
--       old     <- readTVarIO _last
--       outcome <- tryAny $ runDiscovery _disc old
--       case outcome of
--         Left e -> do
--           let msg = "Failed to resolve TCP end point to which to connect: {}."
--           logFormat _logger Error msg (Only $ Shown e)
--           onCloseConnection i (CloseConnection $ Just e)
--         Right opt ->
--           case opt of
--             Nothing -> do
--               let msg = "Failed to resolve TCP end point to which to connect."
--               logMsg _logger Error msg
--               onCloseConnection i (CloseConnection Nothing)
--             Just endpoint -> establish i endpoint

--     return ()

-- --------------------------------------------------------------------------------
-- onEstablish :: Internal -> EstablishConnection -> IO ()
-- onEstablish i (EstablishConnection ept) = establish i ept

-- --------------------------------------------------------------------------------
-- establish :: Internal -> EndPoint -> IO ()
-- establish i@Internal{..} ept = do
--   canProceed <- atomically $ do
--     stage <- readTVar _stage
--     case stage of
--       Connecting att state ->
--         case state of
--           EndpointDiscovery -> do
--             writeTVar _stage (Connecting att ConnectionEstablishing)
--             return True
--           _ -> return False
--       _ -> return False

--   when canProceed $ do
--     _ <- fork $ do
--       outcome <- tryAny $ connect _builder ept
--       case outcome of
--         Left _ -> atomically $ do
--           stage <- readTVar _stage
--           case stage of
--             Connecting att _ -> writeTVar _stage (Connecting att Reconnecting)
--             _                -> return ()
--         Right conn -> do
--           cid <- nextRandom
--           atomically $ writeTVar _last (Just ept)
--           established i ept conn

--     return ()


-- --------------------------------------------------------------------------------
-- established :: Internal -> EndPoint -> Connection -> IO ()
-- established i@Internal{..} ept connection = do
--   valid <- atomically $ do
--     stage <- readTVar _stage
--     case stage of
--       Connecting{} -> do
--         writeTVar _stage (Connected connection)
--         return True
--       _ -> return False

--   when valid $ do
--     let msg = "New connection {} established on [{}]."
--     logFormat _logger Info msg ( Shown $ connectionId connection
--                                , Shown ept
--                                )

-- --------------------------------------------------------------------------------
-- forceReconnect :: Internal -> NodeEndPoints -> IO ()
-- forceReconnect i@Internal{..} node = do
--   let ept = if isJust $ s_ssl _setts
--             then let Just pt = secureEndPoint node in pt
--             else tcpEndPoint node

--   att     <- freshAttempt
--   outcome <- atomically $
--     readTVar _stage >>= \case
--       Connected conn -> do
--         if connectionEndPoint conn /= ept
--           then do
--             writeTVar _stage (Connecting att EndpointDiscovery)
--             return (Just conn)
--           else return Nothing
--       _ -> return Nothing

--   for_ outcome $ \conn -> do
--     let msg = "Connection {}: going to reconnect to [{}], current [{}]."
--     logFormat _logger Info msg ( Shown $ connectionId conn
--                                , Shown ept
--                                , Shown $ connectionEndPoint conn
--                                )
--     closeConnection i conn
--     establish i ept

-- --------------------------------------------------------------------------------
-- closeConnection :: Internal -> Connection -> IO ()
-- closeConnection Internal{..} conn = do
--   dispose conn
--   let msg = "Connection {} is closed."
--   logFormat _logger Info msg (Only $ Shown $ connectionId conn)

-- --------------------------------------------------------------------------------
-- arrived :: Internal -> Connection -> Package -> IO ()
-- arrived i@Internal{..} conn pkg =
--   case packageCmd pkg of
--     cmd | cmd == heartbeatRequestCmd
--           -> let respPkg = heartbeatResponsePackage $ packageCorrelation pkg in
--              enqueuePackage conn respPkg
--         | cmd == badRequestCmd && packageCorrelation pkg == nil
--           -> do let reason = packageDataAsText pkg
--                     msg = "Connection-wide BadRequest received. \
--                           \ Too dangerous to continue: " <> fold reason
--                 shutdown i
--                 publish _mainBus (FatalCondition msg)
--         | otherwise
--           -> Operation.handle _opMgr pkg >>= \case
--                Nothing  -> Subscription.handle _subMgr pkg >>= \case
--                  Nothing -> do
--                    let msg = "Unknown package correlation id {}."
--                    logFormat _logger Warn msg (Only $ Shown $ packageCmd pkg)
--                  Just dec ->
--                    case dec of
--                      Subscription.Handled        -> return ()
--                      Subscription.Reconnect node -> forceReconnect i node
--                Just dec ->
--                  case dec of
--                    Operation.Handled        -> return ()
--                    Operation.Reconnect node -> forceReconnect i node

-- --------------------------------------------------------------------------------
-- onArrived :: Internal -> PackageArrived -> IO ()
-- onArrived i (PackageArrived conn pkg) =
--   readTVarIO (_stage i) >>= \case
--     Connected known
--       | connectionId known == connectionId conn -> arrived i conn pkg
--       | otherwise -> return ()
--     _ -> return ()

-- --------------------------------------------------------------------------------
-- shutdown :: Internal -> IO ()
-- shutdown i@Internal{..} = do
--   logMsg _logger Info "Shutting down..."
--   mConn <- atomically $ do
--     stage <- readTVar _stage
--     writeTVar _stage Closed
--     case stage of
--       Connected conn -> return (Just conn)
--       _              -> return Nothing

--   traverse_ (closeConnection i) mConn

--   Operation.cleanup _opMgr

-- --------------------------------------------------------------------------------
-- onShutdown :: Internal -> SystemShutdown -> IO ()
-- onShutdown i@Internal{..} _ = do
--   done <- atomically $
--     readTVar _stage >>= \case
--       Closed -> return True
--       _      -> return False

--   unless done (shutdown i)
--   publish _mainBus (ServiceTerminated ConnectionManager)

-- --------------------------------------------------------------------------------
-- data TickOutcome
--   = TickClose
--   | TickNoop
--   | TickProceed ConnectingState
--   | TickConnected Connection

-- --------------------------------------------------------------------------------
-- onTick :: Internal -> Tick -> IO ()
-- onTick i@Internal{..} _ = do
--   now <- getCurrentTime

--   outcome <- atomically $ do
--     stage <- readTVar _stage
--     case stage of
--       Connecting att Reconnecting -> do
--         let count   = attemptCount att
--             started = attemptLastStart att
--             delay   = s_reconnect_delay _setts

--         if diffUTCTime now started >= delay
--           then
--             let proceeding = do
--                   let newAtt = att { attemptCount     = count + 1
--                                    , attemptLastStart = now }
--                   writeTVar _stage (Connecting newAtt Reconnecting)
--                   return $ TickProceed Reconnecting in
--             case s_retry _setts of
--               AtMost maxTrial
--                 | count <= maxTrial -> proceeding
--                 | otherwise         -> return TickClose
--               KeepRetrying -> proceeding
--           else return TickNoop
--       Connected cur -> return (TickConnected cur)
--       _             -> return TickNoop

--   case outcome of
--     TickClose -> do
--       --onCloseConnection i (CloseConnection Nothing)
--       -- publish _mainBus SystemShutdown
--       return ()
--     TickProceed state ->
--       case state of
--         Reconnecting -> do
--           logMsg _logger Info "Try reconnecting..."
--           discover i
--         _ -> return ()
--     TickConnected conn -> do
--       Operation.check _opMgr conn
--       Subscription.check _subMgr conn
--     _ -> return ()

-- --------------------------------------------------------------------------------
-- onSubmitOperation :: Internal -> SubmitOperation -> IO ()
-- onSubmitOperation Internal{..} (SubmitOperation cb op) = do
--   stage <- atomically $ readTVar _stage
--   case stage of
--     Closed ->
--       logMsg _logger Warn "Connection closed but we received an operation request"
--     Connected conn -> Operation.submit _opMgr op cb (Just conn)
--     _              -> Operation.submit _opMgr op cb Nothing

-- --------------------------------------------------------------------------------
-- onConnectionError :: Internal -> ConnectionError -> IO ()
-- onConnectionError i@Internal{..} (ConnectionError conn e) = do
--   let msg = "Connection error: reason {}"
--   logFormat _logger Error msg (Only $ Shown e)
--   closeConnection i conn
--   startConnect i

-- --------------------------------------------------------------------------------
-- onSubmitSubscription :: Internal -> SubmitSubscription -> IO ()
-- onSubmitSubscription Internal{..} cmd = do
--   logMsg _logger Debug "Received subscription operation"
--   stage <- atomically $ readTVar _stage
--   case stage of
--     Closed ->
--       logMsg _logger Warn "Connection closed but we received an operation request"
--     Connected conn -> Subscription.submit _subMgr (Just conn) cmd
--     _              -> Subscription.submit _subMgr Nothing cmd