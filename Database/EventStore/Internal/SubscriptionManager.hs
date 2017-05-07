{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.SubscriptionManager
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.SubscriptionManager
  ( PersistActionMaxAttemptReached(..)
  , SubscriptionMaxAttemptReached(..)
  , subscriptionManager
  ) where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Time
import Data.UUID
import Data.UUID.V4

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Subscription.Command
import Database.EventStore.Internal.Subscription.Packages
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.Callback
import Database.EventStore.Internal.Subscription.Api
import Database.EventStore.Internal.Subscription.Types
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Active =
  Active { _activeConnId :: !UUID
         , _activeSubId  :: !UUID
         , _activeSub    :: !(Callback SubAction)
         }

--------------------------------------------------------------------------------
data Pending =
  Pending { _pendingConnId  :: !UUID
          , _pendingSubId   :: !UUID
          , _pendingCreated :: !UTCTime
          , _pendingRetries :: !Int
          , _pendingPackage :: !Package
          , _pendingSub     :: !(Callback SubAction)
          }

--------------------------------------------------------------------------------
data PersistActionRequest =
  PersistActionRequest { _actionId      :: !UUID
                       , _actionCreated :: !UTCTime
                       , _actionRetries :: !Int
                       , _actionPackage :: !Package
                       , _actionCb      :: !(Callback ())
                       }

--------------------------------------------------------------------------------
type Pendings       = HashMap UUID Pending
type Actives        = HashMap UUID Active
type ActionRequests = HashMap UUID PersistActionRequest

--------------------------------------------------------------------------------
data Internal =
  Internal { _setts    :: Settings
           , _logger   :: Logger
           , _mainBus  :: Hub
           , _pendings :: TVar Pendings
           , _actives  :: TVar Actives
           , _requests :: TVar ActionRequests
           , _connId   :: TVar UUID
           }

--------------------------------------------------------------------------------
-- | Occurs when a subscription has been retried more than
--   's_subscriptionRetry'.
newtype SubscriptionMaxAttemptReached =
  SubscriptionMaxAttemptReached UUID
  deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception SubscriptionMaxAttemptReached

--------------------------------------------------------------------------------
checkAndRetrySubs :: Settings -> Hub -> Pendings -> IO Pendings
checkAndRetrySubs setts bus pendings = do
  now <- getCurrentTime
  foldM (go now) pendings (mapToList pendings)
  where
    go now current (key, p)
      | diffUTCTime now (_pendingCreated p) >= maxDelay =
        let retry = do
              corrId <- nextRandom
              let oldPkg     = _pendingPackage p
                  newPkg     = oldPkg { packageCorrelation = corrId }
                  newPending = p { _pendingPackage = newPkg
                                 , _pendingCreated = now
                                 , _pendingRetries = _pendingRetries p + 1
                                 }
                  next = deleteMap key $ insertMap corrId newPending current

              publish bus (TcpSend newPkg)
              return next in
        case s_subscriptionRetry setts of
          AtMost maxAttempts
            | _pendingRetries p <= maxAttempts
              -> retry
            | otherwise
              -> do reject (_pendingSub p)
                      (SubscriptionMaxAttemptReached key)

                    return $ deleteMap key current
          KeepRetrying -> retry
      | otherwise = return current

    maxDelay = s_subscriptionTimeout setts

--------------------------------------------------------------------------------
-- | Occurs when a persistent action has been retried more than
--   's_subscriptionRetry'.
newtype PersistActionMaxAttemptReached =
  PersistActionMaxAttemptReached UUID
  deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception PersistActionMaxAttemptReached

--------------------------------------------------------------------------------
checkAndRetryActions :: Settings -> Hub -> ActionRequests -> IO ActionRequests
checkAndRetryActions setts bus pendings = do
  now <- getCurrentTime
  foldM (go now) pendings (mapToList pendings)
  where
    go now current (key, p)
      | diffUTCTime now (_actionCreated p) >= maxDelay =
        let retry = do
              corrId <- nextRandom
              let oldPkg     = _actionPackage p
                  newPkg     = oldPkg { packageCorrelation = corrId }
                  newPending = p { _actionPackage = newPkg
                                 , _actionCreated = now
                                 , _actionRetries = _actionRetries p + 1
                                 }
                  next = deleteMap key $ insertMap corrId newPending current

              publish bus (TcpSend newPkg)
              return next in
        case s_subscriptionRetry setts of
          AtMost maxAttempts
            | _actionRetries p <= maxAttempts
              -> retry
            | otherwise
              -> do reject (_actionCb p) (PersistActionMaxAttemptReached key)
                    return $ deleteMap key current
          KeepRetrying -> retry
      | otherwise = return current

    maxDelay = s_subscriptionTimeout setts

--------------------------------------------------------------------------------
checkAndRetry :: Internal -> IO ()
checkAndRetry Internal{..} = do
  pendings    <- readTVarIO _pendings
  newPendings <- checkAndRetrySubs _setts _mainBus pendings
  atomically $ writeTVar _pendings newPendings

  requests    <- readTVarIO _requests
  newRequests <- checkAndRetryActions _setts _mainBus requests
  atomically $ writeTVar _requests newRequests

--------------------------------------------------------------------------------
subscriptionManager :: Logger -> Settings -> Hub -> IO ()
subscriptionManager logger setts mainBus = do
  internal <- Internal setts logger mainBus <$> newTVarIO mempty
                                            <*> newTVarIO mempty
                                            <*> newTVarIO mempty
                                            <*> newTVarIO nil

  subscribe mainBus (onInit internal)
  subscribe mainBus (onSub internal)
  subscribe mainBus (onRecv internal)
  subscribe mainBus (onShutdown internal)
  subscribe mainBus (onCheck internal)
  subscribe mainBus (onConnectionChanged internal)

--------------------------------------------------------------------------------
onInit :: Internal -> SystemInit -> IO ()
onInit Internal{..} _ =
  publish _mainBus (Initialized SubscriptionManager)

--------------------------------------------------------------------------------
onSub :: Internal -> SubmitSubscription -> IO ()
onSub Internal{..} cmd = do
  corrId <- nextRandom
  now    <- getCurrentTime
  pkg    <- atomically $ do
    cid <- readTVar _connId
    case cmd of
      ConnectStream cb s tos -> do
        let pkg = createConnectRegularPackage _setts corrId s tos
            p   = Pending { _pendingConnId  = cid
                          , _pendingSubId   = corrId
                          , _pendingCreated = now
                          , _pendingRetries = 1
                          , _pendingPackage = pkg
                          , _pendingSub     = cb
                          }

        m <- readTVar _pendings
        writeTVar _pendings (insertMap corrId p m)
        return pkg
      ConnectPersist cb g s b -> do
        let pkg = createConnectPersistPackage _setts corrId g s b
            p   = Pending { _pendingConnId  = cid
                          , _pendingSubId   = corrId
                          , _pendingCreated = now
                          , _pendingRetries = 1
                          , _pendingPackage = pkg
                          , _pendingSub     = cb
                          }

        m <- readTVar _pendings
        writeTVar _pendings (insertMap corrId p m)
        return pkg
      CreatePersist cb g s ss -> do
        let tpe = PersistCreate ss
            pkg = createPersistActionPackage _setts corrId g s tpe
            p   = PersistActionRequest { _actionId      = corrId
                                       , _actionCreated = now
                                       , _actionRetries = 1
                                       , _actionPackage = pkg
                                       , _actionCb      = cb
                                       }

        m <- readTVar _requests
        writeTVar _requests (insertMap corrId p m)
        return pkg
      UpdatePersist cb g s ss -> do
        let tpe = PersistUpdate ss
            pkg = createPersistActionPackage _setts corrId g s tpe
            p   = PersistActionRequest { _actionId      = corrId
                                       , _actionCreated = now
                                       , _actionRetries = 1
                                       , _actionPackage = pkg
                                       , _actionCb      = cb
                                       }

        m <- readTVar _requests
        writeTVar _requests (insertMap corrId p m)
        return pkg
      DeletePersist cb g s -> do
        let pkg = createPersistActionPackage _setts corrId g s PersistDelete
            p   = PersistActionRequest { _actionId      = corrId
                                       , _actionCreated = now
                                       , _actionRetries = 1
                                       , _actionPackage = pkg
                                       , _actionCb      = cb
                                       }

        m <- readTVar _requests
        writeTVar _requests (insertMap corrId p m)
        return pkg
      AckPersist details uids -> do
        let uuid     = subId details
            Just sid = subSubId details
        return $ createAckPackage _setts uuid sid uids
      NakPersist details na r uids -> do
        let uuid     = subId details
            Just sid = subSubId details
        return $ createNakPackage _setts uuid sid na r uids
      Unsubscribe details -> do
        let uuid = subId details
        return $ createUnsubscribePackage _setts uuid

  publish _mainBus (TcpSend pkg)

--------------------------------------------------------------------------------
data RecvOutcome
  = NotHandled
  | Handled (IO ())

--------------------------------------------------------------------------------
handled :: IO () -> STM RecvOutcome
handled = return . Handled

--------------------------------------------------------------------------------
notHandled :: STM RecvOutcome
notHandled = return NotHandled

--------------------------------------------------------------------------------
onRecv :: Internal -> PackageReceived -> IO ()
onRecv i@Internal{..} (PackageReceived pkg) = do
  outcome <- atomically $ do
    mp <- readTVar _pendings
    ma <- readTVar _actives
    mr <- readTVar _requests
    case lookup (packageCorrelation pkg) mp of
      Just p  -> onPendingRecvSTM i p pkg
      Nothing ->
        case lookup (packageCorrelation pkg) ma of
          Just a  -> onActiveRecvSTM i a pkg
          Nothing ->
            case lookup (packageCorrelation pkg) mr of
              Just ac -> onPersistActionSTM i ac pkg
              Nothing -> notHandled

  case outcome of
    NotHandled     -> return ()
    Handled action -> action

--------------------------------------------------------------------------------
onPendingRecvSTM :: Internal -> Pending -> Package -> STM RecvOutcome
onPendingRecvSTM Internal{..} Pending{..} pkg =
  case decodeServerMessage pkg of
    ConfirmationMsg comPos eventNum ->
      onSuccess comPos eventNum Nothing
    PersistentConfirmationMsg subIdent comPos eventNum ->
      onSuccess comPos eventNum (Just subIdent)
    DroppedMsg reason -> do
      onFailure
      handled $ dropped _pendingSub reason
    BadRequestMsg msg -> do
      onFailure
      handled $ dropped _pendingSub (SubServerError msg)
    NotAuthenticatedMsg msg -> do
      onFailure
      handled $ dropped _pendingSub (SubNotAuthenticated msg)
    NotHandledMsg reason info -> do
      onFailure
      handled $ dropped _pendingSub (SubNotHandled reason info)
    UnknownMsg cmd -> do
      let msg = fmap (\c -> "unknown command: " <> tshow c) cmd
      onFailure
      handled $ dropped _pendingSub (SubServerError msg)
    _ -> do
      onFailure
      let msg = "Logic error in Subscription Driver (the impossible happened)"
      handled $ dropped _pendingSub (SubClientError msg)
  where
    onFailure = do
      m <- readTVar _pendings
      writeTVar _pendings (deleteMap _pendingSubId m)

    onSuccess comPos eventNum subIdent = do
      mp  <- readTVar _pendings
      ma  <- readTVar _actives
      cid <- readTVar _connId

      let details =
            SubDetails { subId           = _pendingSubId
                       , subCommitPos    = comPos
                       , subLastEventNum = eventNum
                       , subSubId        = subIdent
                       }

          active =
            Active { _activeConnId = cid
                   , _activeSubId  = _pendingSubId
                   , _activeSub    = _pendingSub
                   }

      if cid == _pendingConnId
        then do
          writeTVar _pendings (deleteMap _pendingSubId mp)
          writeTVar _actives (insertMap _pendingSubId active ma)
          handled $ confirmed _pendingSub details
        else onFailure >> notHandled

--------------------------------------------------------------------------------
onActiveRecvSTM :: Internal -> Active -> Package -> STM RecvOutcome
onActiveRecvSTM Internal{..} Active{..} pkg =
  case decodeServerMessage pkg of
    EventAppearedMsg e ->
      handled $ submit _activeSub e
    PersistentEventAppearedMsg e ->
      handled $ submit _activeSub e
    DroppedMsg reason -> do
      onFailure
      handled $ dropped _activeSub reason
    BadRequestMsg msg -> do
      onFailure
      handled $ dropped _activeSub (SubServerError msg)
    NotAuthenticatedMsg msg -> do
      onFailure
      handled $ dropped _activeSub (SubNotAuthenticated msg)
    NotHandledMsg reason info -> do
      onFailure
      handled $ dropped _activeSub (SubNotHandled reason info)
    UnknownMsg cmd -> do
      let msg = fmap (\c -> "unknown command: " <> tshow c) cmd
      onFailure
      handled $ dropped _activeSub (SubServerError msg)
    _ -> do
      onFailure
      let msg = "Logic error in Subscription Driver (the impossible happened)"
      handled $ dropped _activeSub (SubClientError msg)
  where
    onFailure = do
      m <- readTVar _actives
      writeTVar _actives (deleteMap _activeSubId m)

--------------------------------------------------------------------------------
onPersistActionSTM :: Internal
                   -> PersistActionRequest
                   -> Package
                   -> STM RecvOutcome
onPersistActionSTM Internal{..} PersistActionRequest{..} pkg = do
  m <- readTVar _requests
  writeTVar _requests (deleteMap _actionId m)
  case decodeServerMessage pkg of
    PersistentCreatedMsg res ->
      handled $ completeRequest _actionCb (createRException res)
    PersistentUpdatedMsg res ->
      handled $ completeRequest _actionCb (updateRException res)
    PersistentDeletedMsg res ->
      handled $ completeRequest _actionCb (deleteRException res)
    _ -> notHandled

--------------------------------------------------------------------------------
completeRequest :: Callback () -> Maybe PersistActionException -> IO ()
completeRequest p Nothing  = fulfill p ()
completeRequest p (Just e) = reject p e

--------------------------------------------------------------------------------
onShutdown :: Internal -> SystemShutdown -> IO ()
onShutdown Internal{..} _ = do
  logMsg _logger Info "Shutting down..."

  cleaning <- atomically $ do
    pendings <- readTVar _pendings
    requests <- readTVar _requests
    actives  <- readTVar _actives

    writeTVar _pendings mempty
    writeTVar _requests mempty
    writeTVar _actives  mempty

    return $ do
      for_ pendings $ \Pending{..} ->
        fulfill _pendingSub (Dropped SubAborted)

      for_ actives $ \Active{..}  ->
        fulfill _activeSub (Dropped SubAborted)

      for_ requests $ \PersistActionRequest{..} ->
        reject _actionCb PersistActionAborted

  cleaning
  publish _mainBus (ServiceTerminated SubscriptionManager)

--------------------------------------------------------------------------------
onCheck :: Internal -> Check -> IO ()
onCheck i _ = checkAndRetry i

--------------------------------------------------------------------------------
onConnectionChanged :: Internal -> ConnectionChanged -> IO ()
onConnectionChanged Internal{..} _ = do
  cleaning <- atomically $ do
    actives <- readTVar _actives
    writeTVar _actives mempty

    return $
      for_ actives $ \Active{..} ->
        fulfill _activeSub (Dropped SubAborted)

  cleaning
