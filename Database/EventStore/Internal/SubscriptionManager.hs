{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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
  ( subscriptionManager ) where

--------------------------------------------------------------------------------
import ClassyPrelude
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
          , _pendingSub     :: !(Callback SubAction)
          }

--------------------------------------------------------------------------------
data PersistActionRequest =
  PersistActionRequest { _actionId      :: !UUID
                       , _actionCreated :: !UTCTime
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
           , _mainBus  :: Bus
           , _pendings :: TVar Pendings
           , _actives  :: TVar Actives
           , _requests :: TVar ActionRequests
           , _connId   :: TVar UUID
           }

--------------------------------------------------------------------------------
subscriptionManager :: Logger -> Settings -> Bus -> IO ()
subscriptionManager logger setts mainBus = do
  internal <- Internal setts logger mainBus <$> newTVarIO mempty
                                            <*> newTVarIO mempty
                                            <*> newTVarIO mempty
                                            <*> newTVarIO nil

  subscribe mainBus (onInit internal)
  subscribe mainBus (onSub internal)
  subscribe mainBus (onRecv internal)
  subscribe mainBus (onShutdown internal)

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
                                       , _actionCb      = cb
                                       }

        m <- readTVar _requests
        writeTVar _requests (insertMap corrId p m)
        return pkg
      DeletePersist cb g s -> do
        let pkg = createPersistActionPackage _setts corrId g s PersistDelete
            p   = PersistActionRequest { _actionId      = corrId
                                       , _actionCreated = now
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
  publish _mainBus (ServiceTerminated SubscriptionManager)
