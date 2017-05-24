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
  , Manager
  , new
  , check
  , submit
  , handle
  ) where

--------------------------------------------------------------------------------
import ClassyPrelude hiding (handle)
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
import Database.EventStore.Internal.Connection (Connection(..))
import qualified Database.EventStore.Internal.Subscription.Api as Api
import Database.EventStore.Internal.Subscription.Types
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Active =
  Active { _activeConnId :: !UUID
         , _activeSubId  :: !UUID
         , _activeSub    :: !(Callback SubAction)
         }

--------------------------------------------------------------------------------
data Pending a =
  Pending { _pendingConnId  :: !UUID
          , _pendingId      :: !UUID
          , _pendingCreated :: !UTCTime
          , _pendingRetries :: !Int
          , _pendingRequest :: Request
          , _pendingCb      :: !(Callback a)
          }

--------------------------------------------------------------------------------
newtype Awaiting = Awaiting Request

--------------------------------------------------------------------------------
type Pendings a     = HashMap UUID (Pending a)
type SubRequests    = Pendings SubAction
type Actives        = HashMap UUID Active
type ActionRequests = Pendings ()

--------------------------------------------------------------------------------
data Manager =
  Manager { _setts        :: Settings
           , _logger      :: Logger
           , _subRequests :: TVar SubRequests
           , _actives     :: TVar Actives
           , _actRequests :: TVar ActionRequests
           , _awaits      :: IORef [Awaiting]
           }

--------------------------------------------------------------------------------
data Expect
  = ExpectSub (Callback SubAction)
  | ExpectAction (Callback ())
  | ExpectNothing

--------------------------------------------------------------------------------
newtype Request = Request { createPackage :: UUID -> (Package, Expect) }

--------------------------------------------------------------------------------
createRequest :: Settings -> SubmitSubscription -> Request
createRequest setts tpe = Request $ \corrId ->
  case tpe of
    ConnectStream cb stream tos ->
      let pkg = createConnectRegularPackage setts corrId stream tos in
      (pkg, ExpectSub cb)
    ConnectPersist cb group stream batch ->
      let pkg = createConnectPersistPackage setts corrId group stream batch in
      (pkg, ExpectSub cb)
    CreatePersist cb group stream csetts ->
      let tpe = PersistCreate csetts
          pkg = createPersistActionPackage setts corrId group stream tpe in
      (pkg, ExpectAction cb)
    UpdatePersist cb group stream usetts ->
      let tpe = PersistUpdate usetts
          pkg = createPersistActionPackage setts corrId group stream tpe in
      (pkg, ExpectAction cb)
    DeletePersist cb group stream ->
      let pkg = createPersistActionPackage setts corrId group stream PersistDelete in
      (pkg, ExpectAction cb)
    AckPersist details uids ->
      let uuid     = subId details
          Just sid = subSubId details
          pkg      = createAckPackage setts uuid sid uids in
      (pkg, ExpectNothing)
    NakPersist details na reason uids ->
      let uuid     = subId details
          Just sid = subSubId details
          pkg      = createNakPackage setts uuid sid na reason uids in
      (pkg, ExpectNothing)
    Unsubscribe details ->
      let uuid = subId details
          pkg  = createUnsubscribePackage setts uuid in
      (pkg, ExpectNothing)

--------------------------------------------------------------------------------
createPending :: Connection -> UUID -> Request -> Callback a -> IO (Pending a)
createPending conn uuid req cb = do
  now  <- getCurrentTime
  return Pending { _pendingConnId  = connectionId conn
                 , _pendingId      = uuid
                 , _pendingCreated = now
                 , _pendingRetries = 1
                 , _pendingRequest = req
                 , _pendingCb      = cb
                 }

--------------------------------------------------------------------------------
registerAwaiting :: Manager -> Connection -> IO ()
registerAwaiting self@Manager{..} conn = do
  xs <- atomicModifyIORef' _awaits $ \stack -> ([], stack)

  traverse_ registering xs
  where
    registering (Awaiting req) = register self conn req


--------------------------------------------------------------------------------
schedule :: Manager -> Request -> IO ()
schedule Manager{..} req =
  atomicModifyIORef' _awaits $ \stack ->
    (Awaiting req : stack, ())

--------------------------------------------------------------------------------
register :: Manager -> Connection -> Request -> IO ()
register Manager{..} conn req = do
  corrId <- nextRandom
  let (pkg, expect) = createPackage req corrId

  enqueuePackage conn pkg

  case expect of
    ExpectSub cb -> do
      pending <- createPending conn corrId req cb
      atomically $ modifyTVar' _subRequests (insertMap corrId pending)
    ExpectAction cb -> do
      pending <- createPending conn corrId req cb
      atomically $ modifyTVar' _actRequests (insertMap corrId pending)
    ExpectNothing -> return ()

--------------------------------------------------------------------------------
-- | Occurs when a subscription has been retried more than
--   's_subscriptionRetry'.
newtype SubscriptionMaxAttemptReached =
  SubscriptionMaxAttemptReached UUID
  deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception SubscriptionMaxAttemptReached

--------------------------------------------------------------------------------
checkAndRetry :: Settings -> Connection -> Pendings a -> IO (Pendings a)
checkAndRetry Settings{..} conn start = do
  now <- getCurrentTime
  foldM (go now) start (mapToList start)
  where
    go now pendings (key, p)
      | diffUTCTime now (_pendingCreated p) >= s_subscriptionTimeout =
        let retry = do
              corrId <- nextRandom
              let (pkg, expect) = createPackage (_pendingRequest p) corrId
                  pending = p { _pendingRetries = _pendingRetries p + 1
                              , _pendingCreated = now
                              }

              enqueuePackage conn pkg
              return $ deleteMap key $ insertMap corrId pending pendings in
        case s_subscriptionRetry of
          AtMost maxAttempts
            | _pendingRetries p <= maxAttempts
              -> retry
            | otherwise -> do
              reject (_pendingCb p) (SubscriptionMaxAttemptReached key)
              return $ deleteMap key pendings
          KeepRetrying -> retry
      | otherwise = return pendings

--------------------------------------------------------------------------------
-- | Occurs when a persistent action has been retried more than
--   's_subscriptionRetry'.
newtype PersistActionMaxAttemptReached =
  PersistActionMaxAttemptReached UUID
  deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception PersistActionMaxAttemptReached

--------------------------------------------------------------------------------
check :: Manager -> Connection -> IO ()
check self@Manager{..} conn = do
  subs    <- readTVarIO _subRequests
  newSubs <- checkAndRetry _setts conn subs
  atomically $ writeTVar _subRequests newSubs

  acts    <- readTVarIO _actRequests
  newActs <- checkAndRetry _setts conn acts
  atomically $ writeTVar _actRequests newActs

  registerAwaiting self conn

--------------------------------------------------------------------------------
new :: LogManager -> Settings -> IO Manager
new logMgr setts =
  Manager setts logger <$> newTVarIO mempty
                       <*> newTVarIO mempty
                       <*> newTVarIO mempty
                       <*> newIORef []
  where
    logger = getLogger "SubscriptionManager" logMgr

--------------------------------------------------------------------------------
submit :: Manager -> Maybe Connection -> SubmitSubscription -> IO ()
submit m@Manager{..} oConn cmd =
  case oConn of
    Nothing   -> schedule m req
    Just conn -> register m conn req
  where
    req = createRequest _setts cmd

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
handle :: Manager -> Package -> IO ()
handle i@Manager{..} pkg = do
  outcome <- atomically $ do
    mp <- readTVar _subRequests
    ma <- readTVar _actives
    mr <- readTVar _actRequests
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
onPendingRecvSTM :: Manager -> Pending SubAction -> Package -> STM RecvOutcome
onPendingRecvSTM Manager{..} Pending{..} pkg =
  case decodeServerMessage pkg of
    ConfirmationMsg comPos eventNum ->
      onSuccess comPos eventNum Nothing
    PersistentConfirmationMsg subIdent comPos eventNum ->
      onSuccess comPos eventNum (Just subIdent)
    DroppedMsg reason -> do
      onFailure
      handled $ Api.dropped _pendingCb reason
    BadRequestMsg msg -> do
      onFailure
      handled $ Api.dropped _pendingCb (SubServerError msg)
    NotAuthenticatedMsg msg -> do
      onFailure
      handled $ Api.dropped _pendingCb (SubNotAuthenticated msg)
    NotHandledMsg reason info -> do
      onFailure
      handled $ Api.dropped _pendingCb (SubNotHandled reason info)
    UnknownMsg cmd -> do
      let msg = fmap (\c -> "unknown command: " <> tshow c) cmd
      onFailure
      handled $ Api.dropped _pendingCb (SubServerError msg)
    _ -> do
      onFailure
      let msg = "Logic error in Subscription Driver (the impossible happened)"
      handled $ Api.dropped _pendingCb (SubClientError msg)
  where
    onFailure = do
      m <- readTVar _subRequests
      writeTVar _subRequests (deleteMap _pendingId m)

    onSuccess comPos eventNum subIdent = do
      mp  <- readTVar _subRequests
      ma  <- readTVar _actives

      let details =
            SubDetails { subId           = _pendingId
                       , subCommitPos    = comPos
                       , subLastEventNum = eventNum
                       , subSubId        = subIdent
                       }

          active =
            Active { _activeConnId = _pendingConnId
                   , _activeSubId  = _pendingId
                   , _activeSub    = _pendingCb
                   }

      writeTVar _subRequests (deleteMap _pendingId mp)
      writeTVar _actives (insertMap _pendingId active ma)
      handled $ Api.confirmed _pendingCb details

--------------------------------------------------------------------------------
onActiveRecvSTM :: Manager -> Active -> Package -> STM RecvOutcome
onActiveRecvSTM Manager{..} Active{..} pkg =
  case decodeServerMessage pkg of
    EventAppearedMsg e ->
      handled $ Api.submit _activeSub e
    PersistentEventAppearedMsg e ->
      handled $ Api.submit _activeSub e
    DroppedMsg reason -> do
      onFailure
      handled $ Api.dropped _activeSub reason
    BadRequestMsg msg -> do
      onFailure
      handled $ Api.dropped _activeSub (SubServerError msg)
    NotAuthenticatedMsg msg -> do
      onFailure
      handled $ Api.dropped _activeSub (SubNotAuthenticated msg)
    NotHandledMsg reason info -> do
      onFailure
      handled $ Api.dropped _activeSub (SubNotHandled reason info)
    UnknownMsg cmd -> do
      let msg = fmap (\c -> "unknown command: " <> tshow c) cmd
      onFailure
      handled $ Api.dropped _activeSub (SubServerError msg)
    _ -> do
      onFailure
      let msg = "Logic error in Subscription Driver (the impossible happened)"
      handled $ Api.dropped _activeSub (SubClientError msg)
  where
    onFailure = do
      m <- readTVar _actives
      writeTVar _actives (deleteMap _activeSubId m)

--------------------------------------------------------------------------------
onPersistActionSTM :: Manager
                   -> Pending ()
                   -> Package
                   -> STM RecvOutcome
onPersistActionSTM Manager{..} Pending{..} pkg = do
  modifyTVar' _actRequests (deleteMap _pendingId)
  case decodeServerMessage pkg of
    PersistentCreatedMsg res ->
      handled $ completeRequest _pendingCb (createRException res)
    PersistentUpdatedMsg res ->
      handled $ completeRequest _pendingCb (updateRException res)
    PersistentDeletedMsg res ->
      handled $ completeRequest _pendingCb (deleteRException res)
    _ -> notHandled

--------------------------------------------------------------------------------
completeRequest :: Callback () -> Maybe PersistActionException -> IO ()
completeRequest p Nothing  = fulfill p ()
completeRequest p (Just e) = reject p e