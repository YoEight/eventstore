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
  , Decision(..)
  , new
  , check
  , submit
  , handle
  ) where

--------------------------------------------------------------------------------
import ClassyPrelude hiding (handle, group)
import Data.Time
import Data.UUID
import Data.UUID.V4

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Subscription.Command
import Database.EventStore.Internal.Subscription.Packages
import Database.EventStore.Internal.Callback
import Database.EventStore.Internal.Connection (Connection(..))
import qualified Database.EventStore.Internal.Subscription.Api as Api
import Database.EventStore.Internal.Subscription.Types
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Resource =
  Resource { _connId :: !UUID
           , _type   :: !ResourceType
           }

--------------------------------------------------------------------------------
droppedResource :: Resource -> SubDropReason -> IO ()
droppedResource resource e =
  case _type resource of
    ConfirmedSub callback   -> Api.dropped callback e
    NotConfirmedSub pending -> Api.dropped (_pendingCallback pending) e

--------------------------------------------------------------------------------
data ResourceType
  = ConfirmedSub !(Callback SubAction)
  | NotConfirmedSub !Pending

--------------------------------------------------------------------------------
data Pending =
  Pending { _pendingRequest  :: !Request
          , _pendingCallback :: !(Callback SubAction)
          , _pendingState    :: !PendingState
          }

--------------------------------------------------------------------------------
newPending :: Request -> IO (Pending, Package)
newPending req = do
  uuid    <- nextRandom
  let (pkg, callback) = createPackage req uuid
  pending <- Pending req callback <$> newPendingState
  return (pending, pkg)

--------------------------------------------------------------------------------
data RetryDecision
  = DoNothing
  | Retry !Pending !Package
  | MaxAttemptReached

--------------------------------------------------------------------------------
retryPending :: Settings -> Pending -> IO RetryDecision
retryPending Settings{..} self = do
  now <- getCurrentTime
  let pendingStart = _created (_pendingState self)
      hasTimeout   = diffUTCTime now pendingStart >= s_subscriptionTimeout
      canRetry =
        case s_subscriptionRetry of
          AtMost maxAttempts -> _retries (_pendingState self) <= maxAttempts
          KeepRetrying       -> True

  if hasTimeout
    then
      if canRetry
      then do
        uuid <- nextRandom
        let state    = incrPendingState now (_pendingState self)
            nextSelf = self { _pendingState = state }
            pkg      = createPackageOnly (_pendingRequest self) uuid
        return (Retry nextSelf pkg)
      else return MaxAttemptReached
    else return DoNothing

--------------------------------------------------------------------------------
data PendingState =
  PendingState { _retries :: !Int
               , _created :: !UTCTime
               }

--------------------------------------------------------------------------------
newPendingState :: IO PendingState
newPendingState = PendingState 0 <$> getCurrentTime

--------------------------------------------------------------------------------
incrPendingState :: UTCTime -> PendingState -> PendingState
incrPendingState date self = PendingState (_retries self + 1) date

--------------------------------------------------------------------------------
newtype Awaiting = Awaiting Request

--------------------------------------------------------------------------------
type Resources = HashMap UUID Resource

--------------------------------------------------------------------------------
data Manager =
  Manager {  _settings     :: Settings
           , _logger       :: Logger
           , _resourcesRef :: IORef Resources
           , _awaitRef     :: IORef [Awaiting]
           }

--------------------------------------------------------------------------------
new :: LogManager -> Settings -> IO Manager
new logMgr setts =
  Manager setts logger <$> newIORef mempty
                       <*> newIORef []
  where
    logger = getLogger "SubscriptionManager" logMgr

--------------------------------------------------------------------------------
newtype Request =
  Request { createPackage :: UUID -> (Package, Callback SubAction) }

--------------------------------------------------------------------------------
createPackageOnly :: Request -> UUID -> Package
createPackageOnly req = fst . createPackage req

--------------------------------------------------------------------------------
createRequest :: Settings -> SubmitSubscription -> Request
createRequest setts tpe = Request $ \corrId ->
  case tpe of
    ConnectStream cb stream tos ->
      let pkg = createConnectRegularPackage setts corrId stream tos in
      (pkg, cb)
    ConnectPersist cb group stream batch ->
      let pkg = createConnectPersistPackage setts corrId group stream batch in
      (pkg, cb)

--------------------------------------------------------------------------------
registerAwaiting :: Manager -> Connection -> IO ()
registerAwaiting self@Manager{..} conn = do
  xs <- atomicModifyIORef' _awaitRef $ \stack -> ([], stack)

  traverse_ registering xs
  where
    registering (Awaiting req) = register self conn req

--------------------------------------------------------------------------------
schedule :: Manager -> Request -> IO ()
schedule Manager{..} req =
  modifyIORef' _awaitRef (Awaiting req :)

--------------------------------------------------------------------------------
register :: Manager -> Connection -> Request -> IO ()
register Manager{..} conn req = do
  (pending, pkg) <- newPending req
  let key = packageCorrelation pkg
      res = Resource (connectionId conn) (NotConfirmedSub pending)

  modifyIORef' _resourcesRef (insertMap key res)

  enqueuePackage conn pkg

--------------------------------------------------------------------------------
-- | Occurs when a subscription has been retried more than
--   's_subscriptionRetry'.
newtype SubscriptionMaxAttemptReached =
  SubscriptionMaxAttemptReached UUID
  deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception SubscriptionMaxAttemptReached

--------------------------------------------------------------------------------
checkAndRetry :: Manager -> Connection -> IO ()
checkAndRetry Manager{..} conn = do
  resources <- readIORef _resourcesRef
  updated   <- foldM checking resources (mapToList resources)
  writeIORef _resourcesRef updated
  where
    checking resources (key, resource) = do
      if _connId resource /= connectionId conn
        then
          case _type resource of
            ConfirmedSub callback -> do
              Api.dropped callback SubAborted
              return $ deleteMap key resources
            NotConfirmedSub pending -> do
              state <- newPendingState
              uuid  <- nextRandom
              let pkg = createPackageOnly (_pendingRequest pending) uuid
                  updatedPending  = pending { _pendingState = state }
                  updatedType     = NotConfirmedSub updatedPending
                  updatedResource = Resource { _connId = connectionId conn
                                             , _type   = updatedType
                                             }
              enqueuePackage conn pkg
              return $ deleteMap key $ insertMap uuid updatedResource resources
        else
          case _type resource of
            ConfirmedSub{}          -> return resources
            NotConfirmedSub pending ->
              retryPending _settings pending >>= \case
                DoNothing -> return resources
                Retry updatedPending pkg -> do
                  let newKey  = packageCorrelation pkg
                      newType = NotConfirmedSub updatedPending
                      updated = resource { _type = newType }
                  enqueuePackage conn pkg

                  return $ deleteMap key $ insertMap newKey updated resources
                MaxAttemptReached -> do
                  let cb = _pendingCallback pending
                  reject cb (SubscriptionMaxAttemptReached key)
                  return $ deleteMap key resources

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
check self conn = do
  checkAndRetry self conn
  registerAwaiting self conn

--------------------------------------------------------------------------------
submit :: Manager -> Maybe Connection -> SubmitSubscription -> IO ()
submit m@Manager{..} oConn cmd =
  case oConn of
    Nothing   -> schedule m req
    Just conn -> register m conn req
  where
    req = createRequest _settings cmd
-----------------------------------------------------------------------------
data Decision
  = Handled

--------------------------------------------------------------------------------
handle :: Manager -> Package -> IO (Maybe Decision)
handle Manager{..} pkg = do
  resources <- readIORef _resourcesRef
  let corrId = packageCorrelation pkg
  case lookup corrId resources of
    Nothing       -> return Nothing
    Just resource ->
      case decodeServerMessage pkg of
        LiveMsg tpe ->
          case _type resource of
            NotConfirmedSub{} -> do
              let msg = "Received a live message on subscription not confirmed \
                        \yet."
              logMsg _logger Warn msg
              return $ Just Handled
            ConfirmedSub callback -> do
              case tpe of
                EventAppearedMsg event ->
                  Api.submit callback event
                PersistentEventAppearedMsg event ->
                  Api.submit callback event
                DroppedMsg reason -> do
                  writeIORef _resourcesRef (deleteMap corrId resources)
                  Api.dropped callback reason
              return $ Just Handled

        ConfirmationMsg tpe ->
          case _type resource of
            ConfirmedSub{} -> do
              let msg = "Received a confirmation message on an already \
                        \confirmed subscription."
              logMsg _logger Warn msg
              return $ Just Handled
            NotConfirmedSub pending -> do
              let confirmed       = ConfirmedSub $ _pendingCallback pending
                  updated         = resource { _type = confirmed }
                  commitPos       = confirmationCommitPos tpe
                  lastEventNum    = confirmationLastEventNum tpe
                  persistentSubId = confirmationPersistentSubId tpe
                  details         = SubDetails
                                    { subId           = corrId
                                    , subCommitPos    = commitPos
                                    , subLastEventNum = lastEventNum
                                    , subSubId        = persistentSubId
                                    }

              writeIORef _resourcesRef (insertMap corrId updated resources)
              Api.confirmed (_pendingCallback pending) details
              return $ Just Handled
        ErrorMsg tpe -> do
          writeIORef _resourcesRef (deleteMap corrId resources)
          Just <$> handleError resource tpe

--------------------------------------------------------------------------------
handleError :: Resource -> ErrorMsg -> IO Decision
handleError resource tpe =
  case tpe of
    BadRequestMsg msg ->
      Handled <$ droppedResource resource (SubServerError msg)
    NotAuthenticatedMsg msg ->
      Handled <$ droppedResource resource (SubNotAuthenticated msg)
    NotHandledMsg reason info ->
      Handled <$ droppedResource resource (SubNotHandled reason info)
    UnknownMsg cmd -> do
      let msg =  fmap (\c -> "unknown command: " <> tshow c) cmd
      Handled <$ droppedResource resource (SubServerError msg)