--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Subscription.Regular
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Subscription.Regular where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Callback
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Exec
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Subscription.Api
import Database.EventStore.Internal.Subscription.Types
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Phase
  = Pending
  | Running SubDetails
  | Closed (Maybe SubDropReason)

--------------------------------------------------------------------------------
-- | Also referred as volatile subscription. For example, if a stream has 100
--   events in it when a subscriber connects, the subscriber can expect to see
--   event number 101 onwards until the time the subscription is closed or
--   dropped.
data RegularSubscription =
  RegularSubscription { _regExec   :: Exec
                      , _regStream :: StreamName
                      , _regPhase  :: TVar Phase
                      , _regNext   :: STM (Maybe ResolvedEvent)
                      }

--------------------------------------------------------------------------------
instance Subscription RegularSubscription where
  nextEventMaybeSTM = _regNext

  getSubscriptionDetailsSTM s = do
    p <- readTVar (_regPhase s)
    case p of
      Pending         -> retrySTM
      Running details -> return details
      Closed r        -> throwSTM (SubscriptionClosed r)

  subscriptionStream = _regStream

  unsubscribe s = subUnsubscribe (_regExec s) s

--------------------------------------------------------------------------------
newRegularSubscription :: Exec
                       -> StreamName
                       -> Bool
                       -> IO RegularSubscription
newRegularSubscription exec stream tos = do
  phaseVar <- newTVarIO Pending
  queue    <- newTQueueIO

  let name = streamNameRaw stream
      sub  = RegularSubscription exec stream phaseVar $ do
        p       <- readTVar phaseVar
        isEmpty <- isEmptyTQueue queue
        if isEmpty
          then
            case p of
              Closed r -> throwSTM (SubscriptionClosed r)
              _        -> return Nothing
          else Just <$> readTQueue queue

      callback (Confirmed details) = atomically $
        writeTVar phaseVar (Running details)
      callback (Dropped r) = atomically $
        writeTVar phaseVar (Closed $ Just r)
      callback (Submit e) = atomically $ do
        p <- readTVar phaseVar
        case p of
          Running{} -> writeTQueue queue e
          _         -> return ()

  cb <- newCallbackSimple callback
  publish exec (ConnectStream cb name tos)
  return sub