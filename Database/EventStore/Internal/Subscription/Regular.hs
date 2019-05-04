{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wno-orphans #-}
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
import Database.EventStore.Internal.Callback
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Control
import Database.EventStore.Internal.Exec
import Database.EventStore.Internal.Operation.Volatile
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Subscription.Api
import Database.EventStore.Internal.Subscription.Types
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Phase
  = Pending
  | Running SubDetails
  | Closed (Either SomeException SubDropReason)

--------------------------------------------------------------------------------
-- | Also referred as volatile subscription. For example, if a stream has 100
--   events in it when a subscriber connects, the subscriber can expect to see
--   event number 101 onwards until the time the subscription is closed or
--   dropped.
data RegularSubscription t =
  RegularSubscription { _regExec   :: Exec
                      , _regStream :: StreamId t
                      , _regPhase  :: TVar Phase
                      , _regNext   :: STM (Maybe ResolvedEvent)
                      }

--------------------------------------------------------------------------------
instance Subscription (RegularSubscription t) where
  nextEventMaybeSTM = _regNext

  getSubscriptionDetailsSTM s = do
    p <- readTVar (_regPhase s)
    case p of
      Pending         -> retrySTM
      Running details -> return details
      Closed outcome  ->
        case outcome of
          Right r -> throwSTM (SubscriptionClosed $ Just r)
          Left e  -> throwSTM e

  unsubscribe s = subUnsubscribe (_regExec s) s

--------------------------------------------------------------------------------
instance SubscriptionStream (RegularSubscription t) t where
    subscriptionStream = _regStream

--------------------------------------------------------------------------------
newRegularSubscription :: Exec
                       -> StreamId t
                       -> Bool
                       -> Maybe Credentials
                       -> IO (RegularSubscription t)
newRegularSubscription exec streamId tos cred = do
  phaseVar <- newTVarIO Pending
  queue    <- newTQueueIO

  let sub = RegularSubscription exec streamId phaseVar $ do
        p       <- readTVar phaseVar
        isEmpty <- isEmptyTQueue queue
        if isEmpty
          then
            case p of
              Closed outcome ->
                case outcome of
                  Right r -> throwSTM (SubscriptionClosed $ Just r)
                  Left e  -> throwSTM e
              _ -> return Nothing
          else Just <$> readTQueue queue

      callback (Left e) = atomically $
          writeTVar phaseVar (Closed $ Left e)
      callback (Right action) =
        case action of
          Confirmed details -> atomically $
            writeTVar phaseVar (Running details)
          Dropped r -> atomically $
            writeTVar phaseVar (Closed $ Right r)
          Submit e -> atomically $ do
            readTVar phaseVar >>= \case
              Running{} -> writeTQueue queue e
              _         -> return ()
          ConnectionReset -> atomically $
            writeTVar phaseVar (Closed $ Right SubAborted)

  cb <- newCallback callback
  publishWith exec (SubmitOperation cb (volatile streamId tos cred))
  return sub
