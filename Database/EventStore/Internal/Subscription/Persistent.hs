{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Subscription.Persistent
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Subscription.Persistent where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Callback
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Exec
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Subscription.Api
import Database.EventStore.Internal.Subscription.Message
import Database.EventStore.Internal.Subscription.Types
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Phase
  = Pending
  | Running SubDetails
  | Closed (Maybe SubDropReason)

--------------------------------------------------------------------------------
-- | The server remembers the state of the subscription. This allows for many
--   different modes of operations compared to a regular or catchup subscription
--   where the client holds the subscription state.
--   (Need EventStore >= v3.1.0).
data PersistentSubscription =
  PersistentSubscription { _perExec   :: Exec
                         , _perStream :: StreamName
                         , _perPhase  :: TVar Phase
                         , _perNext   :: STM (Maybe ResolvedEvent)
                         }

--------------------------------------------------------------------------------
instance Subscription PersistentSubscription where
  nextEventMaybeSTM = _perNext

  getSubscriptionDetailsSTM s = do
    p <- readTVar (_perPhase s)
    case p of
      Pending         -> retrySTM
      Running details -> return details
      Closed r        -> throwSTM (SubscriptionClosed r)

  subscriptionStream = _perStream

  unsubscribe s = subUnsubscribe (_perExec s) s

--------------------------------------------------------------------------------
newPersistentSubscription :: Exec
                          -> Text
                          -> StreamName
                          -> Int32
                          -> IO PersistentSubscription
newPersistentSubscription exec grp stream bufSize = do
  phaseVar <- newTVarIO Pending
  queue    <- newTQueueIO

  let name = streamNameRaw stream
      sub  = PersistentSubscription exec stream phaseVar $ do
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
      callback Reconnect = do
        atomically $ writeTVar phaseVar Pending
        cb <- newCallbackSimple callback
        publish exec (ConnectPersist cb grp name bufSize)

  cb <- newCallbackSimple callback
  publish exec (ConnectPersist cb grp name bufSize)
  return sub

--------------------------------------------------------------------------------
-- | Acknowledges those event ids have been successfully processed.
notifyEventsProcessed :: PersistentSubscription -> [UUID] -> IO ()
notifyEventsProcessed PersistentSubscription{..} evts = do
  details <- atomically $ do
    p <- readTVar _perPhase
    case p of
      Closed r  -> throwSTM (SubscriptionClosed r)
      Pending   -> retrySTM
      Running d -> return d

  publish _perExec (AckPersist details evts)

--------------------------------------------------------------------------------
-- | Acknowledges that 'ResolvedEvent' has been successfully processed.
acknowledge :: PersistentSubscription -> ResolvedEvent -> IO ()
acknowledge sub e = notifyEventsProcessed sub [resolvedEventOriginalId e]

--------------------------------------------------------------------------------
-- | Acknowledges those 'ResolvedEvent's have been successfully processed.
acknowledgeEvents :: PersistentSubscription -> [ResolvedEvent] -> IO ()
acknowledgeEvents sub = notifyEventsProcessed sub . fmap resolvedEventOriginalId

--------------------------------------------------------------------------------
-- | Mark a message that has failed processing. The server will take action
--   based upon the action parameter.
failed :: PersistentSubscription
       -> ResolvedEvent
       -> NakAction
       -> Maybe Text
       -> IO ()
failed sub e a r = notifyEventsFailed sub a r [resolvedEventOriginalId e]

--------------------------------------------------------------------------------
-- | Mark messages that have failed processing. The server will take action
--   based upon the action parameter.
eventsFailed :: PersistentSubscription
             -> [ResolvedEvent]
             -> NakAction
             -> Maybe Text
             -> IO ()
eventsFailed sub evts a r =
  notifyEventsFailed sub a r $ fmap resolvedEventOriginalId evts

--------------------------------------------------------------------------------
-- | Acknowledges those event ids have failed to be processed successfully.
notifyEventsFailed :: PersistentSubscription
                   -> NakAction
                   -> Maybe Text
                   -> [UUID]
                   -> IO ()
notifyEventsFailed PersistentSubscription{..} act res evts = do
  details <- atomically $ do
    p <- readTVar _perPhase
    case p of
      Closed r  -> throwSTM (SubscriptionClosed r)
      Pending   -> retrySTM
      Running d -> return d

  publish _perExec (NakPersist details act res evts)
