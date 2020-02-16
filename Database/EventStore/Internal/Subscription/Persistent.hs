{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS -Wno-orphans #-}
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
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Control
import Database.EventStore.Internal.Exec
import Database.EventStore.Internal.Operation.Persist
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Subscription.Api
import Database.EventStore.Internal.Subscription.Message
import Database.EventStore.Internal.Subscription.Packages
import Database.EventStore.Internal.Subscription.Types
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | The server remembers the state of the subscription. This allows for many
--   different modes of operations compared to a regular or catchup subscription
--   where the client holds the subscription state.
--   (Need EventStore >= v3.1.0).
data PersistentSubscription =
  PersistentSubscription
  { _perExec :: Exec
  , _perSubId :: UUID
  , _perStream :: StreamName
  , _perCred :: Maybe Credentials
  , _perSubKey :: TVar (Maybe Text)
  , _perChan :: Chan SubAction
  }

--------------------------------------------------------------------------------
instance Subscription PersistentSubscription where
  nextSubEvent s = readChan (_perChan s)

  unsubscribe s = publishWith (_perExec s) (SendPackage pkg)
    where
      pkg = createUnsubscribePackage (_perSubId s)

--------------------------------------------------------------------------------
instance SubscriptionStream PersistentSubscription EventNumber where
    subscriptionStream = _perStream

--------------------------------------------------------------------------------
newPersistentSubscription
  :: Exec
  -> Text
  -> StreamName
  -> Int32
  -> Maybe Credentials
  -> IO PersistentSubscription
newPersistentSubscription exec grp (StreamName stream) bufSize cred
  = do (subId, varSubKey, chan) <- persist exec grp stream bufSize cred
       let sub =
             PersistentSubscription
             { _perExec = exec
             , _perSubId = subId
             , _perCred = cred
             , _perSubKey = varSubKey
             , _perChan = chan
             , _perStream = StreamName stream
             }

       pure sub

--------------------------------------------------------------------------------
persistentGetSubKey
  :: PersistentSubscription
  -> IO Text
persistentGetSubKey sub
  = atomically $
      do subKeyMay <- readTVar (_perSubKey sub)
         case subKeyMay of
           Just key
             -> pure key
           Nothing
             -> retrySTM

--------------------------------------------------------------------------------
-- | Acknowledges those event ids have been successfully processed.
notifyEventsProcessed
  :: PersistentSubscription
  -> [UUID]
  -> IO ()
notifyEventsProcessed sub evts
  = do subKey <- persistentGetSubKey sub
       let uuid = _perSubId sub
           pkg = createAckPackage (_perCred sub) uuid subKey evts
       publishWith (_perExec sub) (SendPackage pkg)

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
notifyEventsFailed
  :: PersistentSubscription
  -> NakAction
  -> Maybe Text -- Reason
  -> [UUID]
  -> IO ()
notifyEventsFailed sub act res evts
  = do subKey <- persistentGetSubKey sub
       let uuid = _perSubId sub
           pkg = createNakPackage (_perCred sub) uuid subKey act res evts
       publishWith (_perExec sub) (SendPackage pkg)
