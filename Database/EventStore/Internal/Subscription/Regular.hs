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
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Control
import Database.EventStore.Internal.Exec
import Database.EventStore.Internal.Operation.Volatile
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Subscription.Api
import Database.EventStore.Internal.Subscription.Types
import Database.EventStore.Internal.Subscription.Packages

--------------------------------------------------------------------------------
-- | Also referred as volatile subscription. For example, if a stream has 100
--   events in it when a subscriber connects, the subscriber can expect to see
--   event number 101 onwards until the time the subscription is closed or
--   dropped.
data RegularSubscription t =
  RegularSubscription
  { _regExec :: Exec
  , _regSubId :: UUID
  , _regStream :: StreamId t
  , _regChan :: Chan SubAction
  }

--------------------------------------------------------------------------------
instance Subscription (RegularSubscription s) where
  nextSubEvent s = readChan (_regChan s)

  unsubscribe s = publishWith (_regExec s) (SendPackage pkg)
    where
      pkg = createUnsubscribePackage (_regSubId s)

--------------------------------------------------------------------------------
instance SubscriptionStream (RegularSubscription t) t where
    subscriptionStream = _regStream

--------------------------------------------------------------------------------
newRegularSubscription
  :: Exec
  -> StreamId t
  -> Bool
  -> Maybe Credentials
  -> IO (RegularSubscription t)
newRegularSubscription exec streamId tos cred
  = do (subId, chan) <- volatile exec streamId tos cred
       let sub =
             RegularSubscription
             { _regExec = exec
             , _regSubId = subId
             , _regStream = streamId
             , _regChan = chan
             }

       pure sub

