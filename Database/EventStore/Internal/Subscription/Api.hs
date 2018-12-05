{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Subscription.Api
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
-- Main Subscription bookkeeping structure.
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Subscription.Api where

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Callback
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Control
import Database.EventStore.Internal.Types
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Subscription.Packages
import Database.EventStore.Internal.Subscription.Types

--------------------------------------------------------------------------------
submit :: Callback SubAction -> ResolvedEvent -> IO ()
submit s xs = fulfill s (Submit xs)

--------------------------------------------------------------------------------
dropped :: Callback SubAction -> SubDropReason -> IO ()
dropped s r = fulfill s (Dropped r)

--------------------------------------------------------------------------------
confirmed :: Callback SubAction -> SubDetails -> IO ()
confirmed s d = fulfill s (Confirmed d)

--------------------------------------------------------------------------------
-- | Common operations supported by a subscription.
class Subscription s where
  -- | Asks for the next incoming event like 'nextEventMaybe' while still being
  --   in the the 'STM'.
  nextEventMaybeSTM :: s -> STM (Maybe ResolvedEvent)

  -- | Returns the runtime details of a subscription.
  getSubscriptionDetailsSTM :: s -> STM SubDetails

  -- | Asynchronously unsubscribe from the the stream.
  unsubscribe :: s -> IO ()

--------------------------------------------------------------------------------
-- | Returns the stream of a subscription.
class SubscriptionStream s t | t -> s where
    subscriptionStream :: s -> StreamId t

--------------------------------------------------------------------------------
-- | Awaits for the next event.
nextEvent :: Subscription s => s -> IO ResolvedEvent
nextEvent s = atomically $ do
  outcome <- nextEventMaybeSTM s
  case outcome of
    Just e  -> return e
    Nothing -> retrySTM

--------------------------------------------------------------------------------
-- | Non blocking version of 'nextEvent'.
nextEventMaybe :: Subscription s => s -> IO (Maybe ResolvedEvent)
nextEventMaybe = atomically . nextEventMaybeSTM

--------------------------------------------------------------------------------
-- | Waits until the `Subscription` has been confirmed.
waitConfirmation :: Subscription s => s -> IO ()
waitConfirmation s = atomically $ do
    _ <- getSubscriptionDetailsSTM s
    return ()

--------------------------------------------------------------------------------
-- | Like 'unsubscribeConfirmed' but lives in 'STM' monad.
unsubscribeConfirmedSTM :: Subscription s => s -> STM Bool
unsubscribeConfirmedSTM s = do
  let action = do
        _ <- getSubscriptionDetailsSTM s
        return False
  catchSTM action $ \(_ :: SomeException) -> return True

--------------------------------------------------------------------------------
-- | Non blocking version of `waitUnsubscribeConfirmed`.
unsubscribeConfirmed :: Subscription s => s -> IO Bool
unsubscribeConfirmed = atomically . unsubscribeConfirmedSTM

--------------------------------------------------------------------------------
-- | Wait until unsubscription has been confirmed by the server.
waitUnsubscribeConfirmed :: Subscription s => s -> IO ()
waitUnsubscribeConfirmed s = atomically $
    unlessM (unsubscribeConfirmedSTM s) retrySTM

--------------------------------------------------------------------------------
subUnsubscribe :: (Pub pub, Subscription s) => pub -> s -> IO ()
subUnsubscribe pub s = do
  outcome <- atomically $ do
    unsubscribed <- unsubscribeConfirmedSTM s
    if unsubscribed
      then return Nothing
      else Just <$> getSubscriptionDetailsSTM s

  for_ outcome $ \details -> do
    let pkg = createUnsubscribePackage (subId details)
    publishWith pub (SendPackage pkg)

--------------------------------------------------------------------------------
-- | Gets the ID of the subscription.
getSubscriptionId :: Subscription s => s -> IO SubscriptionId
getSubscriptionId s = atomically $ do
  details <- getSubscriptionDetailsSTM s
  return (SubscriptionId $ subId details)
