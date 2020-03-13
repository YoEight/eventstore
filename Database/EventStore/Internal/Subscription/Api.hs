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
import           Streaming
import qualified Streaming.Prelude as Streaming

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Types
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Subscription.Types

--------------------------------------------------------------------------------
-- | Common operations supported by a subscription.
class Subscription s where
  -- | Asks for the next subcription event. If that function is called after
  --   a SubDropped event, expect it to hang indefinitely.
  nextSubEvent :: s -> IO SubAction

  -- | Asynchronously unsubscribe from a subscription.
  unsubscribe :: s -> IO ()

--------------------------------------------------------------------------------
-- | Returns the stream of a subscription.
class SubscriptionStream s t | t -> s where
    subscriptionStream :: s -> StreamId t

--------------------------------------------------------------------------------
-- | Streams a subscription events. The stream will end when hitting `Dropped`
--   event but will still emit it.
streamSubEvents :: Subscription s => s -> Stream (Of SubAction) IO ()
streamSubEvents s
  = do rest <- Streaming.span predicate $ Streaming.repeatM (nextSubEvent s)
       outcome <- lift $ Streaming.uncons rest
       for_ outcome $ \(dropped, _) -> Streaming.yield dropped
  where
    predicate (Dropped _) = False
    predicate _ = True

--------------------------------------------------------------------------------
-- | Like `streamSubEvent` but will only emit `ResolvedEvent`.
streamSubResolvedEvents :: Subscription s => s -> Stream (Of ResolvedEvent) IO ()
streamSubResolvedEvents = Streaming.mapMaybe go . streamSubEvents
  where
    go (Submit e) = Just e
    go _ = Nothing

