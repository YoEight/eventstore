{-# LANGUAGE ExistentialQuantification #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Cmd
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Cmd where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Manager.Subscription.Driver
import Database.EventStore.Internal.Manager.Subscription.Model
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Cmd
    = forall a. NewOperation (Either OperationError a -> IO ()) (Operation a)
      -- Pushes a new 'Operation' asynchronously.
    | SubCmd SubCmd

--------------------------------------------------------------------------------
data SubCmd
    = ConnectStream (SubConnectEvent -> IO ()) Text Bool
      -- Subscribes to a regular stream.
    | ConnectPersist (SubConnectEvent -> IO ()) Text Text Int32
      -- Subscribes to a persistent subscription.
    | Unsubscribe Running
      -- Unsubscribe from a subscription.
    | CreatePersist (Either PersistActionException ConfirmedAction -> IO ())
          Text Text PersistentSubscriptionSettings
      -- Creates a persistent subscription.
    | UpdatePersist (Either PersistActionException ConfirmedAction -> IO ())
          Text Text PersistentSubscriptionSettings
      -- Updates a persistent subscription.
    | DeletePersist (Either PersistActionException ConfirmedAction -> IO ())
          Text Text
      -- Deletes a persistent subscription.
    | AckPersist Running [UUID]
      -- Acknowledges a set of events has been successfully handled.
    | NakPersist Running NakAction (Maybe Text) [UUID]
      -- Acknowledges a set of events hasn't been handled successfully.
