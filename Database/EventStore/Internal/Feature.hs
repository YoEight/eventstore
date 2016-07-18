{-# LANGUAGE DataKinds #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Feature
-- Copyright : (C) 2016 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Feature where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Manager.Subscription
import Database.EventStore.Internal.Promise
import Database.EventStore.Internal.Results
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Feature
    = Operation OperationFeature
    | Subscription SubscriptionFeature

--------------------------------------------------------------------------------
data TargetedStreamEvents
    = RegularStreamEvents Text Int32
    | AllStreamEvents Position

--------------------------------------------------------------------------------
data OperationFeature
    = SendEvents Text ExpectedVersion [Event] (Promise WriteResult)
    | ReadEvent Text Int32 Bool (Promise (ReadResult 'RegularStream ReadEvent))
    | ReadEvents ReadDirection Int32 Bool TargetedStreamEvents
    | DeleteStream Text ExpectedVersion Bool (Promise DeleteResult)
    | StartTransaction Text ExpectedVersion (Promise TransactionId)
    | TransactionWriteEvents TransactionId [Event] (Promise ())
    | TransactionCommit TransactionId (Promise WriteResult)
    | SetStreamMetadata Text ExpectedVersion StreamMetadata (Promise WriteResult)
    | GetStreamMetadata Text (Promise StreamMetadataResult)

--------------------------------------------------------------------------------
data TargetedSubscription
    = RegularStreamSubscription Text Bool
    | AllStreamSubscription Bool
    | PersistentSubscription Text Text Int32

--------------------------------------------------------------------------------
data PersistentSubscriptionFeature
    = NotifyEventsProcessed UUID [UUID]
    | NotifyEventsFailed UUID NakAction (Maybe Text) [UUID]
    | CreateSubscription Text Text PersistentSubscriptionSettings
    | UpdateSubscription Text Text PersistentSubscriptionSettings
    | DeleteSubscription Text Text

--------------------------------------------------------------------------------
data SubscriptionFeature
    = Subscribe TargetedSubscription
    | Unsubscribe UUID
    | PersistentFeature  (Promise ()) PersistentSubscriptionFeature