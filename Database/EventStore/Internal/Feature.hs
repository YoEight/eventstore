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
    = SendEvent Text ExpectedVersion [Event]
    | ReadEvent Text Int32 Bool
    | ReadEvents ReadDirection Int32 Bool TargetedStreamEvents
    | DeleteStream Text ExpectedVersion Bool
    | StartTransaction Text ExpectedVersion
    | TransactionWriteEvents TransactionId [Event]
    | TransactionCommit TransactionId
    | SetStreamMetadata Text ExpectedVersion StreamMetadata
    | GetStreamMetadata Text

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
    | PersistentFeature PersistentSubscriptionFeature