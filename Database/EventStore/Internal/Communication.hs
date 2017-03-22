{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Communication
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Communication where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.EndPoint
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Promise
import Database.EventStore.Internal.Types
import Database.EventStore.Internal.Manager.Subscription.Driver
import Database.EventStore.Internal.Manager.Subscription.Model

--------------------------------------------------------------------------------
data SystemInit = SystemInit

--------------------------------------------------------------------------------
data SystemShutdown = SystemShutdown

--------------------------------------------------------------------------------
newtype TcpSend = TcpSend Package

--------------------------------------------------------------------------------
data Service
  = OperationManager
  | ConnectionManager
  | SubscriptionManager
  deriving (Show, Eq, Enum, Bounded, Generic)

--------------------------------------------------------------------------------
instance Hashable Service

--------------------------------------------------------------------------------
data Initialized = Initialized Service

--------------------------------------------------------------------------------
data InitFailed = InitFailed Service

--------------------------------------------------------------------------------
data FatalException
  = forall e. Exception e => FatalException e
  | FatalCondition

--------------------------------------------------------------------------------
data PackageReceived = PackageReceived Package

--------------------------------------------------------------------------------
data SubmitOperation = forall a. SubmitOperation (Promise a) (Operation a)

--------------------------------------------------------------------------------
data ForceReconnect = ForceReconnect NodeEndPoints

--------------------------------------------------------------------------------
data Abort = Abort

--------------------------------------------------------------------------------
data SubmitSubscription
  = ConnectStream (SubConnectEvent -> IO ()) Text Bool
  | ConnectPersist (SubConnectEvent -> IO ()) Text Text Int32
  | CreatePersist (Promise ConfirmedAction)
                  Text
                  Text
                  PersistentSubscriptionSettings
  | Unsubscribe Running
  | UpdatePersist (Promise ConfirmedAction)
                  Text
                  Text
                  PersistentSubscriptionSettings
  | DeletePersist (Promise ConfirmedAction) Text Text
  | AckPersist (Promise ()) Running [UUID]
  | NakPersist (Promise ()) Running NakAction (Maybe Text) [UUID]