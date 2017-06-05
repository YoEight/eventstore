{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveDataTypeable        #-}
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
import Data.Typeable

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Callback
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Types
import Database.EventStore.Internal.Subscription.Message
import Database.EventStore.Internal.Subscription.Types

--------------------------------------------------------------------------------
data SystemInit = SystemInit deriving Typeable

--------------------------------------------------------------------------------
data SystemShutdown = SystemShutdown deriving Typeable

--------------------------------------------------------------------------------
newtype TcpSend = TcpSend Package deriving Typeable

--------------------------------------------------------------------------------
data Service
  = ConnectionManager
  | TimerService
  deriving (Show, Eq, Enum, Bounded, Typeable, Generic)

--------------------------------------------------------------------------------
instance Hashable Service

--------------------------------------------------------------------------------
data Initialized = Initialized Service deriving Typeable

--------------------------------------------------------------------------------
data InitFailed = InitFailed Service deriving Typeable

--------------------------------------------------------------------------------
data FatalException
  = forall e. Exception e => FatalException e
  | FatalCondition Text
  deriving Typeable

--------------------------------------------------------------------------------
data SubmitOperation =
  forall a. SubmitOperation (Callback a) (Operation a)
  deriving Typeable

--------------------------------------------------------------------------------
data PackageReceived = PackageReceived Package deriving Typeable

--------------------------------------------------------------------------------
data SubmitSubscription
  = ConnectStream (Callback SubAction) Text Bool
  | ConnectPersist (Callback SubAction) Text Text Int32
  deriving Typeable

--------------------------------------------------------------------------------
data SubmitPersistentOperation
  = CreatePersist (Callback ())
                  Text
                  Text
                  PersistentSubscriptionSettings
  | UpdatePersist (Callback ())
                  Text
                  Text
                  PersistentSubscriptionSettings
  | DeletePersist (Callback ()) Text Text
  | AckPersist SubDetails [UUID]
  | NakPersist SubDetails NakAction (Maybe Text) [UUID]
  deriving Typeable

data Unsubscribe = Unsubscribe SubDetails deriving Typeable

--------------------------------------------------------------------------------
data ServiceTerminated = ServiceTerminated Service deriving Typeable

--------------------------------------------------------------------------------
data NewTimer =
  forall e. Typeable e => NewTimer e Duration Bool
  deriving Typeable

--------------------------------------------------------------------------------
data Check = Check deriving Typeable

--------------------------------------------------------------------------------
data ConnectionChanged = ConnectionChanged
  deriving Typeable
