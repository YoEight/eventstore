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

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Callback
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data SystemInit = SystemInit deriving Typeable

--------------------------------------------------------------------------------
data SystemShutdown = SystemShutdown deriving Typeable

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
data ServiceTerminated = ServiceTerminated Service deriving Typeable

--------------------------------------------------------------------------------
data NewTimer =
  forall e. Typeable e => NewTimer e Duration Bool
  deriving Typeable

--------------------------------------------------------------------------------
newtype SendPackage = SendPackage Package deriving Typeable