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
import Database.EventStore.Internal.Types

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
  deriving (Show, Eq, Enum, Bounded, Generic)

--------------------------------------------------------------------------------
instance Hashable Service

--------------------------------------------------------------------------------
data Initialized = Initialized Service

--------------------------------------------------------------------------------
data InitFailed = InitFailed Service

--------------------------------------------------------------------------------
data FatalException = forall e. Exception e => FatalException e

--------------------------------------------------------------------------------
data PackageReceived = PackageReceived Package