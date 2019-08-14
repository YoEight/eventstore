{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module    :  Database.EventStore.Internal.Effect.Driver
-- Copyright :  (C) 2019 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Effect.Driver where

--------------------------------------------------------------------------------
import Data.Hashable (Hashable)
import Data.Time (NominalDiffTime)
import Data.UUID (UUID)
import Polysemy
import Prelude

--------------------------------------------------------------------------------
import Database.EventStore.Internal.EndPoint
import Database.EventStore.Internal.Types (Package)

--------------------------------------------------------------------------------
newtype PackageId = PackageId UUID deriving (Show, Ord, Eq, Hashable)

--------------------------------------------------------------------------------
newtype ConnectionId = ConnectionId UUID deriving (Show, Ord, Eq, Hashable)

--------------------------------------------------------------------------------
data ConnectingState
  = Reconnecting
  | EndpointDiscovery
  | ConnectionEstablishing ConnectionId
  deriving Show

--------------------------------------------------------------------------------
data Stage
  = Connecting ConnectingState
  -- | Connected ConnectionId
  | Closed
  deriving Show

--------------------------------------------------------------------------------
data Driver m a where
  Connect :: EndPoint -> Driver m ConnectionId
  ForceReconnect :: UUID -> NodeEndPoints -> Driver m ConnectionId
  CloseConnection :: ConnectionId -> Driver m ()
  GenerateId :: Driver m UUID
  Discover :: Driver m ()
  GetElapsedTime :: Driver m NominalDiffTime

--------------------------------------------------------------------------------
makeSem ''Driver

