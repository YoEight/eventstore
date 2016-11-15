--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Manager.Subscription.Types
-- Copyright : (C) 2016 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Manager.Subscription.Types where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
-- | Indicates why a subscription has been dropped.
data SubDropReason
    = SubUnsubscribed
      -- ^ Subscription connection has been closed by the user.
    | SubAccessDenied
      -- ^ The current user is not allowed to operate on the supplied stream.
    | SubNotFound
      -- ^ Given stream name doesn't exist.
    | SubPersistDeleted
      -- ^ Given stream is deleted.
    | SubAborted
      -- ^ Occurs when the user shutdown the connection from the server or if
      -- the connection to the server is no longer possible.
    deriving (Show, Eq)
