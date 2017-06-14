{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Subscription.Types
-- Copyright : (C) 2016 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Subscription.Types where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Types
import Database.EventStore.Internal.Subscription.Message

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
    | SubNotAuthenticated (Maybe Text)
    | SubServerError (Maybe Text)
      -- ^ Unexpected error from the server.
    | SubNotHandled !NotHandledReason !(Maybe MasterInfo)
    | SubClientError !Text
    | SubSubscriberMaxCountReached
    deriving (Show, Eq)

--------------------------------------------------------------------------------
toSubDropReason :: DropReason -> SubDropReason
toSubDropReason D_Unsubscribed                  = SubUnsubscribed
toSubDropReason D_NotFound                      = SubNotFound
toSubDropReason D_AccessDenied                  = SubAccessDenied
toSubDropReason D_PersistentSubscriptionDeleted = SubPersistDeleted
toSubDropReason D_SubscriberMaxCountReached     = SubSubscriberMaxCountReached

--------------------------------------------------------------------------------
data SubscriptionClosed = SubscriptionClosed (Maybe SubDropReason)
  deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception SubscriptionClosed

--------------------------------------------------------------------------------
-- | Represents a subscription id.
newtype SubscriptionId = SubscriptionId UUID deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
data SubDetails =
  SubDetails { subId           :: !UUID
             , subCommitPos    :: !Int64
             , subLastEventNum :: !(Maybe Int32)
             , subSubId        :: !(Maybe Text)
             }

--------------------------------------------------------------------------------
-- | Type of persistent action.
data PersistAction
    = PersistCreate PersistentSubscriptionSettings
    | PersistUpdate PersistentSubscriptionSettings
    | PersistDelete

--------------------------------------------------------------------------------
-- | Enumerates all persistent action exceptions.
data PersistActionException
    = PersistActionFail
      -- ^ The action failed.
    | PersistActionAlreadyExist
      -- ^ Happens when creating a persistent subscription on a stream with a
      --   group name already taken.
    | PersistActionDoesNotExist
      -- ^ An operation tried to do something on a persistent subscription or a
      --   stream that don't exist.
    | PersistActionAccessDenied
      -- ^ The current user is not allowed to operate on the supplied stream or
      --   persistent subscription.
    | PersistActionAborted
      -- ^ That action has been aborted because the user shutdown the connection
      --   to the server or the connection to the server is no longer possible.
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception PersistActionException

--------------------------------------------------------------------------------
-- EventStore result mappers:
-- =========================
-- EventStore protocol has several values that means the exact same thing. Those
-- functions convert a specific EventStore to uniform result type common to all
-- persistent actions.
--------------------------------------------------------------------------------
createRException :: CreatePersistentSubscriptionResult
                 -> Maybe PersistActionException
createRException CPS_Success       = Nothing
createRException CPS_AlreadyExists = Just PersistActionAlreadyExist
createRException CPS_Fail          = Just PersistActionFail
createRException CPS_AccessDenied  = Just PersistActionAccessDenied

--------------------------------------------------------------------------------
deleteRException :: DeletePersistentSubscriptionResult
                 -> Maybe PersistActionException
deleteRException DPS_Success      = Nothing
deleteRException DPS_DoesNotExist = Just PersistActionDoesNotExist
deleteRException DPS_Fail         = Just PersistActionFail
deleteRException DPS_AccessDenied = Just PersistActionAccessDenied

--------------------------------------------------------------------------------
updateRException :: UpdatePersistentSubscriptionResult
                 -> Maybe PersistActionException
updateRException UPS_Success      = Nothing
updateRException UPS_DoesNotExist = Just PersistActionDoesNotExist
updateRException UPS_Fail         = Just PersistActionFail
updateRException UPS_AccessDenied = Just PersistActionAccessDenied

--------------------------------------------------------------------------------
data SubAction
  = Submit ResolvedEvent
  | Dropped SubDropReason
  | Confirmed SubDetails
