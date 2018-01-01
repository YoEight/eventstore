--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.PersistOperations
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.PersistOperations
  ( createPersist
  , updatePersist
  , deletePersist
  ) where

--------------------------------------------------------------------------------
import Data.ProtocolBuffers

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Subscription.Message
import Database.EventStore.Internal.Subscription.Types
import Database.EventStore.Internal.Settings
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
persistOperation :: Text
                 -> Text
                 -> Maybe Credentials
                 -> PersistAction
                 -> Operation (Maybe PersistActionException)
persistOperation grp stream cred tpe = construct go
  where
    go =
      case tpe of
        PersistCreate ss -> do
          let req = _createPersistentSubscription grp stream ss
          resp <- send createPersistentSubscriptionCmd
                       createPersistentSubscriptionCompletedCmd cred req
          let result = createRException $ getField $ cpscResult resp
          yield result
        PersistUpdate ss -> do
          let req = _updatePersistentSubscription grp stream ss
          resp <- send updatePersistentSubscriptionCmd
                       updatePersistentSubscriptionCompletedCmd cred req
          let result = updateRException $ getField $ upscResult resp
          yield result
        PersistDelete -> do
          let req = _deletePersistentSubscription grp stream
          resp <- send deletePersistentSubscriptionCmd
                       deletePersistentSubscriptionCompletedCmd cred req
          let result = deleteRException $ getField $ dpscResult resp
          yield result

--------------------------------------------------------------------------------
createPersist :: Text
              -> Text
              -> PersistentSubscriptionSettings
              -> Maybe Credentials
              -> Operation (Maybe PersistActionException)
createPersist grp stream ss cred =
  persistOperation grp stream cred (PersistCreate ss)

--------------------------------------------------------------------------------
updatePersist :: Text
              -> Text
              -> PersistentSubscriptionSettings
              -> Maybe Credentials
              -> Operation (Maybe PersistActionException)
updatePersist grp stream ss cred =
  persistOperation grp stream cred (PersistUpdate ss)

--------------------------------------------------------------------------------
deletePersist :: Text
              -> Text
              -> Maybe Credentials
              -> Operation (Maybe PersistActionException)
deletePersist grp stream cred = persistOperation grp stream cred PersistDelete
