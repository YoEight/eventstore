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
import ClassyPrelude
import Data.ProtocolBuffers

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Subscription.Message
import Database.EventStore.Internal.Subscription.Types
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
persistOperation :: Text
                 -> Text
                 -> PersistAction
                 -> Operation (Maybe PersistActionException)
persistOperation grp stream tpe = construct go
  where
    go =
      case tpe of
        PersistCreate ss -> do
          let req = _createPersistentSubscription grp stream ss
          resp <- send createPersistentSubscriptionCmd
                       createPersistentSubscriptionCompletedCmd req
          let result = createRException $ getField $ cpscResult resp
          yield result
        PersistUpdate ss -> do
          let req = _updatePersistentSubscription grp stream ss
          resp <- send updatePersistentSubscriptionCmd
                       updatePersistentSubscriptionCompletedCmd req
          let result = updateRException $ getField $ upscResult resp
          yield result
        PersistDelete -> do
          let req = _deletePersistentSubscription grp stream
          resp <- send deletePersistentSubscriptionCmd
                       deletePersistentSubscriptionCompletedCmd req
          let result = deleteRException $ getField $ dpscResult resp
          yield result

--------------------------------------------------------------------------------
createPersist :: Text
              -> Text
              -> PersistentSubscriptionSettings
              -> Operation (Maybe PersistActionException)
createPersist grp stream ss = persistOperation grp stream (PersistCreate ss)

--------------------------------------------------------------------------------
updatePersist :: Text
              -> Text
              -> PersistentSubscriptionSettings
              -> Operation (Maybe PersistActionException)
updatePersist grp stream ss = persistOperation grp stream (PersistUpdate ss)

--------------------------------------------------------------------------------
deletePersist :: Text
              -> Text
              -> Operation (Maybe PersistActionException)
deletePersist grp stream = persistOperation grp stream PersistDelete