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
import Database.EventStore.Internal.Communication (Transmit(..))
import Database.EventStore.Internal.Control (publishWith)
import Database.EventStore.Internal.Exec (Exec)
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Subscription.Message
import Database.EventStore.Internal.Subscription.Types
import Database.EventStore.Internal.Settings
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
persistOperation
  :: Exec
  -> Text
  -> Text
  -> Maybe Credentials
  -> PersistAction
  -> IO (Async (Maybe PersistActionException))
persistOperation exec grp stream cred tpe
  = do m <- mailboxNew
       async $
         case tpe of
           PersistCreate ss
             -> do let req = _createPersistentSubscription grp stream ss
                   pkg <- createPkg createPersistentSubscriptionCmd cred req
                   publishWith exec (Transmit m OneTime pkg)
                   outcome <- mailboxReadDecoded m
                   case outcome of
                     Left e
                       -> throw e
                     Right resp
                       -> pure $ createRException $ getField $ cpscResult resp
           PersistUpdate ss
             -> do let req = _updatePersistentSubscription grp stream ss
                   pkg <- createPkg updatePersistentSubscriptionCmd cred req
                   publishWith exec (Transmit m OneTime pkg)
                   outcome <- mailboxReadDecoded m
                   case outcome of
                     Left e
                       -> throw e
                     Right resp
                       -> pure $ updateRException $ getField $ upscResult resp
           PersistDelete
             -> do let req = _deletePersistentSubscription grp stream
                   pkg <- createPkg deletePersistentSubscriptionCmd cred req
                   publishWith exec (Transmit m OneTime pkg)
                   outcome <- mailboxReadDecoded m
                   case outcome of
                     Left e
                       -> throw e
                     Right resp
                       -> pure $ deleteRException $ getField $ dpscResult resp

--------------------------------------------------------------------------------
createPersist
  :: Exec
  -> Text
  -> Text
  -> PersistentSubscriptionSettings
  -> Maybe Credentials
  -> IO (Async (Maybe PersistActionException))
createPersist exec grp stream ss cred
  = persistOperation exec grp stream cred (PersistCreate ss)

--------------------------------------------------------------------------------
updatePersist
  :: Exec
  -> Text
  -> Text
  -> PersistentSubscriptionSettings
  -> Maybe Credentials
  -> IO (Async (Maybe PersistActionException))
updatePersist exec grp stream ss cred
  = persistOperation exec grp stream cred (PersistUpdate ss)

--------------------------------------------------------------------------------
deletePersist
  :: Exec
  -> Text
  -> Text
  -> Maybe Credentials
  -> IO (Async (Maybe PersistActionException))
deletePersist exec grp stream cred
  = persistOperation exec grp stream cred PersistDelete
