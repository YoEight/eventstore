{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Subscription.Packages
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Subscription.Packages where

--------------------------------------------------------------------------------
import Data.Int

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Serialize
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Settings
import Database.EventStore.Internal.Subscription.Message
import Database.EventStore.Internal.Subscription.Types
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Creates a regular subscription connection 'Package'.
createConnectRegularPackage :: Settings -> UUID -> Text -> Bool -> Package
createConnectRegularPackage Settings{..} uuid stream tos =
    Package
    { packageCmd         = subscribeToStreamCmd
    , packageCorrelation = uuid
    , packageData        = runPut $ encodeMessage msg
    , packageCred        = s_credentials
    }
  where
    msg = subscribeToStream stream tos

--------------------------------------------------------------------------------
-- | Creates a persistent subscription connection 'Package'.
createConnectPersistPackage :: Settings
                            -> UUID
                            -> Text
                            -> Text
                            -> Int32
                            -> Package
createConnectPersistPackage Settings{..} uuid grp stream bufSize =
    Package
    { packageCmd         = connectToPersistentSubscriptionCmd
    , packageCorrelation = uuid
    , packageData        = runPut $ encodeMessage msg
    , packageCred        = s_credentials
    }
  where
    msg = _connectToPersistentSubscription grp stream bufSize

--------------------------------------------------------------------------------
-- | Creates a persistent subscription 'Package'.
createPersistActionPackage :: Settings
                           -> UUID
                           -> Text
                           -> Text
                           -> PersistAction
                           -> Package
createPersistActionPackage Settings{..} u grp strm tpe =
    Package
    { packageCmd         = cmd
    , packageCorrelation = u
    , packageData        = runPut msg
    , packageCred        = s_credentials
    }
  where
    msg =
        case tpe of
            PersistCreate sett ->
                encodeMessage $ _createPersistentSubscription grp strm sett
            PersistUpdate sett ->
                encodeMessage $ _updatePersistentSubscription grp strm sett
            PersistDelete ->
                encodeMessage $ _deletePersistentSubscription grp strm
    cmd =
        case tpe of
            PersistCreate _  -> createPersistentSubscriptionCmd
            PersistUpdate _  -> updatePersistentSubscriptionCmd
            PersistDelete    -> deletePersistentSubscriptionCmd

--------------------------------------------------------------------------------
-- | Creates Ack 'Package'.
createAckPackage :: Settings -> UUID -> Text -> [UUID] -> Package
createAckPackage Settings{..} corr sid eids =
    Package
    { packageCmd         = persistentSubscriptionAckEventsCmd
    , packageCorrelation = corr
    , packageData        = runPut $ encodeMessage msg
    , packageCred        = s_credentials
    }
  where
    bytes = fmap (toStrict . toByteString) eids
    msg   = persistentSubscriptionAckEvents sid bytes

--------------------------------------------------------------------------------
-- | Create Nak 'Package'.
createNakPackage :: Settings
                 -> UUID
                 -> Text
                 -> NakAction
                 -> Maybe Text
                 -> [UUID]
                 -> Package
createNakPackage Settings{..} corr sid act txt eids =
    Package
    { packageCmd         = persistentSubscriptionNakEventsCmd
    , packageCorrelation = corr
    , packageData        = runPut $ encodeMessage msg
    , packageCred        = s_credentials
    }
  where
    bytes = fmap (toStrict . toByteString) eids
    msg   = persistentSubscriptionNakEvents sid bytes txt act

--------------------------------------------------------------------------------
-- | Create an unsubscribe 'Package'.
createUnsubscribePackage :: UUID -> Package
createUnsubscribePackage uuid =
    Package
    { packageCmd         = unsubscribeFromStreamCmd
    , packageCorrelation = uuid
    , packageData        = runPut $ encodeMessage UnsubscribeFromStream
    , packageCred        = Nothing
    }
