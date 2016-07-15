{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Manager.Subscription.Packages
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Manager.Subscription.Packages where

--------------------------------------------------------------------------------
import Data.Int

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.ProtocolBuffers
import Data.Serialize
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Manager.Subscription.Message
import Database.EventStore.Internal.Manager.Subscription.Model
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Creates a regular subscription connection 'Package'.
createConnectRegularPackage :: Settings -> UUID -> Text -> Bool -> Package
createConnectRegularPackage Settings{..} uuid stream tos =
    Package
    { packageCmd         = 0xC0
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
    { packageCmd         = 0xC5
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
            PersistCreate _  -> 0xC8
            PersistUpdate _  -> 0xCE
            PersistDelete    -> 0xCA

--------------------------------------------------------------------------------
-- | Creates Ack 'Package'.
createAckPackage :: Settings -> UUID -> Text -> [UUID] -> Package
createAckPackage Settings{..} corr sid eids =
    Package
    { packageCmd         = 0xCC
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
    { packageCmd         = 0xCD
    , packageCorrelation = corr
    , packageData        = runPut $ encodeMessage msg
    , packageCred        = s_credentials
    }
  where
    bytes = fmap (toStrict . toByteString) eids
    msg   = persistentSubscriptionNakEvents sid bytes txt act

--------------------------------------------------------------------------------
-- | Create an unsubscribe 'Package'.
createUnsubscribePackage :: Settings -> UUID -> Package
createUnsubscribePackage Settings{..} uuid =
    Package
    { packageCmd         = 0xC3
    , packageCorrelation = uuid
    , packageData        = runPut $ encodeMessage UnsubscribeFromStream
    , packageCred        = s_credentials
    }
