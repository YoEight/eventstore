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
import Data.ProtocolBuffers
import Data.Serialize
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Settings
import Database.EventStore.Internal.Subscription.Message
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Creates Ack 'Package'.
createAckPackage :: Maybe Credentials -> UUID -> Text -> [UUID] -> Package
createAckPackage cred corr sid eids =
    Package
    { packageCmd         = persistentSubscriptionAckEventsCmd
    , packageCorrelation = corr
    , packageData        = runPut $ encodeMessage msg
    , packageCred        = cred
    }
  where
    bytes = fmap (toStrict . toByteString) eids
    msg   = persistentSubscriptionAckEvents sid bytes

--------------------------------------------------------------------------------
-- | Create Nak 'Package'.
createNakPackage :: Maybe Credentials
                 -> UUID
                 -> Text
                 -> NakAction
                 -> Maybe Text
                 -> [UUID]
                 -> Package
createNakPackage cred corr sid act txt eids =
    Package
    { packageCmd         = persistentSubscriptionNakEventsCmd
    , packageCorrelation = corr
    , packageData        = runPut $ encodeMessage msg
    , packageCred        = cred
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

--------------------------------------------------------------------------------
createAuthPackage :: Credentials -> UUID -> Package
createAuthPackage cred uuid =
  Package
  { packageCmd         = authenticateCmd
  , packageCorrelation = uuid
  , packageData        = ""
  , packageCred        = Just cred
  }
