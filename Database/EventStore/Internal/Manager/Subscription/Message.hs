{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# OPTIONS_GHC -fcontext-stack=26     #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Manager.Subscription.Message
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Manager.Subscription.Message where

--------------------------------------------------------------------------------
import Data.ByteString (ByteString)
import Data.Int
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Text (Text)

--------------------------------------------------------------------------------
import Database.EventStore.Internal.TimeSpan
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Stream subscription connection request.
data SubscribeToStream
    = SubscribeToStream
      { subscribeStreamId       :: Required 1 (Value Text)
      , subscribeResolveLinkTos :: Required 2 (Value Bool)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode SubscribeToStream

--------------------------------------------------------------------------------
-- | 'SubscribeToStream' smart constructor.
subscribeToStream :: Text -> Bool -> SubscribeToStream
subscribeToStream stream_id res_link_tos =
    SubscribeToStream
    { subscribeStreamId       = putField stream_id
    , subscribeResolveLinkTos = putField res_link_tos
    }

--------------------------------------------------------------------------------
-- | Stream subscription connection response.
data SubscriptionConfirmation
    = SubscriptionConfirmation
      { subscribeLastCommitPos   :: Required 1 (Value Int64)
      , subscribeLastEventNumber :: Optional 2 (Value Int32)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode SubscriptionConfirmation

--------------------------------------------------------------------------------
-- | Serialized event sent by the server when a new event has been appended to a
--   stream.
data StreamEventAppeared
    = StreamEventAppeared
      { streamResolvedEvent :: Required 1 (Message ResolvedEventBuf) }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode StreamEventAppeared

--------------------------------------------------------------------------------
-- | Represents the reason subscription drop happened.
data DropReason
    = D_Unsubscribed
    | D_AccessDenied
    | D_NotFound
    | D_PersistentSubscriptionDeleted
    deriving (Enum, Eq, Show)

--------------------------------------------------------------------------------
-- | A message sent by the server when a subscription has been dropped.
data SubscriptionDropped
    = SubscriptionDropped
      { dropReason :: Optional 1 (Enumeration DropReason) }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode SubscriptionDropped

--------------------------------------------------------------------------------
-- | A message sent to the server to indicate the user asked to end a
--   subscription.
data UnsubscribeFromStream = UnsubscribeFromStream deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode UnsubscribeFromStream

--------------------------------------------------------------------------------
-- | Create persistent subscription request.
data CreatePersistentSubscription =
    CreatePersistentSubscription
    { cpsGroupName         :: Required 1  (Value Text)
    , cpsStreamId          :: Required 2  (Value Text)
    , cpsResolveLinkTos    :: Required 3  (Value Bool)
    , cpsStartFrom         :: Required 4  (Value Int32)
    , cpsMsgTimeout        :: Required 5  (Value Int32)
    , cpsRecordStats       :: Required 6  (Value Bool)
    , cpsLiveBufSize       :: Required 7  (Value Int32)
    , cpsReadBatchSize     :: Required 8  (Value Int32)
    , cpsBufSize           :: Required 9  (Value Int32)
    , cpsMaxRetryCount     :: Required 10 (Value Int32)
    , cpsPreferRoundRobin  :: Required 11 (Value Bool)
    , cpsChkPtAfterTime    :: Required 12 (Value Int32)
    , cpsChkPtMaxCount     :: Required 13 (Value Int32)
    , cpsChkPtMinCount     :: Required 14 (Value Int32)
    , cpsSubMaxCount       :: Required 15 (Value Int32)
    , cpsNamedConsStrategy :: Optional 16 (Value Text)
    } deriving (Generic, Show)

--------------------------------------------------------------------------------
-- | 'CreatePersistentSubscription' smart constructor.
_createPersistentSubscription :: Text
                              -> Text
                              -> PersistentSubscriptionSettings
                              -> CreatePersistentSubscription
_createPersistentSubscription group stream sett =
    CreatePersistentSubscription
    { cpsGroupName         = putField group
    , cpsStreamId          = putField stream
    , cpsResolveLinkTos    = putField $ psSettingsResolveLinkTos sett
    , cpsStartFrom         = putField $ psSettingsStartFrom sett
    , cpsMsgTimeout        = putField
                             . fromIntegral
                             . (truncate :: Double -> Int64)
                             . timeSpanTotalMillis
                             $ psSettingsMsgTimeout sett
    , cpsRecordStats       = putField $ psSettingsExtraStats sett
    , cpsLiveBufSize       = putField $ psSettingsLiveBufSize sett
    , cpsReadBatchSize     = putField $ psSettingsReadBatchSize sett
    , cpsBufSize           = putField $ psSettingsHistoryBufSize sett
    , cpsMaxRetryCount     = putField $ psSettingsMaxRetryCount sett
    , cpsPreferRoundRobin  = putField False
    , cpsChkPtAfterTime    = putField
                             . fromIntegral
                             . (truncate :: Double -> Int64)
                             . timeSpanTotalMillis
                             $ psSettingsCheckPointAfter sett
    , cpsChkPtMaxCount     = putField $ psSettingsMaxCheckPointCount sett
    , cpsChkPtMinCount     = putField $ psSettingsMinCheckPointCount sett
    , cpsSubMaxCount       = putField $ psSettingsMaxSubsCount sett
    , cpsNamedConsStrategy = putField $ Just strText
    }
  where
    strText = strategyText $ psSettingsNamedConsumerStrategy sett

--------------------------------------------------------------------------------
instance Encode CreatePersistentSubscription

--------------------------------------------------------------------------------
-- | Create persistent subscription outcome.
data CreatePersistentSubscriptionResult
    = CPS_Success
    | CPS_AlreadyExists
    | CPS_Fail
    | CPS_AccessDenied
    deriving (Enum, Eq, Show)

--------------------------------------------------------------------------------
-- | Create persistent subscription response.
data CreatePersistentSubscriptionCompleted =
    CreatePersistentSubscriptionCompleted
    { cpscResult :: Required 1 (Enumeration CreatePersistentSubscriptionResult)
    , cpscReason :: Optional 2 (Value Text)
    } deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode CreatePersistentSubscriptionCompleted

--------------------------------------------------------------------------------
-- | Delete persistent subscription request.
data DeletePersistentSubscription =
    DeletePersistentSubscription
    { dpsGroupName :: Required 1 (Value Text)
    , dpsStreamId  :: Required 2 (Value Text)
    } deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode DeletePersistentSubscription

--------------------------------------------------------------------------------
-- | 'DeletePersistentSubscription' smart construction.
_deletePersistentSubscription :: Text -> Text -> DeletePersistentSubscription
_deletePersistentSubscription group_name stream_id =
    DeletePersistentSubscription
    { dpsGroupName = putField group_name
    , dpsStreamId  = putField stream_id
    }

--------------------------------------------------------------------------------
-- | Delete persistent subscription outcome.
data DeletePersistentSubscriptionResult
    = DPS_Success
    | DPS_DoesNotExist
    | DPS_Fail
    | DPS_AccessDenied
    deriving (Enum, Eq, Show)

--------------------------------------------------------------------------------
-- | Delete persistent subscription response.
data DeletePersistentSubscriptionCompleted =
    DeletePersistentSubscriptionCompleted
    { dpscResult :: Required 1 (Enumeration DeletePersistentSubscriptionResult)
    , dpscReason :: Optional 2 (Value Text)
    } deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode DeletePersistentSubscriptionCompleted

--------------------------------------------------------------------------------
-- | Update persistent subscription request.
data UpdatePersistentSubscription =
    UpdatePersistentSubscription
    { upsGroupName         :: Required 1  (Value Text)
    , upsStreamId          :: Required 2  (Value Text)
    , upsResolveLinkTos    :: Required 3  (Value Bool)
    , upsStartFrom         :: Required 4  (Value Int32)
    , upsMsgTimeout        :: Required 5  (Value Int32)
    , upsRecordStats       :: Required 6  (Value Bool)
    , upsLiveBufSize       :: Required 7  (Value Int32)
    , upsReadBatchSize     :: Required 8  (Value Int32)
    , upsBufSize           :: Required 9  (Value Int32)
    , upsMaxRetryCount     :: Required 10 (Value Int32)
    , upsPreferRoundRobin  :: Required 11 (Value Bool)
    , upsChkPtAfterTime    :: Required 12 (Value Int32)
    , upsChkPtMaxCount     :: Required 13 (Value Int32)
    , upsChkPtMinCount     :: Required 14 (Value Int32)
    , upsSubMaxCount       :: Required 15 (Value Int32)
    , upsNamedConsStrategy :: Optional 16 (Value Text)
    } deriving (Generic, Show)

--------------------------------------------------------------------------------
-- | 'UpdatePersistentSubscription' smart constructor.
_updatePersistentSubscription :: Text
                              -> Text
                              -> PersistentSubscriptionSettings
                              -> UpdatePersistentSubscription
_updatePersistentSubscription group stream sett =
    UpdatePersistentSubscription
    { upsGroupName         = putField group
    , upsStreamId          = putField stream
    , upsResolveLinkTos    = putField $ psSettingsResolveLinkTos sett
    , upsStartFrom         = putField $ psSettingsStartFrom sett
    , upsMsgTimeout        = putField
                             . fromIntegral
                             . (truncate :: Double -> Int64)
                             . timeSpanTotalMillis
                             $ psSettingsMsgTimeout sett
    , upsRecordStats       = putField $ psSettingsExtraStats sett
    , upsLiveBufSize       = putField $ psSettingsLiveBufSize sett
    , upsReadBatchSize     = putField $ psSettingsReadBatchSize sett
    , upsBufSize           = putField $ psSettingsHistoryBufSize sett
    , upsMaxRetryCount     = putField $ psSettingsMaxRetryCount sett
    , upsPreferRoundRobin  = putField False
    , upsChkPtAfterTime    = putField
                             . fromIntegral
                             . (truncate :: Double -> Int64)
                             . timeSpanTotalMillis
                             $ psSettingsCheckPointAfter sett
    , upsChkPtMaxCount     = putField $ psSettingsMaxCheckPointCount sett
    , upsChkPtMinCount     = putField $ psSettingsMinCheckPointCount sett
    , upsSubMaxCount       = putField $ psSettingsMaxSubsCount sett
    , upsNamedConsStrategy = putField $ Just strText
    }
  where
    strText = strategyText $ psSettingsNamedConsumerStrategy sett

--------------------------------------------------------------------------------
instance Encode UpdatePersistentSubscription

--------------------------------------------------------------------------------
-- | Update persistent subscription outcome.
data UpdatePersistentSubscriptionResult
    = UPS_Success
    | UPS_DoesNotExist
    | UPS_Fail
    | UPS_AccessDenied
    deriving (Enum, Eq, Show)

--------------------------------------------------------------------------------
-- | Update persistent subscription response.
data UpdatePersistentSubscriptionCompleted =
    UpdatePersistentSubscriptionCompleted
    { upscResult :: Required 1 (Enumeration UpdatePersistentSubscriptionResult)
    , upscReason :: Optional 2 (Value Text)
    } deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode UpdatePersistentSubscriptionCompleted

--------------------------------------------------------------------------------
-- | Connect to a persistent subscription request.
data ConnectToPersistentSubscription =
    ConnectToPersistentSubscription
    { ctsId                  :: Required 1 (Value Text)
    , ctsStreamId            :: Required 2 (Value Text)
    , ctsAllowedInFlightMsgs :: Required 3 (Value Int32)
    } deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode ConnectToPersistentSubscription

--------------------------------------------------------------------------------
-- | 'ConnectToPersistentSubscription' smart constructor.
_connectToPersistentSubscription :: Text
                                 -> Text
                                 -> Int32
                                 -> ConnectToPersistentSubscription
_connectToPersistentSubscription sub_id stream_id all_fly_msgs =
    ConnectToPersistentSubscription
    { ctsId                  = putField sub_id
    , ctsStreamId            = putField stream_id
    , ctsAllowedInFlightMsgs = putField all_fly_msgs
    }

--------------------------------------------------------------------------------
-- | Ack processed events request.
data PersistentSubscriptionAckEvents =
    PersistentSubscriptionAckEvents
    { psaeId              :: Required 1 (Value Text)
    , psaeProcessedEvtIds :: Repeated 2 (Value ByteString)
    } deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode PersistentSubscriptionAckEvents

--------------------------------------------------------------------------------
-- | 'PersistentSubscriptionAckEvents' smart constructor.
persistentSubscriptionAckEvents :: Text
                                -> [ByteString]
                                -> PersistentSubscriptionAckEvents
persistentSubscriptionAckEvents sub_id evt_ids =
    PersistentSubscriptionAckEvents
    { psaeId              = putField sub_id
    , psaeProcessedEvtIds = putField evt_ids
    }

--------------------------------------------------------------------------------
-- | Gathers every possible Nak actions.
data NakAction
    = NA_Unknown
    | NA_Park
    | NA_Retry
    | NA_Skip
    | NA_Stop
    deriving (Enum, Eq, Show)

--------------------------------------------------------------------------------
-- | Nak processed events request.
data PersistentSubscriptionNakEvents =
    PersistentSubscriptionNakEvents
    { psneId              :: Required 1 (Value Text)
    , psneProcessedEvtIds :: Repeated 2 (Value ByteString)
    , psneMsg             :: Optional 3 (Value Text)
    , psneAction          :: Required 4 (Enumeration NakAction)
    } deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode PersistentSubscriptionNakEvents

--------------------------------------------------------------------------------
-- | 'PersistentSubscriptionNakEvents' smart constructor.
persistentSubscriptionNakEvents :: Text
                                -> [ByteString]
                                -> Maybe Text
                                -> NakAction
                                -> PersistentSubscriptionNakEvents
persistentSubscriptionNakEvents sub_id evt_ids msg action =
    PersistentSubscriptionNakEvents
    { psneId              = putField sub_id
    , psneProcessedEvtIds = putField evt_ids
    , psneMsg             = putField msg
    , psneAction          = putField action
    }

--------------------------------------------------------------------------------
-- | Connection to persistent subscription response.
data PersistentSubscriptionConfirmation =
    PersistentSubscriptionConfirmation
    { pscLastCommitPos :: Required 1 (Value Int64)
    , pscId            :: Required 2 (Value Text)
    , pscLastEvtNumber :: Optional 3 (Value Int32)
    } deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode PersistentSubscriptionConfirmation

--------------------------------------------------------------------------------
-- | Avalaible event sent by the server in the context of a persistent
--   subscription..
data PersistentSubscriptionStreamEventAppeared =
    PersistentSubscriptionStreamEventAppeared
    { psseaEvt :: Required 1 (Message ResolvedIndexedEvent) }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode PersistentSubscriptionStreamEventAppeared
