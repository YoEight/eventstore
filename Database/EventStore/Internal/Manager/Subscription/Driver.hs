{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Manager.Subscription.Driver
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Manager.Subscription.Driver
    ( SubEvent(..)
    , SubDropReason(..)
    , Driver
    , newDriver
    , handlePackage
    ) where

--------------------------------------------------------------------------------
import Data.Int
import Data.Maybe

--------------------------------------------------------------------------------
import Data.ByteString
import Data.Serialize
import Data.ProtocolBuffers
import Data.Text
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Manager.Subscription.Message
import Database.EventStore.Internal.Manager.Subscription.Model
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data SubEvent
    = forall t. EventAppeared (Id t) ResolvedEvent
    | forall t. SubDropped UUID (Running t) SubDropReason
    | forall t. SubConfirmed (Running t)
    | PersistActionConfirmed ConfirmedAction
    | PersistActionFailed UUID PendingAction PersistActionException

--------------------------------------------------------------------------------
data SubDropReason
    = SubUnsubscribed
    | SubAccessDenied
    | SubNotFound
    | SubPersistDeleted
    deriving (Show, Eq)

--------------------------------------------------------------------------------
data PersistActionException
    = PersistActionFail
    | PersistActionAlreadyExist
    | PersistActionDoesNotExist
    | PersistActionAccessDenied

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
toSubDropReason :: DropReason -> SubDropReason
toSubDropReason D_Unsubscribed                  = SubUnsubscribed
toSubDropReason D_NotFound                      = SubNotFound
toSubDropReason D_AccessDenied                  = SubAccessDenied
toSubDropReason D_PersistentSubscriptionDeleted = SubPersistDeleted

--------------------------------------------------------------------------------
data Input a where
    PackageArrived :: Package -> Input (Maybe (SubEvent, Driver))
    ExecuteCommand :: Command -> Input (Package, Driver)

--------------------------------------------------------------------------------
data Command
    = ConnectReg Text Bool UUID
    | ConnectPersist Text Text Int32 UUID

--------------------------------------------------------------------------------
newtype Driver = Driver (forall a. Input a -> a)

--------------------------------------------------------------------------------
newDriver :: Settings -> Driver
newDriver setts = Driver $ runDriver setts newModel

--------------------------------------------------------------------------------
handlePackage :: Package -> Driver -> Maybe (SubEvent, Driver)
handlePackage pkg (Driver k) = k (PackageArrived pkg)

--------------------------------------------------------------------------------
runDriver :: Settings -> Model -> Input a -> a
runDriver setts = go
  where
    go :: forall a. Model -> Input a -> a
    go m (PackageArrived Package{..}) =
        case packageCmd of
            0xC2 -> do
                r   <- runModel (queryRegSub packageCorrelation) m
                msg <- maybeDecodeMessage packageData
                let e   = getField $ streamResolvedEvent msg
                    evt = newResolvedEventFromBuf e
                return (EventAppeared (runningId r) evt, Driver $ go m)

            0xC7 -> do
                r   <- runModel (queryPersistSub packageCorrelation) m
                msg <- maybeDecodeMessage packageData
                let e   = getField $ psseaEvt msg
                    evt = newResolvedEvent e
                return (EventAppeared (runningId r) evt, Driver $ go m)

            0xC1 -> do
                msg <- maybeDecodeMessage packageData
                let lcp  = getField $ subscribeLastCommitPos msg
                    len  = getField $ subscribeLastEventNumber msg
                    meta = RegularMeta lcp len
                    cmd  = confirmConnect packageCorrelation meta
                (r, nxt_m) <- runModel cmd m
                return (SubConfirmed r, Driver $ go nxt_m)

            0xC6 -> do
                msg <- maybeDecodeMessage packageData
                let lcp  = getField $ pscLastCommitPos msg
                    sid  = getField $ pscId msg
                    len  = getField $ pscLastEvtNumber msg
                    meta = PersistMeta sid lcp len
                    cmd  = confirmConnect packageCorrelation meta
                (r, nxt_m) <- runModel cmd m
                return (SubConfirmed r, Driver $ go nxt_m)

            0xC9 -> confirmPAction (getField . cpscResult) createRException
            0xCF -> confirmPAction (getField . upscResult) updateRException
            0xCB -> confirmPAction (getField . dpscResult) deleteRException

            0xC4 -> do
                Box r <- runModel (querySomeSub packageCorrelation) m
                msg   <- maybeDecodeMessage packageData
                let reason  = fromMaybe D_Unsubscribed $ getField $ dropReason msg
                    nxt_m   = runModel (unsub $ runningId r) m
                    dreason = toSubDropReason reason
                    evt     = SubDropped packageCorrelation r dreason
                return (evt, Driver $ go nxt_m)
            _ -> Nothing
      where
        confirmPAction :: Decode m
                      => (m -> r)
                      -> (r -> Maybe PersistActionException)
                      -> Maybe (SubEvent, Driver)
        confirmPAction fd em = do
            msg <- maybeDecodeMessage packageData
            case em $ fd msg of
                Just e -> do
                    p <- runModel (queryAction packageCorrelation) m
                    let evt = PersistActionFailed packageCorrelation p e
                    return (evt, Driver $ go m)
                Nothing -> do
                    let cmd = confirmAction packageCorrelation
                    (c, nxt_m) <- runModel cmd m
                    return (PersistActionConfirmed c, Driver $ go nxt_m)

    go m (ExecuteCommand c) =
        case c of
            ConnectReg s tos u ->
                let pkg   = createConnectRegularPackage setts u s tos
                    nxt_m = runModel (connectReg s tos u) m in
                (pkg, Driver $ go nxt_m)
            ConnectPersist g n b u ->
                let pkg   = createConnectPersistPackage setts u g n b
                    nxt_m = runModel (connectPersist g n b u) m in
                (pkg, Driver $ go nxt_m)

--------------------------------------------------------------------------------
maybeDecodeMessage :: Decode a => ByteString -> Maybe a
maybeDecodeMessage bytes =
    case runGet decodeMessage bytes of
        Right a -> Just a
        _       -> Nothing

--------------------------------------------------------------------------------
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