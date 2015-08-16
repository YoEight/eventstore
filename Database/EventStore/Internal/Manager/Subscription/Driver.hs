{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Manager.Subscription.Driver
-- Copyright : (C) 2015 Yorick Laupa
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
    , packageArrived
    , connectToStream
    , connectToPersist
    , createPersist
    , updatePersist
    , deletePersist
    , ackPersist
    , nakPersist
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
import Database.EventStore.Internal.Manager.Subscription.Packages
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
packageArrived :: Package -> Driver -> Maybe (SubEvent, Driver)
packageArrived pkg (Driver k) = k (PackageArrived pkg)

--------------------------------------------------------------------------------
connectToStream :: Text -> Bool -> UUID -> Driver -> (Package, Driver)
connectToStream s t u (Driver k) = k (ExecuteCmd $ ConnectReg s t u)

--------------------------------------------------------------------------------
connectToPersist :: Text -> Text -> Int32 -> UUID -> Driver -> (Package, Driver)
connectToPersist g s b u (Driver k) = k (ExecuteCmd $ ConnectPersist g s b u)

--------------------------------------------------------------------------------
createPersist :: Text
              -> Text
              -> PersistentSubscriptionSettings
              -> UUID 
              -> Driver
              -> (Package, Driver)
createPersist g s ss u (Driver k) =
    k (ExecuteCmd $ ApplyPersistAction g s u (PersistCreate ss))

--------------------------------------------------------------------------------
updatePersist :: Text
              -> Text
              -> PersistentSubscriptionSettings
              -> UUID 
              -> Driver
              -> (Package, Driver)
updatePersist g s ss u (Driver k) =
    k (ExecuteCmd $ ApplyPersistAction g s u (PersistUpdate ss))

--------------------------------------------------------------------------------
deletePersist :: Text -> Text -> UUID -> Driver -> (Package, Driver)
deletePersist g s u (Driver k) =
    k (ExecuteCmd $ ApplyPersistAction g s u PersistDelete)

--------------------------------------------------------------------------------
ackPersist :: Id 'PersistType -> Text -> [UUID] -> Driver -> (Package, Driver)
ackPersist i sid evts (Driver k) = k (ExecuteCmd $ PersistAck i sid evts)

--------------------------------------------------------------------------------
nakPersist :: Id 'PersistType
           -> Text
           -> NakAction
           -> Maybe Text
           -> [UUID]
           -> Driver
           -> (Package, Driver)
nakPersist i sid na mt evts (Driver k) =
    k (ExecuteCmd $ PersistNak i sid na mt evts)

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
    ExecuteCmd     :: Command -> Input (Package, Driver)

--------------------------------------------------------------------------------
data Command
    = ConnectReg Text Bool UUID
    | ConnectPersist Text Text Int32 UUID
    | ApplyPersistAction Text Text UUID PersistAction
    | PersistAck (Id 'PersistType) Text [UUID]
    | PersistNak (Id 'PersistType) Text NakAction (Maybe Text) [UUID]

--------------------------------------------------------------------------------
newtype Driver = Driver (forall a. Input a -> a)

--------------------------------------------------------------------------------
newDriver :: Settings -> Driver
newDriver setts = Driver $ runDriver setts newModel

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

    go m (ExecuteCmd c) =
        case c of
            ConnectReg s tos u ->
                let pkg   = createConnectRegularPackage setts u s tos
                    nxt_m = runModel (connectReg s tos u) m in
                (pkg, Driver $ go nxt_m)
            ConnectPersist g n b u ->
                let pkg   = createConnectPersistPackage setts u g n b
                    nxt_m = runModel (connectPersist g n b u) m in
                (pkg, Driver $ go nxt_m)
            ApplyPersistAction g n u a ->
                let pkg   = createPersistActionPackage setts u g n a
                    nxt_m = runModel (persistAction g n u a) m in
                (pkg, Driver $ go nxt_m)
            PersistAck (PersistId u) sid evts ->
                let pkg = createAckPackage setts u sid evts in
                (pkg, Driver $ go m)
            PersistNak (PersistId u) sid na r evts ->
                let pkg = createNakPackage setts u sid na r evts in
                (pkg, Driver $ go m)

--------------------------------------------------------------------------------
maybeDecodeMessage :: Decode a => ByteString -> Maybe a
maybeDecodeMessage bytes =
    case runGet decodeMessage bytes of
        Right a -> Just a
        _       -> Nothing
