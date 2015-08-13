{-# LANGUAGE GADTs                     #-}
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
    , Driver
    , newDriver
    , handlePackage
    ) where

--------------------------------------------------------------------------------
import Data.ByteString
import Data.Serialize
import Data.ProtocolBuffers
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Manager.Subscription.Message
import Database.EventStore.Internal.Manager.Subscription.Model
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data SubEvent
    = forall t. EventAppeared (Id t) ResolvedEvent
    | forall t. SubDropped (Id t)
    | forall t. SubConfirmed (Id t)
    | PersistActionConfirmed ConfirmedAction
    | PersistActionFailed UUID PendingAction PersistActionException

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
data Input a where
    PackageArrived :: Package -> Input (Maybe (SubEvent, Driver))

--------------------------------------------------------------------------------
newtype Driver = Driver (forall a. Input a -> a)

--------------------------------------------------------------------------------
newDriver :: Driver
newDriver = Driver $ runDriver newModel

--------------------------------------------------------------------------------
handlePackage :: Package -> Driver -> Maybe (SubEvent, Driver)
handlePackage pkg (Driver k) = k (PackageArrived pkg)

--------------------------------------------------------------------------------
runDriver :: Model -> Input a -> a
runDriver m (PackageArrived Package{..}) =
    case packageCmd of
        0xC2 -> do
            let sid   = RegularId packageCorrelation
                query = Query . SelectSub $ sid
            _   <- runModel query m
            msg <- maybeDecodeMessage packageData
            let e   = getField $ streamResolvedEvent msg
                evt = newResolvedEventFromBuf e
            return (EventAppeared sid evt, Driver $ runDriver m)

        0xC7 -> do
            let sid   = PersistId packageCorrelation
                query = Query . SelectSub $ sid
            _   <- runModel query m
            msg <- maybeDecodeMessage packageData
            let e   = getField $ psseaEvt msg
                evt = newResolvedEvent e
            return (EventAppeared sid evt, Driver $ runDriver m)

        0xC1 -> do
            msg <- maybeDecodeMessage packageData
            let lcp  = getField $ subscribeLastCommitPos msg
                len  = getField $ subscribeLastEventNumber msg
                meta = RegularMeta lcp len
                req  = Execute $ Confirm $ ConfirmSub packageCorrelation meta
            (sid, nxt_m) <- runModel req m
            return (SubConfirmed sid, Driver $ runDriver nxt_m)

        0xC6 -> do
            msg <- maybeDecodeMessage packageData
            let lcp  = getField $ pscLastCommitPos msg
                sid  = getField $ pscId msg
                len  = getField $ pscLastEvtNumber msg
                meta = PersistMeta sid lcp len
                req  = Execute $ Confirm $ ConfirmSub packageCorrelation meta
            (sidt, nxt_m) <- runModel req m
            return (SubConfirmed sidt, Driver $ runDriver nxt_m)

        0xC9 -> handlePersistActionConfirmation (getField . cpscResult)
                                                createRException
                                                packageCorrelation
                                                packageData m

        0xCF -> handlePersistActionConfirmation (getField . upscResult)
                                                updateRException
                                                packageCorrelation
                                                packageData m

        0xCB -> handlePersistActionConfirmation (getField . dpscResult)
                                                deleteRException
                                                packageCorrelation
                                                packageData m

        _ -> Nothing

--------------------------------------------------------------------------------
handlePersistActionConfirmation :: Decode msg
                                => (msg -> r)
                                -> (r -> Maybe PersistActionException)
                                -> UUID
                                -> ByteString
                                -> Model
                                -> Maybe (SubEvent, Driver)
handlePersistActionConfirmation fd em u bytes m = do
    msg <- maybeDecodeMessage bytes
    case em $ fd msg of
        Just e -> do
            let query = Query . SelectAction $ u
            p <- runModel query m
            return (PersistActionFailed u p e, Driver $ runDriver m)
        Nothing -> do
            let cmd = Execute . Confirm . ConfirmAction $ u
            (c, nxt_m) <- runModel cmd m
            return (PersistActionConfirmed c, Driver $ runDriver nxt_m)

--------------------------------------------------------------------------------
maybeDecodeMessage :: Decode a => ByteString -> Maybe a
maybeDecodeMessage bytes =
    case runGet decodeMessage bytes of
        Right a -> Just a
        _       -> Nothing