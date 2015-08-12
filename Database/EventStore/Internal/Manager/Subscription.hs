{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_GHC -fcontext-stack=26     #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Manager.Subscription
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Manager.Subscription where

--------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Fix
import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.Foldable
import           Data.Functor
import qualified Data.HashMap.Strict as H
import           Data.Int
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Typeable
import           Prelude

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Serialize
import Data.Text hiding (group)
import Data.UUID
import FRP.Sodium
import System.Random

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation.ReadStreamEventsOperation
import Database.EventStore.Internal.Types hiding (Event, newEvent)
import Database.EventStore.Internal.Util.Sodium
import Database.EventStore.Internal.Manager.Subscription.Driver
import Database.EventStore.Internal.Manager.Subscription.Message
import Database.EventStore.Internal.Manager.Subscription.Model

--------------------------------------------------------------------------------
data Sub a where
    RegularSub    :: Text -> Bool -> Sub Regular
    PersistentSub :: Text -> Text -> Int32 -> Sub Persistent

--------------------------------------------------------------------------------
data Pending =
    forall s. Push s =>
    Pending
    { _penId  :: !UUID
    , _penSub :: !(Sub s)
    , _penCb  :: Subscription s -> IO ()
    }

--------------------------------------------------------------------------------
data Confirmed =
    forall s. Push s =>
    Confirmed
    { _conId  :: !UUID
    , _conTyp :: !(Sub s)
    , _conCB  :: Subscription s -> IO ()
    , _conSub :: !(IO (Subscription s))
    }

--------------------------------------------------------------------------------
data OnGoing =
    forall s. Push s =>
    OnGoing
    { _ongTyp :: !(Sub s)
    , _ongSub :: !(Subscription s)
    }

--------------------------------------------------------------------------------
-- | Value's type returned when calling 'subNextEvent'
type family NextEvent a :: * where
    NextEvent Regular    = Either DropReason ResolvedEvent
    NextEvent Persistent = Either DropReason ResolvedEvent
    NextEvent Catchup    = Either CatchupError ResolvedEvent

--------------------------------------------------------------------------------
-- | Represents a subscription to a stream.
data Subscription a =
    Subscription
    { subStreamId :: !Text
      -- ^ The name of the stream to which the subscription is subscribed.
    , subUnsubscribe :: !(IO ())
      -- ^ Asynchronously unsubscribe from the the stream.
    , subNextEvent :: !(IO (NextEvent a))
      -- ^ Awaits for the next event.
    , subIsSubscribedToAll :: !Bool
      -- ^ True if this subscription is to $all stream.
    , _subInternal :: !a
    }

--------------------------------------------------------------------------------
-- | Internal use only because we all know that lawless type-classes are bad.
--   But Haskell clearly lacks of a proper module system. So meanwhile, we use
--   type-class as a (hacky) way to have a bit of modularity.
class Push a where
    _pushEvt :: a -> Either DropReason ResolvedEvent -> IO ()

--------------------------------------------------------------------------------
_subPushEvt :: Push a
            => Subscription a
            -> Either DropReason ResolvedEvent
            -> IO ()
_subPushEvt = _pushEvt . _subInternal

--------------------------------------------------------------------------------
-- | Represents a subscription that is directly identifiable. 'Regular' and
--   'Persistent' fit that description while 'Catchup' doesn't. Because
--   'Catchup' reads all events from a particular checkpoint and when it's
--   finished, it issues a subscription request.
class Identifiable a where
    _getId              :: a -> UUID
    _getLastCommitPos   :: a -> Int64
    _getLastEventNumber :: a -> Maybe Int32

--------------------------------------------------------------------------------
-- | Gets the ID of the subscription.
subId :: Identifiable a => Subscription a -> UUID
subId = _getId . _subInternal

--------------------------------------------------------------------------------
-- | The last commit position seen on the subscription (if this a subscription
--   to $all stream).
subLastCommitPos :: Identifiable a => Subscription a -> Int64
subLastCommitPos = _getLastCommitPos . _subInternal

--------------------------------------------------------------------------------
-- | The last event number seen on the subscription (if this is a subscription
--   to a single stream).
subLastEventNumber :: Identifiable a => Subscription a -> Maybe Int32
subLastEventNumber = _getLastEventNumber . _subInternal

--------------------------------------------------------------------------------
-- | Represents a subscription to a single stream or $all stream in the
--   EventStore.
data Regular =
    Regular
    { _regId              :: !UUID
    , _regResolveLinkTos  :: !Bool
    , _regLastCommitPos   :: !Int64
    , _regLastEventNumber :: !(Maybe Int32)
    , _regChan            :: !(Chan (Either DropReason ResolvedEvent))
    }

--------------------------------------------------------------------------------
instance Identifiable Regular where
    _getId              = _regId
    _getLastCommitPos   = _regLastCommitPos
    _getLastEventNumber = _regLastEventNumber

--------------------------------------------------------------------------------
instance Push Regular where
    _pushEvt reg = writeChan (_regChan reg)

--------------------------------------------------------------------------------
-- | Determines whether or not any link events encontered in the stream will be
--   resolved.
subResolveLinkTos :: Subscription Regular -> Bool
subResolveLinkTos Subscription { _subInternal = reg } = _regResolveLinkTos reg

--------------------------------------------------------------------------------
-- | Errors that could arise during a catch-up subscription. 'Text' value
--   represents the stream name.
data CatchupError
    = CatchupStreamDeleted Text
    | CatchupUnexpectedStreamStatus Text ReadStreamResult
    | CatchupSubscriptionDropReason Text DropReason
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception CatchupError

--------------------------------------------------------------------------------
-- | Represents catch-up subscription.
data Catchup = Catchup { _catchupSub :: MVar (Subscription Regular) }

--------------------------------------------------------------------------------
-- | Waits until 'Catchup' subscription catch-up its stream.
waitTillCatchup :: Subscription Catchup -> IO ()
waitTillCatchup Subscription { _subInternal = Catchup mvar } = do
    _ <- readMVar mvar
    return ()

--------------------------------------------------------------------------------
-- | Non blocking version of `waitTillCatchup`.
hasCaughtUp :: Subscription Catchup -> IO Bool
hasCaughtUp Subscription { _subInternal = Catchup mvar } =
    fmap isJust $ tryReadMVar mvar

--------------------------------------------------------------------------------
-- | Represents a persistent subscription.
data Persistent =
    Persistent
    { _persistId       :: !UUID
    , _persistChan     :: !(Chan (Either DropReason ResolvedEvent))
    , _persistSubId    :: !Text
    , _persistGroup    :: !Text
    , _persistLastCPos :: !Int64
    , _persistLastENum :: !(Maybe Int32)
    , _persistAckCmd   :: AckCmd -> IO ()
    }

--------------------------------------------------------------------------------
instance Identifiable Persistent where
    _getId              = _persistId
    _getLastCommitPos   = _persistLastCPos
    _getLastEventNumber = _persistLastENum

--------------------------------------------------------------------------------
instance Push Persistent where
    _pushEvt p = writeChan (_persistChan p)

--------------------------------------------------------------------------------
-- | Acknowledges those event ids have been successfully processed.
notifyEventsProcessed :: Subscription Persistent -> [UUID] -> IO ()
notifyEventsProcessed sub eids = _persistAckCmd p (AckCmd eids)
  where
    p = _subInternal sub

--------------------------------------------------------------------------------
-- | Acknowledges those event ids have failed to be processed successfully.
notifyEventsFailed :: Subscription Persistent
                   -> NakAction
                   -> Maybe Text
                   -> [UUID]
                   -> IO ()
notifyEventsFailed sub act msg eids = _persistAckCmd p (NakCmd act msg eids)
  where
    p = _subInternal sub

--------------------------------------------------------------------------------
data AckCmd
    = AckCmd [UUID]
    | NakCmd NakAction (Maybe Text) [UUID]

--------------------------------------------------------------------------------
data PendingPersistAction =
    PendingPersistAction
    { _ppaId     :: !UUID
    , _ppaGroup  :: !Text
    , _ppaStream :: !Text
    , _ppaTyp    :: !PersistAction
    , _ppaCB     :: Either OperationException () -> IO ()
    }

--------------------------------------------------------------------------------
data PersistActionConfirmed =
    PersistActionConfirmed
    { _pacId     :: !UUID
    , _pacResult :: !(Either OperationException ())
    , _pacCB     :: Either OperationException () -> IO ()
    }

--------------------------------------------------------------------------------
data Manager
    = Manager
      { _pendings              :: !(M.Map UUID Pending)
      , _ongoings              :: !(M.Map UUID OnGoing)
      , _pendingPersistActions :: !(M.Map UUID PendingPersistAction)
      }

--------------------------------------------------------------------------------
-- data RegularMeta =
--     RegularMeta
--     { _regMetaId         :: !UUID
--     , _regMetaStream     :: !Text
--     , _regMetaTos        :: !Bool
--     , _regMetaLastCommit :: !Int64
--     , _regMetaLastEvent  :: !Int32
--     }

--------------------------------------------------------------------------------
 -- type family SubId a :: * where
 --    SubId RegularType = RegularSubId
 --    SubId PersistType = PersistSubId

--------------------------------------------------------------------------------
initManager :: Manager
initManager =
    Manager
    { _pendings              = M.empty
    , _ongoings              = M.empty
    , _pendingPersistActions = M.empty
    }

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
maybeDecodeMessage :: Decode a => ByteString -> Maybe a
maybeDecodeMessage bytes =
    case runGet decodeMessage bytes of
        Right a -> Just a
        _       -> Nothing

--------------------------------------------------------------------------------
unsafeDecodeMessage :: Decode a => ByteString -> a
unsafeDecodeMessage bytes =
    case runGet decodeMessage bytes of
        Right a -> a
        Left  e -> error $ "decoding error: " ++ e

--------------------------------------------------------------------------------
data Appeared =
    forall s. Push s =>
    Appeared
    { _appSub :: !(Subscription s)
    , _appEvt :: !ResolvedEvent
    }

--------------------------------------------------------------------------------
onEventAppeared :: Package -> Manager -> Maybe Appeared
onEventAppeared Package{..} Manager{..} =
    case M.lookup packageCorrelation _ongoings of
        Just (OnGoing typ sub) ->
            case (packageCmd, typ) of
                (0xC2, RegularSub _ _ ) ->
                    let msg = unsafeDecodeMessage packageData
                        evt = getField $ streamResolvedEvent msg in
                     Just $ Appeared sub (newResolvedEventFromBuf evt)
                (0xC7, PersistentSub _ _ _) ->
                    let msg = unsafeDecodeMessage packageData
                        evt = getField $ psseaEvt msg in
                    Just $ Appeared sub (newResolvedEvent evt)
                _ -> Nothing
        _ -> Nothing

--------------------------------------------------------------------------------
confirmSub :: (UUID -> IO ())
           -> (UUID -> Text -> AckCmd -> IO ())
           -> Package
           -> Manager
           -> Maybe Confirmed
confirmSub unsub ackF Package{..} Manager{..} =
    case M.lookup packageCorrelation _pendings of
        Just (Pending _ typ cb) ->
            case (packageCmd, typ) of
                (0xC1, RegularSub stream tos) ->
                    let !msg = unsafeDecodeMessage packageData
                        lcp  = getField $ subscribeLastCommitPos msg
                        len  = getField $ subscribeLastEventNumber msg in
                    Just $ Confirmed packageCorrelation typ cb $ do
                        chan <- newChan
                        let reg = Regular
                                  { _regId              = packageCorrelation
                                  , _regResolveLinkTos  = tos
                                  , _regLastCommitPos   = lcp
                                  , _regLastEventNumber = len
                                  , _regChan            = chan
                                  }

                        return Subscription
                               { subStreamId          = stream
                               , subUnsubscribe       = unsub packageCorrelation
                               , subNextEvent         = readChan chan
                               , subIsSubscribedToAll = stream == ""
                               , _subInternal         = reg
                               }
                (0xC6, PersistentSub grp stream _) ->
                    let !msg = unsafeDecodeMessage packageData
                        lcp  = getField $ pscLastCommitPos msg
                        sid  = getField $ pscId msg
                        len  = getField $ pscLastEvtNumber msg in
                    Just $ Confirmed packageCorrelation typ cb $ do
                        chan <- newChan
                        let pes = Persistent
                                  { _persistId       = packageCorrelation
                                  , _persistChan     = chan
                                  , _persistSubId    = sid
                                  , _persistGroup    = grp
                                  , _persistLastCPos = lcp
                                  , _persistLastENum = len
                                  , _persistAckCmd   = \cmd ->
                                    ackF packageCorrelation sid cmd
                                  }

                        return Subscription
                               { subStreamId          = stream
                               , subUnsubscribe       = unsub packageCorrelation
                               , subNextEvent         = readChan chan
                               , subIsSubscribedToAll = False
                               , _subInternal         = pes
                               }
                _ -> Nothing
        _ -> Nothing

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------
data Subscribe
    = Subscribe
      { _subId             :: !UUID
      , _subCallback       :: Subscription Regular -> IO ()
      , _subStream         :: !Text
      , _subResolveLinkTos :: !Bool
      }

--------------------------------------------------------------------------------
data RegisterSub = forall s. Push s => RegisterSub UUID (Sub s) (Subscription s)

--------------------------------------------------------------------------------
-- Commands
--------------------------------------------------------------------------------
data SubCommand
    = forall s. Push s => SubscribeTo (Sub s) (Subscription s -> IO ())
    | SubmitPersistAction Text
                          Text
                          PersistAction
                          (Either OperationException () -> IO ())

--------------------------------------------------------------------------------
subscriptionNetwork :: Settings
                    -> (Package -> Reactive ())
                    -> Event Package
                    -> Reactive (SubCommand -> IO ())
subscriptionNetwork sett push_pkg e_pkg = do
    -- When a subscription request has been submitted by the user.
    (on_sub, push_sub) <- newEvent

    -- When a subscription has been confirmed by EventStore and we succesfully
    -- create a `forall s. Push s => Subscription s` object.
    (on_reg_sub, push_reg_sub) <- newEvent

    -- When a persist action has been emitted by the user.
    (on_persist_action, push_persist_action) <- newEvent

    let push_pkg_io = pushAsync push_pkg
        push_ack_cmd uuid sid cmd =
            push_pkg_io $ createAckCmdPackage sett uuid sid cmd
    mgr_b <- mfix $ \mgr_b -> do
        let send_unsub = push_pkg_io . createUnsubscribePackage sett

            on_con_sub = filterJust $ snapshot (confirmSub send_unsub
                                                           push_ack_cmd)
                                               e_pkg mgr_b

            on_drop = filterJust $ snapshot dropError e_pkg mgr_b

            on_persist_action_cfrm =
                filterJust $ snapshot onPersistActionConfirmed e_pkg mgr_b

            mgr_e = fmap confirmed on_reg_sub               <>
                    fmap subscribeRequest on_sub            <>
                    fmap newPersistAction on_persist_action <>
                    fmap dropped on_drop                    <>
                    fmap persistActionConfirmed on_persist_action_cfrm

        _ <- listen on_drop $ \(Dropped reason sub _) ->
          _subPushEvt sub (Left reason)

        _ <- listen on_persist_action_cfrm $ \(PersistActionConfirmed _ res k) ->
          k res

        _ <- listen on_con_sub $ \(Confirmed uuid typ cb action) -> do
          sub <- action
          _   <- forkIO $ sync $ push_reg_sub (RegisterSub uuid typ sub)
          cb sub

        accum initManager mgr_e

    let on_app  = filterJust $ snapshot onEventAppeared e_pkg mgr_b


        runSubCommand (SubscribeTo typ cb) = do
            uuid <- randomIO
            let sub = Pending
                      { _penId  = uuid
                      , _penSub = typ
                      , _penCb  = cb
                      }
            void $ forkIO $ sync $ push_sub sub
        runSubCommand (SubmitPersistAction group stream typ cb) = do
            uuid <- randomIO
            let action = PendingPersistAction
                         { _ppaId     = uuid
                         , _ppaGroup  = group
                         , _ppaStream = stream
                         , _ppaTyp    = typ
                         , _ppaCB     = cb
                         }
            void $ forkIO $ sync $ push_persist_action action
    _ <- listen on_sub (push_pkg_io . createSubscriptionPackage sett)

    _ <- listen on_persist_action (push_pkg_io . createPersistActionPkg sett)

    _ <- listen on_app $ \(Appeared sub evt) ->
        _subPushEvt sub (Right evt)


    return runSubCommand

--------------------------------------------------------------------------------
createSubscriptionPackage :: Settings -> Pending -> Package
createSubscriptionPackage sett (Pending uuid typ _) =
    case typ of
        RegularSub stream tos ->
            createConnectRegularPackage sett uuid stream tos
        PersistentSub grp str bufSize ->
            createConnectPersistPackage sett uuid grp str bufSize

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
createConnectPersistPackage Settings{..} uuid group stream bufSize =
    Package
    { packageCmd         = 0xC5
    , packageCorrelation = uuid
    , packageData        = runPut $ encodeMessage msg
    , packageCred        = s_credentials
    }
  where
    msg = _connectToPersistentSubscription group stream bufSize

--------------------------------------------------------------------------------
createAckCmdPackage :: Settings -> UUID -> Text -> AckCmd -> Package
createAckCmdPackage sett uuid sid cmd =
    case cmd of
        AckCmd eids         -> createAckPackage sett uuid sid eids
        NakCmd act msg eids -> createNakPackage sett uuid sid act msg eids

--------------------------------------------------------------------------------
createAckPackage :: Settings -> UUID -> Text -> [UUID] -> Package
createAckPackage Settings{..} corr sid eids =
    Package
    { packageCmd         = 0xCC
    , packageCorrelation = corr
    , packageData        = runPut $ encodeMessage msg
    , packageCred        = s_credentials
    }
  where
    bytes = toStrict $ foldMap toByteString eids
    msg   = persistentSubscriptionAckEvents sid bytes

--------------------------------------------------------------------------------
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
    bytes = toStrict $ foldMap toByteString eids
    msg   = persistentSubscriptionNakEvents sid bytes txt act

--------------------------------------------------------------------------------
createUnsubscribePackage :: Settings -> UUID -> Package
createUnsubscribePackage Settings{..} uuid =
    Package
    { packageCmd         = 0xC3
    , packageCorrelation = uuid
    , packageData        = runPut $ encodeMessage UnsubscribeFromStream
    , packageCred        = s_credentials
    }

--------------------------------------------------------------------------------
createPersistActionPkg :: Settings -> PendingPersistAction -> Package
createPersistActionPkg Settings{..} (PendingPersistAction aId grp strm typ _) =
    Package
    { packageCmd         = cmd
    , packageCorrelation = aId
    , packageData        = runPut msg
    , packageCred        = s_credentials
    }
  where
    msg =
        case typ of
            PersistCreate sett ->
                encodeMessage $ _createPersistentSubscription grp strm sett
            PersistUpdate sett ->
                encodeMessage $ _updatePersistentSubscription grp strm sett
            PersistDelete ->
                encodeMessage $ _deletePersistentSubscription grp strm
    cmd =
        case typ of
            PersistCreate _  -> 0xC8
            PersistUpdate _  -> 0xCE
            PersistDelete    -> 0xCA

--------------------------------------------------------------------------------
createPersistActionPackage :: Settings -> UUID -> PendingAction -> Package
createPersistActionPackage Settings{..} u (PendingAction grp strm tpe) =
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
createPersistPackage :: Settings
                     -> UUID
                     -> Text
                     -> Text
                     -> PersistentSubscriptionSettings
                     -> Package
createPersistPackage Settings{..} uuid g n sett =
    Package
    { packageCmd         = 0xC8
    , packageCorrelation = uuid
    , packageData        = runPut msg
    , packageCred        = s_credentials
    }
  where
    msg = encodeMessage $  _createPersistentSubscription g n sett

--------------------------------------------------------------------------------
updatePersistPackage :: Settings
                     -> UUID
                     -> Text
                     -> Text
                     -> PersistentSubscriptionSettings
                     -> Package
updatePersistPackage Settings{..} uuid g n sett =
    Package
    { packageCmd         = 0xCE
    , packageCorrelation = uuid
    , packageData        = runPut msg
    , packageCred        = s_credentials
    }
  where
    msg = encodeMessage $ _updatePersistentSubscription g n sett

--------------------------------------------------------------------------------
createPersistUnsubPackage :: Settings -> Text -> Text -> UUID -> Package
createPersistUnsubPackage Settings{..} g n uuid =
    Package
    { packageCmd         = 0xCA
    , packageCorrelation = uuid
    , packageData        = runPut msg
    , packageCred        = s_credentials
    }
  where
    msg = encodeMessage $ _deletePersistentSubscription g n


--------------------------------------------------------------------------------
data Dropped =
    forall s. Push s =>
    Dropped
    { droppedReason :: !DropReason
    , droppedSub    :: !(Subscription s)
    , droppedId     :: !UUID
    }

--------------------------------------------------------------------------------
dropError :: Package -> Manager -> Maybe Dropped
dropError Package{..} Manager{..}
    | packageCmd == 0xC4 = do
         OnGoing _ sub <- M.lookup packageCorrelation _ongoings
         msg           <- maybeDecodeMessage packageData
         let reason = fromMaybe D_Unsubscribed $ getField $ dropReason msg

         return Dropped
                { droppedReason = reason
                , droppedSub    = sub
                , droppedId     = packageCorrelation
                }
    | otherwise = Nothing

--------------------------------------------------------------------------------
nonEmptyText :: Text -> Maybe Text
nonEmptyText "" = Nothing
nonEmptyText t  = Just t

--------------------------------------------------------------------------------
onPersistActionConfirmed :: Package -> Manager -> Maybe PersistActionConfirmed
onPersistActionConfirmed Package{..} Manager{..} =
    case M.lookup packageCorrelation _pendingPersistActions of
        Just (PendingPersistAction _ grp stream typ cb) ->
            case (packageCmd, typ) of
                (0xC9, PersistCreate _) -> do
                    msg <- maybeDecodeMessage packageData
                    let res    = getField $ cpscResult msg
                        reason = nonEmptyText =<< getField (cpscReason msg)
                        ret =
                            case res of
                                CPS_Success ->
                                    Right ()
                                CPS_Fail ->
                                    Left $ peristentCreationFailure grp
                                                                    stream
                                                                    reason
                                CPS_AlreadyExists ->
                                    Left $ persistentCreationExists grp
                                                                    stream
                                CPS_AccessDenied ->
                                    Left $ persistentAccessDenied stream
                        pac = PersistActionConfirmed
                              { _pacId     = packageCorrelation
                              , _pacResult = ret
                              , _pacCB     = cb
                              }
                    return pac
                (0xCF, PersistUpdate _) -> do
                    msg <- maybeDecodeMessage packageData
                    let res    = getField $ upscResult msg
                        reason = nonEmptyText =<< getField (upscReason msg)
                        ret =
                            case res of
                                UPS_Success ->
                                    Right ()
                                UPS_Fail ->
                                    Left $ peristentCreationFailure grp
                                                                    stream
                                                                    reason
                                UPS_DoesNotExist ->
                                    Left $ persistentDoesNotExist grp
                                                                  stream
                                UPS_AccessDenied ->
                                    Left $ persistentAccessDenied stream
                        pac = PersistActionConfirmed
                              { _pacId     = packageCorrelation
                              , _pacResult = ret
                              , _pacCB     = cb
                              }
                    return pac
                (0xCB, PersistDelete) -> do
                    msg <- maybeDecodeMessage packageData
                    let res    = getField $ dpscResult msg
                        reason = nonEmptyText =<< getField (dpscReason msg)
                        ret =
                            case res of
                                DPS_Success ->
                                    Right ()
                                DPS_Fail ->
                                    Left $ peristentCreationFailure grp
                                                                    stream
                                                                    reason
                                DPS_DoesNotExist ->
                                    Left $ persistentDoesNotExist grp
                                                                  stream
                                DPS_AccessDenied ->
                                    Left $ persistentAccessDenied stream
                        pac = PersistActionConfirmed
                              { _pacId     = packageCorrelation
                              , _pacResult = ret
                              , _pacCB     = cb
                              }
                    return pac
                _ -> Nothing
        _ -> Nothing

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------
persistActionConfirmed :: PersistActionConfirmed -> Manager -> Manager
persistActionConfirmed pc s@Manager{..} =
    s { _pendingPersistActions = M.delete (_pacId pc) _pendingPersistActions }

--------------------------------------------------------------------------------
subscribeRequest :: Pending -> Manager -> Manager
subscribeRequest p@(Pending uuid _ _) s@Manager{..} =
    s { _pendings = M.insert uuid p _pendings }

--------------------------------------------------------------------------------
dropped :: Dropped -> Manager -> Manager
dropped d s@Manager{..} = s { _ongoings = M.delete (droppedId d) _ongoings }

--------------------------------------------------------------------------------
confirmed :: RegisterSub -> Manager -> Manager
confirmed (RegisterSub uuid typ sub) s@Manager{..} =
    s { _pendings = M.delete uuid _pendings
      , _ongoings = M.insert uuid (OnGoing typ sub) _ongoings
      }

--------------------------------------------------------------------------------
newPersistAction :: PendingPersistAction -> Manager -> Manager
newPersistAction ppa@PendingPersistAction{..} s@Manager{..} =
    s { _pendingPersistActions = M.insert _ppaId ppa _pendingPersistActions }

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
peristentCreationFailure :: Text
                         -> Text
                         -> Maybe Text
                         -> OperationException
peristentCreationFailure group stream m_reason = InvalidOperation msg
  where
    msg = "Subscription group " <> group <> " on stream " <> stream <>
          " failed" <> reasonTxt

    reasonTxt = foldMap (" reason: " <>) m_reason

--------------------------------------------------------------------------------
persistentCreationExists :: Text -> Text -> OperationException
persistentCreationExists group stream = InvalidOperation msg
  where
    msg = "Subscription group " <> group <> " on stream " <> stream <>
          " already exists."

--------------------------------------------------------------------------------
persistentDoesNotExist :: Text -> Text -> OperationException
persistentDoesNotExist group stream = InvalidOperation msg
  where
    msg = "Subscription group " <> group <> " on stream " <> stream <>
          " doesn't exist."

--------------------------------------------------------------------------------
persistentAccessDenied :: Text -> OperationException
persistentAccessDenied stream = AccessDenied msg
  where
    msg = "Write access denied for stream " <> stream
