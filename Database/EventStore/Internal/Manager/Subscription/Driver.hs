{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
-- Subscription model driver. It drivers the model accordingly depending on the
-- 'Package' or commands submitted to it.
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Manager.Subscription.Driver
    ( SubDropReason(..)
    , SubConnectEvent(..)
    , PersistActionException(..)
    , ConfirmedAction(..)
    , NakAction(..)
    , Driver
    , newDriver
    , submitPackage
    , connectToStream
    , connectToPersist
    , createPersist
    , updatePersist
    , deletePersist
    , ackPersist
    , nakPersist
    , unsubscribe
    , abort
    ) where

--------------------------------------------------------------------------------
import Control.Exception
import Data.Int
import Data.Maybe
import Data.Typeable

--------------------------------------------------------------------------------
import           Data.ByteString
import qualified Data.HashMap.Strict as H
import           Data.Serialize
import           Data.ProtocolBuffers
import           Data.Text
import           Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Generator
import Database.EventStore.Internal.Manager.Subscription.Message
import Database.EventStore.Internal.Manager.Subscription.Model
import Database.EventStore.Internal.Manager.Subscription.Packages
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Set of events that can occurs during a subscription lifetime.
data SubConnectEvent
    = EventAppeared ResolvedEvent
      -- ^ A wild event appeared !
    | Dropped SubDropReason
      -- ^ The subscription connection dropped.
    | SubConfirmed Running
      -- ^ Subscription connection is confirmed. It means that subscription can
      --   receive events from the server.

--------------------------------------------------------------------------------
-- | Indicates why a subscription has been dropped.
data SubDropReason
    = SubUnsubscribed
      -- ^ Subscription connection has been closed by the user.
    | SubAccessDenied
      -- ^ The current user is not allowed to operate on the supplied stream.
    | SubNotFound
      -- ^ Given stream name doesn't exist.
    | SubPersistDeleted
      -- ^ Given stream is deleted.
    | SubAborted
      -- ^ Occurs when the user shutdown the connection from the server or if
      -- the connection to the server is no longer possible.
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | Enumerates all persistent action exceptions.
data PersistActionException
    = PersistActionFail
      -- ^ The action failed.
    | PersistActionAlreadyExist
      -- ^ Happens when creating a persistent subscription on a stream with a
      --   group name already taken.
    | PersistActionDoesNotExist
      -- ^ An operation tried to do something on a persistent subscription or a
      --   stream that don't exist.
    | PersistActionAccessDenied
      -- ^ The current user is not allowed to operate on the supplied stream or
      --   persistent subscription.
    | PersistActionAborted
      -- ^ That action has been aborted because the user shutdown the connection
      --   to the server or the connection to the server is no longer possible.
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception PersistActionException

--------------------------------------------------------------------------------
-- | Emitted when a persistent action has been carried out successfully.
data ConfirmedAction =
    ConfirmedAction
    { caId     :: !UUID
      -- ^ Action id.
    , caGroup  :: !Text
      -- ^ Subscription group name.
    , caStream :: !Text
      -- ^ Stream name.
    , caAction :: !PersistAction
      -- ^ Persistent action type.
    }

--------------------------------------------------------------------------------
-- | Submits a 'Package' to a subscription driver. If the 'Package' was
--   processed by the driver, it will return a final value and a new driver with
--   its internal state updated accordingly.
submitPackage :: Package -> Driver r -> Maybe (r, Driver r)
submitPackage pkg (Driver k) = k (Pkg pkg)

--------------------------------------------------------------------------------
-- | Starts a regular subscription connection. It returns the associated
--   'Package' and updates driver internal state.
connectToStream :: (SubConnectEvent -> r)
                -> Text -- ^ Stream name.
                -> Bool -- ^ Resolve Link TOS
                -> Driver r
                -> (Package, Driver r)
connectToStream c s t (Driver k) = k (Cmd $ ConnectReg c s t)

--------------------------------------------------------------------------------
-- | Starts a persistent subscription connection. It returns the associated
--   'Package' and updates driver internal state.
connectToPersist :: (SubConnectEvent -> r)
                 -> Text  -- ^ Group name.
                 -> Text  -- ^ Stream name.
                 -> Int32 -- ^ Buffer size.
                 -> Driver r
                 -> (Package, Driver r)
connectToPersist c g s b (Driver k) = k (Cmd $ ConnectPersist c g s b)

--------------------------------------------------------------------------------
-- | Creates a persistent subscription. It returns the associated 'Package' and
--   updates driver internal state.
createPersist :: (Either PersistActionException ConfirmedAction -> r)
              -> Text -- ^ Group name.
              -> Text -- ^ Stream name.
              -> PersistentSubscriptionSettings
              -> Driver r
              -> (Package, Driver r)
createPersist c g s ss (Driver k) =
    k (Cmd $ ApplyPersistAction c g s (PersistCreate ss))

--------------------------------------------------------------------------------
-- | Updates a persistent subscription. It returns the associated 'Package' and
--   updates driver internal state.
updatePersist :: (Either PersistActionException ConfirmedAction -> r)
              -> Text -- ^ Group name.
              -> Text -- ^ Stream name.
              -> PersistentSubscriptionSettings
              -> Driver r
              -> (Package, Driver r)
updatePersist c g s ss (Driver k) =
    k (Cmd $ ApplyPersistAction c g s (PersistUpdate ss))

--------------------------------------------------------------------------------
-- | Deletes a persistent subscription. It returns the associated 'Package' and
-- updates driver internal state.
deletePersist :: (Either PersistActionException ConfirmedAction -> r)
              -> Text -- ^ Group name.
              -> Text -- ^ Stream name.
              -> Driver r
              -> (Package, Driver r)
deletePersist c g s (Driver k) =
    k (Cmd $ ApplyPersistAction c g s PersistDelete)

--------------------------------------------------------------------------------
-- | Given a persistent subscription, acknowledges a set of events have been
--   successfully processed. It returns the associated 'Package' and updates
--   driver internal state.
ackPersist :: r
           -> Running
           -> Text   -- ^ Subscription id.
           -> [UUID] -- ^ Event ids.
           -> Driver r
           -> (Package, Driver r)
ackPersist r i sid evts (Driver k) = k (Cmd $ PersistAck r i sid evts)

--------------------------------------------------------------------------------
-- | Given a persistent subscription, indicates a set of event haven't been
--   processed correctly. It returns the associated 'Package' and updates driver
--   internal state.
nakPersist :: r
           -> Running
           -> Text       -- ^ Subscription id.
           -> NakAction
           -> Maybe Text -- ^ Reason.
           -> [UUID]     -- ^ Event ids.
           -> Driver r
           -> (Package, Driver r)
nakPersist r i sid na mt evts (Driver k) =
    k (Cmd $ PersistNak r i sid na mt evts)

--------------------------------------------------------------------------------
-- | Unsubscribe from a subscription.
unsubscribe :: Running -> Driver r -> (Package, Driver r)
unsubscribe r (Driver k) = k (Cmd $ Unsubscribe r)

--------------------------------------------------------------------------------
-- | Aborts every pending action.
abort :: Driver r -> [r]
abort (Driver k) = k Abort

--------------------------------------------------------------------------------
-- EventStore result mappers:
-- =========================
-- EventStore protocol has several values that means the exact same thing. Those
-- functions convert a specific EventStore to uniform result type common to all
-- persistent actions.
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
--------------------------------------------------------------------------------
-- | Type of inputs handled by the 'Subscription' driver.
data In r a where
    -- A command consists of receiving some parameters, updating the
    -- 'Subscription' model accordingly and thus modifying driver internal
    -- state.
    Cmd :: Cmd r -> In r (Package, Driver r)
    -- A 'Package' has been submitted to the 'Subscription' driver. If the
    -- driver recognize that 'Package', it returns a final value and update
    -- the driver internal state.
    Pkg :: Package -> In r (Maybe (r, Driver r))
    -- Aborts every pending action.
    Abort :: In r [r]

--------------------------------------------------------------------------------
-- | Set of commands handled by the driver.
data Cmd r
    = ConnectReg (SubConnectEvent -> r) Text Bool
      -- ^ Creates a regular 'Subscription' connection. When a 'SubConnectEvent'
      --   has arrived, the driver will use the provided callback and emit a
      --   final value. It holds a stream name and `Resolve Link TOS` setting.
    | ConnectPersist (SubConnectEvent -> r)
                     Text
                     Text
                     Int32
      -- ^ Creates a persistent 'Subscription' connection. When a
      --   'SubConnectEvent' has arrived, the driver will use the provided
      --   callback  and emit a final value. It holds a group name, stream
      --   name and a buffer size.

    | Unsubscribe Running
      -- ^ Unsubscribe from a subscription.

    | ApplyPersistAction (Either PersistActionException ConfirmedAction -> r)
                         Text
                         Text
                         PersistAction
      -- ^ Creates a persistent action. Depending of the failure or the success
      --   of that action, the driver will use the provided callback to emit a
      --   final value. It hols a group name, a stream name and a persistent
      --   action.

    | PersistAck r Running Text [UUID]
      -- ^ Acks a set of Event 'UUID' to notify those events have been correctly
      --   handled. It holds a 'Running' subscription, a subscription id and a
      --   set of `UUID` representing event id. When the ack would be confirmed
      --   the driver will return the supplied final value.
    | PersistNak r
                 Running
                 Text
                 NakAction
                 (Maybe Text)
                 [UUID]
       -- ^ Naks a set of Event 'UUID' to notify those events haven't been
       --   handled correctly. it holds a 'Running' subscription, a
       --   subscription id, a 'NakAction', an optional reason and a set of
       --   event ids. When the nak would be confirmed, the driver will return
       --   the provided final value.

--------------------------------------------------------------------------------
-- | Driver internal state.
data State r =
    State
    { _model :: !Model
      -- ^ Subscription model.
    , _gen :: !Generator
      -- ^ 'UUID' generator.
    , _reg :: !(H.HashMap UUID (Cmd r))
      -- ^ Holds ongoing commands. When stored, it means an action hasn't been
      --   confirmed yet.
    }

--------------------------------------------------------------------------------
initState :: Generator -> State r
initState gen = State newModel gen H.empty

--------------------------------------------------------------------------------
-- | Subscription driver state machine.
newtype Driver r = Driver (forall a. In r a -> a)

--------------------------------------------------------------------------------
-- | Creates a new subscription 'Driver' state machine.
newDriver :: forall r. Settings -> Generator -> Driver r
newDriver setts gen = Driver $ go (initState gen)
  where
    go :: forall a. State r -> In r a -> a
    go st@State{..} (Pkg Package{..}) = do
        elm <- H.lookup packageCorrelation _reg
        case packageCmd of
            0xC2 -> do
                _   <- querySubscription packageCorrelation _model
                msg <- maybeDecodeMessage packageData
                let e   = getField $ streamResolvedEvent msg
                    evt = newResolvedEventFromBuf e
                    app = EventAppeared evt
                    ConnectReg k _ _ = elm
                return (k app, Driver $ go st)

            0xC7 -> do
                _   <- querySubscription packageCorrelation _model
                msg <- maybeDecodeMessage packageData
                let e   = getField $ psseaEvt msg
                    evt = newResolvedEvent e
                    app = EventAppeared evt
                    ConnectPersist k _ _ _ = elm
                return (k app, Driver $ go st)

            0xC1 -> do
                msg <- maybeDecodeMessage packageData
                let lcp  = getField $ subscribeLastCommitPos msg
                    len  = getField $ subscribeLastEventNumber msg
                    meta = RegularMeta lcp len
                    ConnectReg k _ _ = elm
                    nxt_m = confirmedSubscription packageCorrelation meta _model
                run   <- querySubscription packageCorrelation nxt_m
                let nxt_st = st { _model = nxt_m }
                    evt    = SubConfirmed run
                return (k evt, Driver $ go nxt_st)

            0xC6 -> do
                msg <- maybeDecodeMessage packageData
                let lcp  = getField $ pscLastCommitPos msg
                    sid  = getField $ pscId msg
                    len  = getField $ pscLastEvtNumber msg
                    meta = PersistMeta sid lcp len
                    ConnectPersist k _ _ _ = elm
                    nxt_m = confirmedSubscription packageCorrelation meta _model
                run   <- querySubscription packageCorrelation nxt_m
                let nxt_st = st { _model = nxt_m }
                    evt    = SubConfirmed run
                return (k evt, Driver $ go nxt_st)

            0xC9 -> confirmPAction elm (getField . cpscResult) createRException
            0xCF -> confirmPAction elm (getField . upscResult) updateRException
            0xCB -> confirmPAction elm (getField . dpscResult) deleteRException

            0xC4 -> do
                run <- querySubscription packageCorrelation _model
                msg <- maybeDecodeMessage packageData
                let reason  = fromMaybe D_Unsubscribed $ getField
                                                       $ dropReason msg
                    nxt_m   = unsubscribed run _model
                    dreason = toSubDropReason reason
                    evt     = Dropped dreason
                    nxt_reg = H.delete packageCorrelation _reg
                    nxt_st  = st { _model = nxt_m
                                 , _reg   = nxt_reg }
                case elm of
                  ConnectReg k _ _       -> return (k evt, Driver $ go nxt_st)
                  ConnectPersist k _ _ _ -> return (k evt, Driver $ go nxt_st)
                  _                      -> Nothing

            _ -> Nothing
      where
        confirmPAction :: Decode m
                       => Cmd r
                       -> (m -> e)
                       -> (e -> Maybe PersistActionException)
                       -> Maybe (r, Driver r)
        confirmPAction (ApplyPersistAction k g n c) fd em = do
            msg <- maybeDecodeMessage packageData
            _   <- queryPersistentAction packageCorrelation _model
            let nxt_m  = confirmedAction packageCorrelation _model
                nxt_rg = H.delete packageCorrelation _reg
                nxt_st = st { _model = nxt_m
                            , _reg   = nxt_rg
                            }
                evt    = ConfirmedAction packageCorrelation g n c
            case em $ fd msg of
                Just e  -> return (k $ Left e, Driver $ go nxt_st)
                Nothing -> return (k $ Right evt, Driver $ go nxt_st)
        confirmPAction _ _ _ = Nothing

    go st@State{..} (Cmd cmd) =
        case cmd of
            ConnectReg _ s tos ->
                let (u, nxt_g) = nextUUID _gen
                    pkg        = createConnectRegularPackage setts u s tos
                    nxt_m      = connectReg s tos u _model
                    nxt_st     = st { _model = nxt_m
                                    , _gen   = nxt_g
                                    , _reg   = H.insert u cmd _reg } in
                (pkg, Driver $ go nxt_st)
            ConnectPersist _ gn n b ->
                let (u, nxt_g) = nextUUID _gen
                    pkg        = createConnectPersistPackage setts u gn n b
                    nxt_m      = connectPersist gn n b u _model
                    nxt_st     = st { _model = nxt_m
                                    , _gen   = nxt_g
                                    , _reg   = H.insert u cmd _reg } in
                (pkg, Driver $ go nxt_st)
            Unsubscribe r ->
                let pkg    = createUnsubscribePackage setts $ runningUUID r
                    nxt_m  = unsubscribed r _model
                    nxt_st = st { _model = nxt_m } in
                (pkg, Driver $ go nxt_st)
            ApplyPersistAction _ gn n a ->
                let (u, nxt_g) = nextUUID _gen
                    pkg        = createPersistActionPackage setts u gn n a
                    nxt_m      = persistAction gn n u a _model
                    nxt_st     = st { _model = nxt_m
                                    , _gen   = nxt_g
                                    , _reg   = H.insert u cmd _reg } in
                (pkg, Driver $ go nxt_st)
            PersistAck _ run sid evts ->
                let u      = runningUUID run
                    pkg    = createAckPackage setts u sid evts
                    nxt_st = st { _reg = H.insert u cmd _reg } in
                (pkg, Driver $ go nxt_st)
            PersistNak _ run sid na r evts ->
                let u      = runningUUID run
                    pkg    = createNakPackage setts u sid na r evts
                    nxt_st = st { _reg = H.insert u cmd _reg } in
                (pkg, Driver $ go nxt_st)
    go st Abort = (H.elems $ _reg st) >>= _F
      where
        _F (ConnectReg k _ _)           = [k $ Dropped SubAborted]
        _F (ConnectPersist k _ _ _)     = [k $ Dropped SubAborted]
        _F (ApplyPersistAction k _ _ _) = [k $ Left PersistActionAborted]
        _F _                            = []

--------------------------------------------------------------------------------
maybeDecodeMessage :: Decode a => ByteString -> Maybe a
maybeDecodeMessage bytes =
    case runGet decodeMessage bytes of
        Right a -> Just a
        _       -> Nothing
