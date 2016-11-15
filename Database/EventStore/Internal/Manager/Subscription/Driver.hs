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
import Data.Int
import Data.Maybe

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Generator
import Database.EventStore.Internal.Manager.Subscription.Command
import Database.EventStore.Internal.Manager.Subscription.Message
import Database.EventStore.Internal.Manager.Subscription.Model
import Database.EventStore.Internal.Manager.Subscription.Packages
import Database.EventStore.Internal.Manager.Subscription.Types
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
    | Unsubscribed

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
           -> [UUID] -- ^ Event ids.
           -> Driver r
           -> (Package, Driver r)
ackPersist r i evts (Driver k) = k (Cmd $ PersistAck r i evts)

--------------------------------------------------------------------------------
-- | Given a persistent subscription, indicates a set of event haven't been
--   processed correctly. It returns the associated 'Package' and updates driver
--   internal state.
nakPersist :: r
           -> Running
           -> NakAction
           -> Maybe Text -- ^ Reason.
           -> [UUID]     -- ^ Event ids.
           -> Driver r
           -> (Package, Driver r)
nakPersist r i na mt evts (Driver k) =
    k (Cmd $ PersistNak r i na mt evts)

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

    | PersistAck r Running [UUID]
      -- ^ Acks a set of Event 'UUID' to notify those events have been correctly
      --   handled. It holds a 'Running' subscription and a set of `UUID`
      --   representing event id. When the ack would be confirmed the driver
      --   will return the supplied final value.
    | PersistNak r
                 Running
                 NakAction
                 (Maybe Text)
                 [UUID]
      -- ^ Naks a set of Event 'UUID' to notify those events haven't been
      --   handled correctly. it holds a 'Running' subscription, a 'NakAction',
      --   an optional reason and a set of event ids. When the nak would be
      --   confirmed, the driver will return the provided final value.

--------------------------------------------------------------------------------
-- | Driver internal state.
data State r =
    State
    { _model :: !Model
      -- ^ Subscription model.
    , _gen :: !Generator
      -- ^ 'UUID' generator.
    , _reg :: !(HashMap UUID (Cmd r))
      -- ^ Holds ongoing commands. When stored, it means an action hasn't been
      --   confirmed yet.
    }

--------------------------------------------------------------------------------
initState :: Generator -> State r
initState gen = State newModel gen mempty

--------------------------------------------------------------------------------
-- | Subscription driver state machine.
newtype Driver r = Driver (forall a. In r a -> a)

--------------------------------------------------------------------------------
-- | Creates a new subscription 'Driver' state machine.
newDriver :: forall r. Settings -> Generator -> Driver r
newDriver setts gen = Driver $ go (initState gen)
  where
    go :: forall a. State r -> In r a -> a
    go st@State{..} (Pkg pkg) = do
        elm <- lookup (packageCorrelation pkg) _reg
        case decodeServerMessage pkg of
            EventAppearedMsg evt -> do
                _   <- querySubscription (packageCorrelation pkg) _model
                let ConnectReg k _ _ = elm
                return (k $ EventAppeared evt, Driver $ go st)
            PersistentEventAppearedMsg evt -> do
                _   <- querySubscription (packageCorrelation pkg) _model
                let ConnectPersist k _ _ _ = elm
                return (k $ EventAppeared evt, Driver $ go st)
            ConfirmationMsg lcp len -> do
                let meta = RegularMeta lcp len
                    ConnectReg k _ _ = elm
                    nxt_m = confirmedSubscription
                                (packageCorrelation pkg) meta _model
                run <- querySubscription (packageCorrelation pkg) nxt_m
                let nxt_st = st { _model = nxt_m }
                return (k $ SubConfirmed run, Driver $ go nxt_st)
            PersistentConfirmationMsg sid lcp len -> do
                let meta = PersistMeta sid lcp len
                    ConnectPersist k _ _ _ = elm
                    nxt_m = confirmedSubscription
                                (packageCorrelation pkg) meta _model
                run <- querySubscription (packageCorrelation pkg) nxt_m
                let nxt_st = st { _model = nxt_m }
                return (k $ SubConfirmed run, Driver $ go nxt_st)
            PersistentCreatedMsg res ->
                confirmPAction elm $ createRException res
            PersistentUpdatedMsg res ->
                confirmPAction elm $ updateRException res
            PersistentDeletedMsg res ->
                confirmPAction elm $ deleteRException res
            DroppedMsg reason -> do
                run <- querySubscription (packageCorrelation pkg) _model
                let nxt_m   = unsubscribed run _model
                    evt =
                        case reason of
                            SubUnsubscribed -> Unsubscribed
                            _ -> Dropped reason
                    nxt_reg = deleteMap (packageCorrelation pkg) _reg
                    nxt_st  = st { _model = nxt_m
                                 , _reg   = nxt_reg }
                case elm of
                  ConnectReg k _ _       -> return (k evt, Driver $ go nxt_st)
                  ConnectPersist k _ _ _ -> return (k evt, Driver $ go nxt_st)
                  _                      -> Nothing
            UnhandledMsg -> Nothing
      where
        confirmPAction :: Cmd r
                       -> Maybe PersistActionException
                       -> Maybe (r, Driver r)
        confirmPAction (ApplyPersistAction k g n c) em = do
            _ <- queryPersistentAction (packageCorrelation pkg) _model
            let nxt_m  = confirmedAction (packageCorrelation pkg) _model
                nxt_rg = deleteMap (packageCorrelation pkg) _reg
                nxt_st = st { _model = nxt_m
                            , _reg   = nxt_rg
                            }
                evt    = ConfirmedAction (packageCorrelation pkg) g n c
            case em of
                Just e  -> return (k $ Left e, Driver $ go nxt_st)
                Nothing -> return (k $ Right evt, Driver $ go nxt_st)
        confirmPAction _ _ = Nothing

    go st@State{..} (Cmd cmd) =
        case cmd of
            ConnectReg _ s tos ->
                let (u, nxt_g) = nextUUID _gen
                    pkg        = createConnectRegularPackage setts u s tos
                    nxt_m      = connectReg s tos u _model
                    nxt_st     = st { _model = nxt_m
                                    , _gen   = nxt_g
                                    , _reg   = insertMap u cmd _reg } in
                (pkg, Driver $ go nxt_st)
            ConnectPersist _ gn n b ->
                let (u, nxt_g) = nextUUID _gen
                    pkg        = createConnectPersistPackage setts u gn n b
                    nxt_m      = connectPersist gn n b u _model
                    nxt_st     = st { _model = nxt_m
                                    , _gen   = nxt_g
                                    , _reg   = insertMap u cmd _reg } in
                (pkg, Driver $ go nxt_st)
            Unsubscribe r ->
                let pkg    = createUnsubscribePackage setts $ runningUUID r in
                (pkg, Driver $ go st)
            ApplyPersistAction _ gn n a ->
                let (u, nxt_g) = nextUUID _gen
                    pkg        = createPersistActionPackage setts u gn n a
                    nxt_m      = persistAction gn n u a _model
                    nxt_st     = st { _model = nxt_m
                                    , _gen   = nxt_g
                                    , _reg   = insertMap u cmd _reg } in
                (pkg, Driver $ go nxt_st)
            PersistAck _ run evts ->
                let RunningPersist _ _ _ _ sid _ _ = run
                    u   = runningUUID run
                    pkg = createAckPackage setts u sid evts in
                (pkg, Driver $ go st)
            PersistNak _ run na r evts ->
                let RunningPersist _ _ _ _ sid _ _ = run
                    u   = runningUUID run
                    pkg = createNakPackage setts u sid na r evts  in
                (pkg, Driver $ go st)
    go st Abort = (fmap snd $ mapToList $ _reg st) >>= _F
      where
        _F (ConnectReg k _ _)           = [k $ Dropped SubAborted]
        _F (ConnectPersist k _ _ _)     = [k $ Dropped SubAborted]
        _F (ApplyPersistAction k _ _ _) = [k $ Left PersistActionAborted]
        _F _                            = []
