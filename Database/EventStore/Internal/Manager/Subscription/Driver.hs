{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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
import ClassyPrelude hiding (group)
import Control.Monad.State
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
cmdSubCallback :: Cmd r -> Maybe (SubConnectEvent -> r)
cmdSubCallback (ConnectReg k _ _) = Just k
cmdSubCallback (ConnectPersist k _ _ _) = Just k
cmdSubCallback _ = Nothing

--------------------------------------------------------------------------------
-- | Driver internal state.
data Internal r =
    Internal
    { _model :: !Model
      -- ^ Subscription model.
    , _gen :: !Generator
      -- ^ 'UUID' generator.
    , _reg :: !(HashMap UUID (Cmd r))
      -- ^ Holds ongoing commands. When stored, it means an action hasn't been
      --   confirmed yet.
    }

--------------------------------------------------------------------------------
initInternal :: Generator -> Internal r
initInternal gen = Internal newModel gen mempty

--------------------------------------------------------------------------------
-- | Subscription driver state machine.
newtype Driver r = Driver (forall a. In r a -> a)

--------------------------------------------------------------------------------
newtype DriverM r m a = DriverM (ReaderT Settings (StateT (Internal r) m) a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Settings
             , MonadState (Internal r)
             )

--------------------------------------------------------------------------------
instance MonadTrans (DriverM r) where
    lift m = DriverM $ lift $ lift m

--------------------------------------------------------------------------------
noop :: DriverM r Maybe a
noop = lift Nothing

--------------------------------------------------------------------------------
modelSubRunning :: UUID -> DriverM r Maybe Running
modelSubRunning uuid = do
    model <- gets _model
    lift $ querySubscription uuid $ model

--------------------------------------------------------------------------------
modelSubConfirmed :: Monad m => UUID -> Meta -> DriverM r m ()
modelSubConfirmed uuid meta = do
    model <- gets _model
    let nxt = confirmedSubscription uuid meta model
    modify $ \s -> s { _model = nxt }

--------------------------------------------------------------------------------
modelActionConfirmed :: Monad m => UUID -> DriverM r m ()
modelActionConfirmed uuid =
    modify $ \s -> s { _model = confirmedAction uuid $ _model s }

--------------------------------------------------------------------------------
modelUnsubscribed :: UUID -> DriverM r Maybe ()
modelUnsubscribed uuid = do
    run <- modelSubRunning uuid
    model <- gets _model
    modify $ \s -> s { _model = unsubscribed run model }

--------------------------------------------------------------------------------
registerDelete :: Monad m => UUID -> DriverM r m ()
registerDelete uuid = do
    reg <- gets _reg
    let nxtR = deleteMap uuid reg
    modify $ \s -> s { _reg = nxtR }

--------------------------------------------------------------------------------
registerAdd :: Monad m => UUID -> Cmd r -> DriverM r m ()
registerAdd uuid cmd = do
    reg <- gets _reg
    modify $ \s -> s { _reg = insertMap uuid cmd reg }

--------------------------------------------------------------------------------
modelPersistentAction :: UUID -> DriverM r Maybe PendingAction
modelPersistentAction uuid = do
    model <- gets _model
    lift $ queryPersistentAction uuid model

--------------------------------------------------------------------------------
freshUUID :: Monad m => DriverM r m UUID
freshUUID = do
    (uuid, nxtG) <- gets (nextUUID . _gen)
    modify $ \s -> s { _gen = nxtG }
    return uuid

--------------------------------------------------------------------------------
modelConnectReg :: Monad m => Text -> Bool -> DriverM r m UUID
modelConnectReg stream tos = do
    uuid <- freshUUID
    model <- gets _model
    modify $ \s -> s { _model = connectReg stream tos uuid model }
    return uuid

--------------------------------------------------------------------------------
modelConnectPersist :: Monad m => Text -> Text -> Int32 -> DriverM r m UUID
modelConnectPersist group name batch = do
  uuid <- freshUUID
  model <- gets _model
  modify $ \s -> s { _model = connectPersist group name batch uuid model }
  return uuid

--------------------------------------------------------------------------------
modelPersistAction :: Monad m
                   => Text
                   -> Text
                   -> PersistAction
                   -> DriverM r m UUID
modelPersistAction group name action = do
    uuid <- freshUUID
    model <- gets _model
    modify $ \s -> s { _model = persistAction group name uuid action model }
    return uuid

--------------------------------------------------------------------------------
runDriverM :: Monad m
           => Settings
           -> Internal r
           -> DriverM r m a
           -> m (a, Driver r)
runDriverM setts st (DriverM m) = do
    (a, nxtSt) <- runStateT (runReaderT m setts) st
    return (a, Driver $ handleDriver setts nxtSt)

--------------------------------------------------------------------------------
runDriver :: Settings
          -> Internal r
          -> DriverM r Identity a
          -> (a, Driver r)
runDriver setts st action = runIdentity $ runDriverM setts st action

--------------------------------------------------------------------------------
-- Driver main state machine.
handleDriver :: Settings -> Internal r -> In r a -> a
handleDriver setts st (Pkg pkg) = do
    let corrId = packageCorrelation pkg
    cmd <- lookup corrId $ _reg st
    let action = handleMsg corrId cmd $ decodeServerMessage pkg
    runDriverM setts st action
handleDriver setts st (Cmd cmd) =
    let action = handleCmd cmd in
    runDriver setts st action
handleDriver _ st Abort = (fmap snd $ mapToList $ _reg st) >>= _F
  where
    _F (ConnectReg k _ _)           = [k $ Dropped SubAborted]
    _F (ConnectPersist k _ _ _)     = [k $ Dropped SubAborted]
    _F (ApplyPersistAction k _ _ _) = [k $ Left PersistActionAborted]
    _F _                            = []

--------------------------------------------------------------------------------
-- | Handles 'Package's coming from the server.
handleMsg :: UUID -> Cmd r -> ServerMessage -> DriverM r Maybe r
handleMsg corrId = go
  where
    go (ConnectReg k _ _) (EventAppearedMsg evt) = do
        _ <- modelSubRunning corrId
        return $ k $ EventAppeared evt
    go (ConnectPersist k _ _ _) (PersistentEventAppearedMsg evt) = do
        _ <- modelSubRunning corrId
        return $ k $ EventAppeared evt
    go (ConnectReg k _ _) (ConfirmationMsg lcp len) = do
        let meta = RegularMeta lcp len
        modelSubConfirmed corrId meta
        run <- modelSubRunning corrId
        return $ k $ SubConfirmed run
    go (ConnectPersist k _ _ _) (PersistentConfirmationMsg sid lcp len) = do
        let meta = PersistMeta sid lcp len
        modelSubConfirmed corrId meta
        run <- modelSubRunning corrId
        return $ k $ SubConfirmed run
    go cmd (PersistentCreatedMsg res) =
        confirmPAction cmd $ createRException res
    go cmd (PersistentUpdatedMsg res) =
        confirmPAction cmd $ updateRException res
    go cmd (PersistentDeletedMsg res) =
        confirmPAction cmd $ deleteRException res
    go cmd (DroppedMsg reason) = do
        modelUnsubscribed corrId
        registerDelete corrId
        let evt =
              case reason of
                  SubUnsubscribed -> Unsubscribed
                  _ -> Dropped reason
        k <- lift $ cmdSubCallback cmd
        return $ k evt
    go cmd (BadRequestMsg msg) =
        go cmd (DroppedMsg $ SubServerError msg)
    go cmd (NotAuthenticatedMsg msg) =
        go cmd (DroppedMsg $ SubNotAuthenticated msg)
    go cmd (NotHandledMsg reason info) =
        go cmd (DroppedMsg $ SubNotHandled reason info)
    go cmd (UnknownMsg pkgCmdM) = do
        k <- lift $ cmdSubCallback cmd
        let msgM = fmap (\c -> "unknown command: " <> tshow c) pkgCmdM
        return $ k $ Dropped $ SubServerError msgM
    go cmd _ = do
        k <- lift $ cmdSubCallback cmd
        let msg = "Logic error in Subscription Driver (the impossible happened)"
        return $ k $ Dropped $ SubClientError msg

    confirmPAction :: Cmd r
                   -> Maybe PersistActionException
                   -> DriverM r Maybe r
    confirmPAction (ApplyPersistAction k g n c) em = do
        _ <- modelPersistentAction corrId
        modelActionConfirmed corrId
        registerDelete corrId
        let evt = ConfirmedAction corrId g n c
        case em of
            Just e  -> return $ k $ Left e
            Nothing -> return $ k $ Right evt
    confirmPAction _ _ = noop

--------------------------------------------------------------------------------
-- | Handles commands coming from the user.
handleCmd :: Monad m => Cmd r -> DriverM r m Package
handleCmd cmd@(ConnectReg _ s tos) = do
    setts <- ask
    uuid <- modelConnectReg s tos
    registerAdd uuid cmd
    return $ createConnectRegularPackage setts uuid s tos
handleCmd cmd@(ConnectPersist _ gn n b) = do
    setts <- ask
    uuid <- modelConnectPersist gn n b
    registerAdd uuid cmd
    return $ createConnectPersistPackage setts uuid gn n b
handleCmd (Unsubscribe r) = do
    setts <- ask
    return $ createUnsubscribePackage setts $ runningUUID r
handleCmd cmd@(ApplyPersistAction _ gn n a) = do
    setts <- ask
    uuid <- modelPersistAction gn n a
    registerAdd uuid cmd
    return $ createPersistActionPackage setts uuid gn n a
handleCmd (PersistAck _ run evts) = do
    setts <- ask
    let RunningPersist _ _ _ _ sid _ _ = run
        uuid = runningUUID run
    return $ createAckPackage setts uuid sid evts
handleCmd (PersistNak _ run na r evts) = do
    setts <- ask
    let RunningPersist _ _ _ _ sid _ _ = run
        uuid = runningUUID run
    return $ createNakPackage setts uuid sid na r evts

--------------------------------------------------------------------------------
-- | Creates a new subscription 'Driver' state machine.
newDriver :: forall r. Settings -> Generator -> Driver r
newDriver setts gen = Driver $ handleDriver setts (initInternal gen)
