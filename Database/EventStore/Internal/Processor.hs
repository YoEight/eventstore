{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Processor
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
-- Top level operation and subscription logic of EventStore driver.
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Processor
    ( Processor
    , newProcessor
    , connectRegularStream
    , connectPersistent
    , createPersistent
    , updatePersistent
    , deletePersistent
    , newOperation
    ) where

--------------------------------------------------------------------------------
import Data.Int

--------------------------------------------------------------------------------
import Data.Text

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Generator
import Database.EventStore.Internal.Manager.Subscription
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Manager.Operation.Model
import Database.EventStore.Internal.Step
import Database.EventStore.Internal.Types

import qualified Database.EventStore.Internal.Manager.Operation.Model     as Op
import qualified Database.EventStore.Internal.Manager.Subscription.Driver as Sub

--------------------------------------------------------------------------------
-- | Type of inputs handled by the 'Processor' driver.
data In r
    = Cmd (Cmd r)
      -- ^ A command can be an 'Operation' or a 'Subscription' actions.
    | Pkg Package
      -- ^ A 'Package' has been sent by the server and needs to be processed by
      --   the 'Processor'.

--------------------------------------------------------------------------------
-- | Type of commmand a 'Processor' can handle.
data Cmd r
    = SubscriptionCmd (SubscriptionCmd r)
      -- ^ Subcription related commands.
    | forall a. NewOp (Operation 'Init a) (Either OperationError a -> r)
      -- ^ Register a new 'Operation'.

--------------------------------------------------------------------------------
-- | Supported subscription command.
data SubscriptionCmd r
    = ConnectStream (SubConnectEvent -> r) Text Bool
      -- ^ Creates a regular subscription connection.
    | ConnectPersist (SubConnectEvent -> r) Text Text Int32
      -- ^ Creates a persistent subscription connection.

    | CreatePersist (Either PersistActionException ConfirmedAction -> r)
                    Text
                    Text
                    PersistentSubscriptionSettings
      -- ^ Creates a persistent subscription.

    | UpdatePersist (Either PersistActionException ConfirmedAction -> r)
                    Text
                    Text
                    PersistentSubscriptionSettings
      -- ^ Updates a persistent subscription.

    | DeletePersist (Either PersistActionException ConfirmedAction -> r)
                    Text
                    Text
      -- ^ Deletes a persistent subscription.

--------------------------------------------------------------------------------
-- | Creates a regular subscription connection.
connectRegularStream :: (SubConnectEvent -> r)
                     -> Text -- ^ Stream name.
                     -> Bool -- ^ Resolve Link TOS.
                     -> Processor r
                     -> (Package, Processor r)
connectRegularStream c s tos (Processor k) =
    let Send pkg nxt =
            k $ Cmd $ SubscriptionCmd $ ConnectStream c s tos in
    (pkg, nxt)

--------------------------------------------------------------------------------
-- | Creates a persistent subscription connection.
connectPersistent :: (SubConnectEvent -> r)
                  -> Text  -- ^ Group name.
                  -> Text  -- ^ Stream name.
                  -> Int32 -- ^ Buffer size.
                  -> Processor r
                  -> (Package, Processor r)
connectPersistent c g s siz (Processor k) =
    let Send pkg nxt =
            k $ Cmd $ SubscriptionCmd $ ConnectPersist c g s siz in
    (pkg, nxt)

--------------------------------------------------------------------------------
-- | Creates a persistent subscription.
createPersistent :: (Either PersistActionException ConfirmedAction -> r)
                 -> Text -- ^ Group name.
                 -> Text -- ^ Stream name.
                 -> PersistentSubscriptionSettings
                 -> Processor r
                 -> (Package, Processor r)
createPersistent c g s sett (Processor k) =
    let Send pkg nxt =
            k $ Cmd $ SubscriptionCmd $ CreatePersist c g s sett in
    (pkg, nxt)

--------------------------------------------------------------------------------
-- | Updates a persistent subscription.
updatePersistent :: (Either PersistActionException ConfirmedAction -> r)
                 -> Text -- ^ Group name.
                 -> Text -- ^ Stream name.
                 -> PersistentSubscriptionSettings
                 -> Processor r
                 -> (Package, Processor r)
updatePersistent c g s sett (Processor k) =
    let Send pkg nxt =
            k $ Cmd $ SubscriptionCmd $ UpdatePersist c g s sett in
    (pkg, nxt)

--------------------------------------------------------------------------------
-- | Deletes a persistent subscription.
deletePersistent :: (Either PersistActionException ConfirmedAction -> r)
                 -> Text -- ^ Group name.
                 -> Text -- ^ Stream name.
                 -> Processor r
                 -> (Package, Processor r)
deletePersistent c g s (Processor k) =
    let Send pkg nxt =
            k $ Cmd $ SubscriptionCmd $ DeletePersist c g s in
    (pkg, nxt)

--------------------------------------------------------------------------------
-- | Registers a new 'Operation'.
newOperation :: (Either OperationError a -> r)
             -> Operation 'Init a
             -> Processor r
             -> (Package, Processor r)
newOperation c op (Processor k) =
    let Send pkg nxt = k $ Cmd $ NewOp op c in (pkg, nxt)

--------------------------------------------------------------------------------
-- | 'Processor' internal state.
data State r =
    State
    { _subDriver :: Driver r
      -- ^ Subscription driver.
    , _opModel :: Model r
      -- ^ Operation model.
    }

--------------------------------------------------------------------------------
initState :: Settings -> Generator -> State r
initState setts g = State (newDriver setts g1) (newModel g2)
  where
    (g1, g2) = splitGenerator g

--------------------------------------------------------------------------------
-- | Processor state-machine.
newtype Processor r = Processor (In r -> Step Processor r)

--------------------------------------------------------------------------------
-- | Creates a new 'Processor' state-machine.
newProcessor :: Settings -> Generator -> Processor r
newProcessor setts gen = Processor $ go $ initState setts gen
  where
    go st (Cmd tpe) =
        case tpe of
            NewOp op cb ->
                let (pkg, nxt_m) = pushOperation cb op $ _opModel st
                    nxt_st       = st { _opModel = nxt_m } in
                Send pkg (Processor $ go nxt_st)
            SubscriptionCmd cmd -> subCmd st cmd
    go st (Pkg pkg) =
        case Op.submitPackage pkg $ _opModel st of
            Just (Done r nxt_m) ->
                let nxt_st = st { _opModel = nxt_m } in
                Done r (Processor $ go nxt_st)
            Just (Send npkg nxt_m) ->
                let nxt_st = st { _opModel = nxt_m } in
                Send npkg (Processor $ go nxt_st)
            Just (Cont nxt_m) ->
                let nxt_st = st { _opModel = nxt_m } in
                Cont $ Processor $ go nxt_st
            Nothing ->
                case Sub.submitPackage pkg $ _subDriver st of
                    Nothing           -> Cont $ Processor $ go st
                    Just (r, nxt_drv) ->
                        let nxt_st = st { _subDriver = nxt_drv } in
                        Done r (Processor $ go nxt_st)

    subCmd st@State{..} cmd =
        case cmd of
            ConnectStream k s tos ->
                let (pkg, nxt_drv) = connectToStream k s tos _subDriver
                    nxt_st         = st { _subDriver = nxt_drv }
                    nxt            = Processor $ go nxt_st in
                Send pkg nxt
            ConnectPersist k g s b ->
                let (pkg, nxt_drv) = connectToPersist k g s b _subDriver
                    nxt_st         = st { _subDriver = nxt_drv }
                    nxt            = Processor $ go nxt_st in
                Send pkg nxt
            CreatePersist k g s ss ->
                let (pkg, nxt_drv) = createPersist k g s ss _subDriver
                    nxt_st         = st { _subDriver = nxt_drv }
                    nxt            = Processor $ go nxt_st in
                Send pkg nxt
            UpdatePersist k g s ss ->
                let (pkg, nxt_drv) = updatePersist k g s ss _subDriver
                    nxt_st         = st { _subDriver = nxt_drv }
                    nxt            = Processor $ go nxt_st in
                Send pkg nxt
            DeletePersist k g s ->
                let (pkg, nxt_drv) = deletePersist k g s _subDriver
                    nxt_st         = st { _subDriver = nxt_drv }
                    nxt            = Processor $ go nxt_st in
                Send pkg nxt
