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
    , Transition(..)
    , newProcessor
    , connectRegularStream
    , connectPersistent
    , createPersistent
    , updatePersistent
    , deletePersistent
    , ackPersist
    , nakPersist
    , newOperation
    , submitPackage
    , unsubscribe
    , abort
    ) where

--------------------------------------------------------------------------------
import Data.Int

--------------------------------------------------------------------------------
import Data.Text
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Generator
import Database.EventStore.Internal.Operation hiding (SM(..))
import Database.EventStore.Internal.Packages
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
import qualified Database.EventStore.Internal.Manager.Operation.Model as Op
import qualified Database.EventStore.Internal.Manager.Subscription    as Sub

--------------------------------------------------------------------------------
-- | Type of inputs handled by the 'Processor' driver.
data In r
    = Cmd (Cmd r)
      -- ^ A command can be an 'Operation' or a 'Subscription' actions.
    | Pkg Package
      -- ^ Handle a 'Package' coming from the server.

--------------------------------------------------------------------------------
-- | Type of commmand a 'Processor' can handle.
data Cmd r
    = SubscriptionCmd (SubscriptionCmd r)
      -- ^ Subcription related commands.
    | forall a. NewOp (Operation a) (Either OperationError a -> r)
      -- ^ Register a new 'Operation'.
    | Abort
      -- ^ Aborts every pending operation.

--------------------------------------------------------------------------------
-- | Supported subscription command.
data SubscriptionCmd r
    = ConnectStream (Sub.SubConnectEvent -> r) Text Bool
      -- ^ Creates a regular subscription connection.
    | ConnectPersist (Sub.SubConnectEvent -> r) Text Text Int32
      -- ^ Creates a persistent subscription connection.

    | CreatePersist (Either Sub.PersistActionException Sub.ConfirmedAction -> r)
                    Text
                    Text
                    PersistentSubscriptionSettings
      -- ^ Creates a persistent subscription.

    | Unsubscribe Sub.Running
      -- ^ Unsubscribes a subscription.

    | UpdatePersist (Either Sub.PersistActionException Sub.ConfirmedAction -> r)
                    Text
                    Text
                    PersistentSubscriptionSettings
      -- ^ Updates a persistent subscription.

    | DeletePersist (Either Sub.PersistActionException Sub.ConfirmedAction -> r)
                    Text
                    Text
      -- ^ Deletes a persistent subscription.

    | AckPersist r Sub.Running Text [UUID]
      -- ^ Acknowledges a set of events has been successfully handled.

    | NakPersist r Sub.Running Text Sub.NakAction (Maybe Text) [UUID]
      -- ^ Acknowledges a set of events hasn't been handled successfully.

--------------------------------------------------------------------------------
-- | Creates a regular subscription connection.
connectRegularStream :: (Sub.SubConnectEvent -> r)
                     -> Text -- ^ Stream name.
                     -> Bool -- ^ Resolve Link TOS.
                     -> Processor r
                     -> Transition r
connectRegularStream c s tos (Processor k) =
    k $ Cmd $ SubscriptionCmd $ ConnectStream c s tos

--------------------------------------------------------------------------------
-- | Creates a persistent subscription connection.
connectPersistent :: (Sub.SubConnectEvent -> r)
                  -> Text  -- ^ Group name.
                  -> Text  -- ^ Stream name.
                  -> Int32 -- ^ Buffer size.
                  -> Processor r
                  -> Transition r
connectPersistent c g s siz (Processor k) =
    k $ Cmd $ SubscriptionCmd $ ConnectPersist c g s siz

--------------------------------------------------------------------------------
-- | Creates a persistent subscription.
createPersistent :: (Either Sub.PersistActionException Sub.ConfirmedAction -> r)
                 -> Text -- ^ Group name.
                 -> Text -- ^ Stream name.
                 -> PersistentSubscriptionSettings
                 -> Processor r
                 -> Transition r
createPersistent c g s sett (Processor k) =
    k $ Cmd $ SubscriptionCmd $ CreatePersist c g s sett

--------------------------------------------------------------------------------
-- | Updates a persistent subscription.
updatePersistent :: (Either Sub.PersistActionException Sub.ConfirmedAction -> r)
                 -> Text -- ^ Group name.
                 -> Text -- ^ Stream name.
                 -> PersistentSubscriptionSettings
                 -> Processor r
                 -> Transition r
updatePersistent c g s sett (Processor k) =
    k $ Cmd $ SubscriptionCmd $ UpdatePersist c g s sett

--------------------------------------------------------------------------------
-- | Deletes a persistent subscription.
deletePersistent :: (Either Sub.PersistActionException Sub.ConfirmedAction -> r)
                 -> Text -- ^ Group name.
                 -> Text -- ^ Stream name.
                 -> Processor r
                 -> Transition r
deletePersistent c g s (Processor k) =
    k $ Cmd $ SubscriptionCmd $ DeletePersist c g s

--------------------------------------------------------------------------------
-- | Acknowledges a set of events has been successfully handled.
ackPersist :: r -> Sub.Running -> Text -> [UUID] -> Processor r -> Transition r
ackPersist r run gid evts (Processor k) =
    k $ Cmd $ SubscriptionCmd $ AckPersist r run gid evts

--------------------------------------------------------------------------------
-- | Acknowledges a set of events hasn't been handled successfully.
nakPersist :: r
           -> Sub.Running
           -> Text
           -> Sub.NakAction
           -> Maybe Text
           -> [UUID]
           -> Processor r
           -> Transition r
nakPersist r run gid act res evts (Processor k) =
    k $ Cmd $ SubscriptionCmd $ NakPersist r run gid act res evts

--------------------------------------------------------------------------------
-- | Registers a new 'Operation'.
newOperation :: (Either OperationError a -> r)
             -> Operation a
             -> Processor r
             -> Transition r
newOperation c op (Processor k) = k $ Cmd $ NewOp op c

--------------------------------------------------------------------------------
-- | Submits a 'Package'.
submitPackage :: Package -> Processor r -> Transition r
submitPackage pkg (Processor k) = k $ Pkg pkg

--------------------------------------------------------------------------------
-- | Unsubscribes a subscription.
unsubscribe :: Sub.Running -> Processor r -> Transition r
unsubscribe r (Processor k) = k $ Cmd $ SubscriptionCmd $ Unsubscribe r

--------------------------------------------------------------------------------
-- | Aborts every pending operation.
abort :: Processor r -> Transition r
abort (Processor k) = k $ Cmd Abort

--------------------------------------------------------------------------------
-- | 'Processor' internal state.
data State r =
    State
    { _subDriver :: Sub.Driver r
      -- ^ Subscription driver.
    , _opModel :: Op.Model r
      -- ^ Operation model.
    }

--------------------------------------------------------------------------------
initState :: Settings -> Generator -> State r
initState setts g = State (Sub.newDriver setts g1) (Op.newModel setts g2)
  where
    (g1, g2) = splitGenerator g

--------------------------------------------------------------------------------
data Transition r
    = Produce r (Transition r)
    | Transmit Package (Transition r)
    | Await (Processor r)

--------------------------------------------------------------------------------
-- | Processor state-machine.
newtype Processor r = Processor (In r -> Transition r)

--------------------------------------------------------------------------------
loopOpTransition :: State r -> Op.Transition r -> Transition r
loopOpTransition st (Op.Produce r nxt) =
    Produce r (loopOpTransition st nxt)
loopOpTransition st (Op.Transmit pkg nxt) =
    Transmit pkg (loopOpTransition st nxt)
loopOpTransition st (Op.Await m) =
    let nxt_st = st { _opModel = m } in Await $ Processor $ handle nxt_st

--------------------------------------------------------------------------------
abortTransition :: State r -> Op.Transition r -> [r] -> Transition r
abortTransition st init_op init_rs = abortOp init_op
  where
    abortOp (Op.Produce r nxt)  = Produce r (abortOp nxt)
    abortOp (Op.Transmit _ nxt) = abortOp nxt
    abortOp _                   = abortSub init_rs

    abortSub []     = Await $ Processor $ handle st
    abortSub (r:rs) = Produce r (abortSub rs)

--------------------------------------------------------------------------------
handle :: State r -> In r -> Transition r
handle = go
  where
    go st (Cmd tpe) =
        case tpe of
            NewOp op cb ->
                let sm = Op.pushOperation cb op $ _opModel st in
                loopOpTransition st sm
            SubscriptionCmd cmd -> subCmd st cmd
            Abort ->
                let sm = Op.abort $ _opModel st
                    rs = Sub.abort $ _subDriver st in
                abortTransition st sm rs

    go st (Pkg pkg)
        | packageCmd pkg == 0x01 =
          let r_pkg = heartbeatResponsePackage $ packageCorrelation pkg in
          Transmit r_pkg $ Await $ Processor $ go st
        | otherwise =
          let sm_m = Op.submitPackage pkg $ _opModel st in
          case fmap (loopOpTransition st) sm_m of
              Just nxt -> nxt
              Nothing ->
                  case Sub.submitPackage pkg $ _subDriver st of
                      Nothing           -> Await $ Processor $ go st
                      Just (r, nxt_drv) ->
                          let nxt_st = st { _subDriver = nxt_drv } in
                          Produce r $ Await $ Processor $ go nxt_st

    subCmd st@State{..} cmd =
        case cmd of
            ConnectStream k s tos ->
                let (pkg, nxt_drv) = Sub.connectToStream k s tos _subDriver
                    nxt_st         = st { _subDriver = nxt_drv }
                    nxt            = Processor $ go nxt_st in
                Transmit pkg $ Await nxt
            ConnectPersist k g s b ->
                let (pkg, nxt_drv) = Sub.connectToPersist k g s b _subDriver
                    nxt_st         = st { _subDriver = nxt_drv }
                    nxt            = Processor $ go nxt_st in
                Transmit pkg $ Await nxt
            Unsubscribe r ->
                let (pkg, nxt_drv) = Sub.unsubscribe r _subDriver
                    nxt_st         = st { _subDriver = nxt_drv }
                    nxt            = Processor $ go nxt_st in
                Transmit pkg $ Await nxt
            CreatePersist k g s ss ->
                let (pkg, nxt_drv) = Sub.createPersist k g s ss _subDriver
                    nxt_st         = st { _subDriver = nxt_drv }
                    nxt            = Processor $ go nxt_st in
                Transmit pkg $ Await nxt
            UpdatePersist k g s ss ->
                let (pkg, nxt_drv) = Sub.updatePersist k g s ss _subDriver
                    nxt_st         = st { _subDriver = nxt_drv }
                    nxt            = Processor $ go nxt_st in
                Transmit pkg $ Await nxt
            DeletePersist k g s ->
                let (pkg, nxt_drv) = Sub.deletePersist k g s _subDriver
                    nxt_st         = st { _subDriver = nxt_drv }
                    nxt            = Processor $ go nxt_st in
                Transmit pkg $ Await nxt
            AckPersist r run gid evts ->
                let (pkg, nxt_drv) = Sub.ackPersist r run gid evts _subDriver
                    nxt_st         = st { _subDriver = nxt_drv }
                    nxt            = Processor $ go nxt_st in
                Transmit pkg $ Await nxt
            NakPersist r run gid act res evts ->
                let (pkg, nxt_drv) = Sub.nakPersist r run gid act res evts
                                     _subDriver
                    nxt_st         = st { _subDriver = nxt_drv }
                    nxt            = Processor $ go nxt_st in
                Transmit pkg $ Await nxt

--------------------------------------------------------------------------------
-- | Creates a new 'Processor' state-machine.
newProcessor :: Settings -> Generator -> Processor r
newProcessor setts gen = Processor $ handle $ initState setts gen
