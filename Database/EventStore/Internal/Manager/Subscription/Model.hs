{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Manager.Subscription.Model
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Manager.Subscription.Model where

--------------------------------------------------------------------------------
import Data.Int
import Data.Typeable

--------------------------------------------------------------------------------
import qualified Data.HashMap.Strict as H
import           Data.Text
import           Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data PersistAction
    = PersistCreate PersistentSubscriptionSettings
    | PersistUpdate PersistentSubscriptionSettings
    | PersistDelete

--------------------------------------------------------------------------------
data PendingAction =
    PendingAction
    { _paGroup  :: !Text
    , _paStream :: !Text
    , _paTpe    :: !PersistAction
    }

--------------------------------------------------------------------------------
data Box (f :: Type -> *) = forall a. Typeable a => Box (f a)

--------------------------------------------------------------------------------
newtype Register (f :: Type -> *) = Register (H.HashMap UUID (Box f))

--------------------------------------------------------------------------------
emptyReg :: Register f
emptyReg = Register H.empty

--------------------------------------------------------------------------------
regLookup :: Typeable a => proxy a -> UUID -> Register f -> Maybe (f a)
regLookup _ uuid (Register m) = do
    Box b <- H.lookup uuid m
    gcast b

--------------------------------------------------------------------------------
regInsert :: Typeable a => UUID -> f a -> Register f -> Register f
regInsert uuid v (Register m) = Register $ H.insert uuid (Box v) m

--------------------------------------------------------------------------------
regDelete :: UUID -> Register f -> Register f
regDelete u (Register m) = Register $ H.delete u m

--------------------------------------------------------------------------------
regGet :: Typeable a => proxy a -> UUID -> Register f -> f a
regGet p uuid r = let Just v = regLookup p uuid r in v

--------------------------------------------------------------------------------
regPrx :: Proxy 'RegularType
regPrx = Proxy

--------------------------------------------------------------------------------
pendPrx :: Proxy 'PersistType
pendPrx = Proxy

--------------------------------------------------------------------------------
data PendingSub
    = PendingSub Text Bool
    | PendingPersist Text Text Int32

--------------------------------------------------------------------------------
type Table a = H.HashMap UUID a

--------------------------------------------------------------------------------
data Type = RegularType | PersistType

--------------------------------------------------------------------------------
data Pend :: Type -> * where
    PendingReg :: Text -- ^ Stream name.
               -> Bool -- ^ Resolve Link TOS.
               -> Pend 'RegularType

    PendPersist :: Text  -- ^ Group name.
                -> Text  -- ^ Stream name.
                -> Int32 -- ^ Buffer size.
                -> Pend 'PersistType

--------------------------------------------------------------------------------
data Id :: Type -> * where
    RegularId  :: UUID -> Id 'RegularType
    PersistId  :: UUID -> Id 'PersistType

--------------------------------------------------------------------------------
toUUID :: Id t -> UUID
toUUID (RegularId u) = u
toUUID (PersistId u) = u

--------------------------------------------------------------------------------
data Running :: Type -> * where
    RunningReg :: Text  -- ^ Stream name.
               -> Bool  -- ^ Resolve Link TOS.
               -> Int64 -- ^ Last commint position.
               -> Int32 -- ^ Last event number.
               -> Running 'RegularType

    RunningPersist :: Text  -- ^ Group name.
                   -> Text  -- ^ Stream name.
                   -> Int32 -- ^ Buffer size.
                   -> Text  -- ^ Subscription Id.
                   -> Int64 -- ^ Last commit position.
                   -> Int32 -- ^ Last event number.
                   -> Running 'PersistType

--------------------------------------------------------------------------------
data Meta :: Type -> * where
    RegularMeta :: Int64       -- ^ Last commit position.
                -> Maybe Int32 -- ^ Last event number.
                -> Meta 'RegularType

    PersistMeta :: Text  -- ^ Subscription Id.
                -> Int64 -- ^ Last commit position.
                -> Int32 -- ^ Last event number.
                -> Meta 'PersistType

--------------------------------------------------------------------------------
data Mode = Read | Write

--------------------------------------------------------------------------------
data Select a where
    SelectSub    :: Id t -> Select (Maybe (Running t))
    SelectAction :: UUID -> Select (Maybe PendingAction)

--------------------------------------------------------------------------------
data Request :: Mode -> * -> * where
    Query   :: Select a -> Request 'Read a
    Execute :: Action a -> Request 'Write a

--------------------------------------------------------------------------------
data Connect :: Type -> * where
    ConnectReg :: Text -- ^ Stream name.
               -> Bool -- ^ Resolve TOS link.
               -> Connect 'RegularType

    ConnectPersist :: Text  -- ^ Group name.
                   -> Text  -- ^ Stream name.
                   -> Int32 -- ^ Buffer size.
                   -> Connect 'PersistType

--------------------------------------------------------------------------------
data Confirm :: Type -> * -> * where
    -- | Confirm a subscription (regular or not).
    ConfirmSub :: UUID -> Meta t -> Confirm t (Maybe (Id t, Model))

    ConfirmAction :: UUID -> Confirm 'PersistType (Maybe Model)

--------------------------------------------------------------------------------
data Action a where
      -- | Connnect to a subscription.
    ConnectSub :: Connect t -> UUID -> Action Model
    -- | Create a regular subscription.
    -- CreateSub :: Text -- ^ Stream  name.
    --           -> Bool -- ^ Resolve TOS link.
    --           -> UUID -- ^ Reference.
    --           -> Action Package

    -- | Confirm a subscription (regular or not).
    Confirm :: Confirm t a -> Action a

    -- | Unsubscribe.
    UnSub :: Id t -> Action Model

    PersistAction :: Text -> Text -> UUID -> PersistAction -> Action Model

    -- | Update a persistent subscription.
    -- UpdatePersist :: PersistentSubscriptionSettings -> Action Package

--------------------------------------------------------------------------------
data State =
    State
    { _stPending :: !(Register Pend)
    , _stRunning :: !(Register Running)
    , _stAction  :: !(Table PendingAction)
    }

--------------------------------------------------------------------------------
emptyState :: State
emptyState = State emptyReg emptyReg H.empty

--------------------------------------------------------------------------------
newtype Model = Model { _unM :: forall a m. Request m a -> a }

--------------------------------------------------------------------------------
newModel :: Model
newModel = Model $ _modelHandle emptyState

--------------------------------------------------------------------------------
runModel :: Request m a -> Model -> a
runModel req (Model k) = k req

--------------------------------------------------------------------------------
_modelHandle :: State -> Request m a -> a
_modelHandle s (Execute e) =
    case e of
        ConnectSub c u ->
            case c of
                ConnectReg n tos ->
                    _modelConnectReg s n tos u
                ConnectPersist g n b ->
                    _modelConnectPersist s g n b u
        Confirm c ->
            case c of
                ConfirmSub u tpe ->
                    case tpe of
                        RegularMeta lc le ->
                            _modelConfirmRegSub s u lc le
                        PersistMeta sb lc le ->
                            _modelConfirmPersistSub s u sb lc le
                ConfirmAction u ->
                    _modelConfirmPersistAction s u
        UnSub sid -> _modelUnsub s sid
        PersistAction g n u a -> _modelPersistAction s g n u a
_modelHandle s (Query q) =
    case q of
        SelectSub sid  -> _modelLookupSub s sid
        SelectAction u -> _modelLookupAction s u

--------------------------------------------------------------------------------
_modelLookupSub :: State -> Id t -> Maybe (Running t)
_modelLookupSub State{..} (RegularId u) = regLookup regPrx u _stRunning
_modelLookupSub State{..} (PersistId u) = regLookup pendPrx u _stRunning

--------------------------------------------------------------------------------
_modelLookupAction :: State -> UUID -> Maybe PendingAction
_modelLookupAction State{..} u = H.lookup u _stAction

--------------------------------------------------------------------------------
_modelConnectReg :: State
                 -> Text -- ^ Stream name.
                 -> Bool -- ^ Resolve Link TOS.
                 -> UUID
                 -> Model
_modelConnectReg s@State{..} stream tos uuid =
    let m  = regInsert uuid (PendingReg stream tos) _stPending
        s' = s { _stPending = m } in
    Model $ _modelHandle s'

--------------------------------------------------------------------------------
_modelConnectPersist :: State
                     -> Text  -- ^ Group name.
                     -> Text  -- ^ Stream mame.
                     -> Int32 -- ^ Buffer size.
                     -> UUID
                     -> Model
_modelConnectPersist s@State{..} g n b uuid =
    let m  = regInsert uuid (PendPersist g n b) _stPending
        s' = s { _stPending = m } in
    Model $ _modelHandle s'

--------------------------------------------------------------------------------
_modelConfirmRegSub :: State
                    -> UUID
                    -> Int64 -- ^ Last commit position.
                    -> Int32 -- ^ Last event number.
                    -> Maybe (Id 'RegularType, Model)
_modelConfirmRegSub s@State{..} uuid lc le = do
    PendingReg n tos <- regLookup regPrx uuid _stPending
    let r  = RunningReg n tos lc le
        m  = regInsert uuid r _stRunning
        s' = s { _stRunning = m  }
    return (RegularId uuid,  Model $ _modelHandle s')

--------------------------------------------------------------------------------
_modelConfirmPersistSub :: State
                        -> UUID
                        -> Text
                        -> Int64
                        -> Int32
                        -> Maybe (Id 'PersistType, Model)
_modelConfirmPersistSub s@State{..} uuid sb lc le = do
    PendPersist g n b <- regLookup pendPrx uuid _stPending
    let r  = RunningPersist g n b sb lc le
        m  = regInsert uuid r _stRunning
        s' = s { _stRunning = m }
    return (PersistId uuid,  Model $ _modelHandle s')

--------------------------------------------------------------------------------
_modelUnsub :: State -> Id t -> Model
_modelUnsub s@State{..} sid =
    let uuid = toUUID sid
        m    = regDelete uuid _stRunning
        s'   = s { _stRunning = m } in
    Model $ _modelHandle s'

--------------------------------------------------------------------------------
_modelPersistAction :: State
                    -> Text
                    -> Text
                    -> UUID
                    -> PersistAction
                    -> Model
_modelPersistAction s@State{..} g n u a =
    let pa  = PendingAction g n a
        m   = H.insert u pa _stAction
        s'  = s { _stAction = m } in
    Model $ _modelHandle s'

--------------------------------------------------------------------------------
_modelConfirmPersistAction :: State -> UUID -> Maybe Model
_modelConfirmPersistAction s@State{..} u = do
    _ <- H.lookup u _stAction
    let m  = H.delete u _stAction
        s' = s { _stAction = m }
    return $ Model $ _modelHandle s'
