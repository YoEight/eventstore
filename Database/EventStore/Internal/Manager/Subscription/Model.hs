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
module Database.EventStore.Internal.Manager.Subscription.Model
    ( PersistAction(..)
    , ConfirmedAction(..)
    , PendingAction(..)
    , Running(..)
    , Meta(..)
    , Id(..)
    , Box(..)
    , Model
    , runningId
    , queryRegSub
    , queryPersistSub
    , querySomeSub
    , queryAction
    , confirmConnect
    , confirmAction
    , newModel
    , runModel
    , unsub
    , connectReg
    , connectPersist
    , persistAction
    ) where

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
data ConfirmedAction =
    ConfirmedAction
    { caId     :: !UUID
    , caGroup  :: !Text
    , caStream :: !Text
    , caAction :: !PersistAction
    }

--------------------------------------------------------------------------------
data Box (f :: Type -> *) = forall a. Typeable a => Box (f a)

--------------------------------------------------------------------------------
newtype Register (f :: Type -> *) = Register (H.HashMap UUID (Box f))

--------------------------------------------------------------------------------
emptyReg :: Register f
emptyReg = Register H.empty

--------------------------------------------------------------------------------
regLookupBox :: UUID -> Register f -> Maybe (Box f)
regLookupBox u (Register m) = H.lookup u m

--------------------------------------------------------------------------------
regLookup :: Typeable a => proxy a -> UUID -> Register f -> Maybe (f a)
regLookup _ uuid reg = do
    Box b <- regLookupBox uuid reg
    gcast b

--------------------------------------------------------------------------------
regInsert :: Typeable a => UUID -> f a -> Register f -> Register f
regInsert uuid v (Register m) = Register $ H.insert uuid (Box v) m

--------------------------------------------------------------------------------
regDelete :: UUID -> Register f -> Register f
regDelete u (Register m) = Register $ H.delete u m

--------------------------------------------------------------------------------
regPrx :: Proxy 'RegularType
regPrx = Proxy

--------------------------------------------------------------------------------
pendPrx :: Proxy 'PersistType
pendPrx = Proxy

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
    RunningReg :: Id 'RegularType
               -> Text        -- ^ Stream name.
               -> Bool        -- ^ Resolve Link TOS.
               -> Int64       -- ^ Last commint position.
               -> Maybe Int32 -- ^ Last event number.
               -> Running 'RegularType

    RunningPersist :: Id 'PersistType
                   -> Text        -- ^ Group name.
                   -> Text        -- ^ Stream name.
                   -> Int32       -- ^ Buffer size.
                   -> Text        -- ^ Subscription Id.
                   -> Int64       -- ^ Last commit position.
                   -> Maybe Int32 -- ^ Last event number.
                   -> Running 'PersistType

--------------------------------------------------------------------------------
runningId :: Running t -> Id t
runningId (RunningReg i _ _ _ _)         = i
runningId (RunningPersist i _ _ _ _ _ _) = i

--------------------------------------------------------------------------------
data Meta :: Type -> * where
    RegularMeta :: Int64       -- ^ Last commit position.
                -> Maybe Int32 -- ^ Last event number.
                -> Meta 'RegularType

    PersistMeta :: Text        -- ^ Subscription Id.
                -> Int64       -- ^ Last commit position.
                -> Maybe Int32 -- ^ Last event number.
                -> Meta 'PersistType

--------------------------------------------------------------------------------
data Mode = Read | Write

--------------------------------------------------------------------------------
data Select a where
    SelectSub    :: Id t -> Select (Maybe (Running t))
    SelectSome   :: UUID -> Select (Maybe (Box Running))
    SelectAction :: UUID -> Select (Maybe PendingAction)

--------------------------------------------------------------------------------
queryRegSub :: UUID -> Request 'Read (Maybe (Running 'RegularType))
queryRegSub = Query . SelectSub . RegularId

--------------------------------------------------------------------------------
queryPersistSub :: UUID -> Request 'Read (Maybe (Running 'PersistType))
queryPersistSub = Query . SelectSub . PersistId

--------------------------------------------------------------------------------
querySomeSub :: UUID -> Request 'Read (Maybe (Box Running))
querySomeSub = Query . SelectSome

--------------------------------------------------------------------------------
queryAction :: UUID -> Request 'Read (Maybe PendingAction)
queryAction = Query . SelectAction

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
connectReg :: Text -> Bool -> UUID -> Request 'Write Model
connectReg n t u = Execute $ ConnectSub (ConnectReg n t) u

--------------------------------------------------------------------------------
connectPersist :: Text -> Text -> Int32 -> UUID -> Request 'Write Model
connectPersist g n b u = Execute $ ConnectSub (ConnectPersist g n b) u

--------------------------------------------------------------------------------
persistAction :: Text -> Text -> UUID -> PersistAction -> Request 'Write Model
persistAction g n u a = Execute $ PersistAction g n u a

--------------------------------------------------------------------------------
data Confirm :: Type -> * -> * where
    -- | Confirm a subscription (regular or not).
    ConfirmSub :: UUID -> Meta t -> Confirm t (Maybe (Running t, Model))

    ConfirmAction :: UUID
                  -> Confirm 'PersistType (Maybe (ConfirmedAction, Model))

--------------------------------------------------------------------------------
confirmConnect :: UUID -> Meta t -> Request 'Write (Maybe (Running t, Model))
confirmConnect u m = Execute $ Confirm $ ConfirmSub u m

--------------------------------------------------------------------------------
confirmAction :: UUID -> Request 'Write (Maybe (ConfirmedAction, Model))
confirmAction = Execute . Confirm . ConfirmAction

--------------------------------------------------------------------------------
data Action a where
      -- | Connnect to a subscription.
    ConnectSub :: Connect t -> UUID -> Action Model

    -- | Confirm a subscription (regular or not).
    Confirm :: Confirm t a -> Action a

    -- | Unsubscribe.
    UnSub :: Id t -> Action Model

    PersistAction :: Text -> Text -> UUID -> PersistAction -> Action Model

--------------------------------------------------------------------------------
unsub :: Id t -> Request 'Write Model
unsub = Execute . UnSub

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
newModel = Model $ modelHandle emptyState

--------------------------------------------------------------------------------
runModel :: Request m a -> Model -> a
runModel req (Model k) = k req

--------------------------------------------------------------------------------
modelHandle :: State -> Request m a -> a
modelHandle s (Execute e) =
    case e of
        ConnectSub c u ->
            case c of
                ConnectReg n tos ->
                    modelConnectReg s n tos u
                ConnectPersist g n b ->
                    modelConnectPersist s g n b u
        Confirm c ->
            case c of
                ConfirmSub u tpe ->
                    case tpe of
                        RegularMeta lc le ->
                            modelConfirmRegSub s u lc le
                        PersistMeta sb lc le ->
                            modelConfirmPersistSub s u sb lc le
                ConfirmAction u ->
                    modelConfirmPersistAction s u
        UnSub sid -> modelUnsub s sid
        PersistAction g n u a -> modelPersistAction s g n u a
modelHandle s (Query q) =
    case q of
        SelectSub sid  -> modelLookupSub s sid
        SelectAction u -> modelLookupAction s u
        SelectSome u   -> modelLookupSome s u

--------------------------------------------------------------------------------
modelLookupSub :: State -> Id t -> Maybe (Running t)
modelLookupSub State{..} (RegularId u) = regLookup regPrx u _stRunning
modelLookupSub State{..} (PersistId u) = regLookup pendPrx u _stRunning

--------------------------------------------------------------------------------
modelLookupAction :: State -> UUID -> Maybe PendingAction
modelLookupAction State{..} u = H.lookup u _stAction

--------------------------------------------------------------------------------
modelLookupSome :: State -> UUID -> Maybe (Box Running)
modelLookupSome State{..} u = regLookupBox u _stRunning

--------------------------------------------------------------------------------
modelConnectReg :: State
                 -> Text -- ^ Stream name.
                 -> Bool -- ^ Resolve Link TOS.
                 -> UUID
                 -> Model
modelConnectReg s@State{..} stream tos uuid =
    let m  = regInsert uuid (PendingReg stream tos) _stPending
        s' = s { _stPending = m } in
    Model $ modelHandle s'

--------------------------------------------------------------------------------
modelConnectPersist :: State
                     -> Text  -- ^ Group name.
                     -> Text  -- ^ Stream mame.
                     -> Int32 -- ^ Buffer size.
                     -> UUID
                     -> Model
modelConnectPersist s@State{..} g n b uuid =
    let m  = regInsert uuid (PendPersist g n b) _stPending
        s' = s { _stPending = m } in
    Model $ modelHandle s'

--------------------------------------------------------------------------------
modelConfirmRegSub :: State
                   -> UUID
                   -> Int64       -- ^ Last commit position.
                   -> Maybe Int32 -- ^ Last event number.
                   -> Maybe (Running 'RegularType, Model)
modelConfirmRegSub s@State{..} uuid lc le = do
    PendingReg n tos <- regLookup regPrx uuid _stPending
    let si = RegularId uuid
        r  = RunningReg si n tos lc le
        m  = regInsert uuid r _stRunning
        s' = s { _stRunning = m  }
    return (r, Model $ modelHandle s')

--------------------------------------------------------------------------------
modelConfirmPersistSub :: State
                        -> UUID
                        -> Text
                        -> Int64
                        -> Maybe Int32
                        -> Maybe (Running 'PersistType, Model)
modelConfirmPersistSub s@State{..} uuid sb lc le = do
    PendPersist g n b <- regLookup pendPrx uuid _stPending
    let si = PersistId uuid
        r  = RunningPersist si g n b sb lc le
        m  = regInsert uuid r _stRunning
        s' = s { _stRunning = m }
    return (r,  Model $ modelHandle s')

--------------------------------------------------------------------------------
modelUnsub :: State -> Id t -> Model
modelUnsub s@State{..} sid =
    let uuid = toUUID sid
        m    = regDelete uuid _stRunning
        s'   = s { _stRunning = m } in
    Model $ modelHandle s'

--------------------------------------------------------------------------------
modelPersistAction :: State
                    -> Text
                    -> Text
                    -> UUID
                    -> PersistAction
                    -> Model
modelPersistAction s@State{..} g n u a =
    let pa  = PendingAction g n a
        m   = H.insert u pa _stAction
        s'  = s { _stAction = m } in
    Model $ modelHandle s'

--------------------------------------------------------------------------------
modelConfirmPersistAction :: State -> UUID -> Maybe (ConfirmedAction, Model)
modelConfirmPersistAction s@State{..} u = do
    PendingAction g n a  <- H.lookup u _stAction
    let m  = H.delete u _stAction
        s' = s { _stAction = m }
        c  = ConfirmedAction u g n a
    return (c, Model $ modelHandle s')
