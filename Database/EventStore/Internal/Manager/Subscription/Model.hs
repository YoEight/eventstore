{-# LANGUAGE GADTs           #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RecordWildCards #-}
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
-- Main Subscription bookkeeping structure.
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Manager.Subscription.Model
    ( PersistAction(..)
    , PendingAction(..)
    , Running(..)
    , Meta(..)
    , Model
    , runningUUID
    , querySubscription
    , queryPersistentAction
    , confirmedSubscription
    , confirmedAction
    , newModel
    , unsubscribed
    , connectReg
    , connectPersist
    , persistAction
    ) where

--------------------------------------------------------------------------------
import Data.Int

--------------------------------------------------------------------------------
import qualified Data.HashMap.Strict as H
import           Data.Text
import           Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Type of persistent action.
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
type Register a = H.HashMap UUID a

--------------------------------------------------------------------------------
-- | Represents a 'Subscription' which is about to be
--   confirmed.
data Pending
    = PendingReg Text Bool
      --         |    |--- Stream name.
      --         |-------- Resolve Link TOS.
    | PendingPersist Text Text Int32
      --             |    |    |----- Buffer size.
      --             |    |---------- Stream name.
      --             |--------------- Group name.

--------------------------------------------------------------------------------
-- | Represents a running 'Subscription'. Gathers useful information.
data Running
    = RunningReg UUID Text Bool Int64 (Maybe Int32)
      --         |    |    |    |     |------------- Last event number.
      --         |    |    |    |------------------- Last commit position.
      --         |    |    |------------------------ Resolve Link TOS.
      --         |    |----------------------------- Stream name.
      --         |---------------------------------- Sub. id.
    | RunningPersist UUID Text Text Int32 Text Int64 (Maybe Int32)
      --             |    |    |    |     |    |     |------------- Last event
      --             |    |    |    |     |    |                    number.
      --             |    |    |    |     |    |------------------- Last commit
      --             |    |    |    |     |                         position
      --             |    |    |    |     |------------------------ Sub. id.
      --             |    |    |    |------------------------------ buffer size.
      --             |    |    |----------------------------------- Stream name.
      --             |    |---------------------------------------- Group name.
      --             |--------------------------------------------- Sub. id.
      deriving Show

--------------------------------------------------------------------------------
runningUUID :: Running -> UUID
runningUUID (RunningReg i _ _ _ _)         = i
runningUUID (RunningPersist i _ _ _ _ _ _) = i

--------------------------------------------------------------------------------
-- | Type of requests handled by the model.
data Request a where
    Query :: Query a -> Request a
    -- ^ Read request.
    Execute :: Action -> Request Model
    -- ^ Write request.

--------------------------------------------------------------------------------
-- | Set of a piece of information we can query from the 'Subscription' model.
data Query a where
    QuerySub :: UUID -> Query (Maybe Running)
    -- ^ Query a running 'Subscription'.
    QueryAction :: UUID -> Query (Maybe PendingAction)
    -- ^ Query a pending persistent action.

--------------------------------------------------------------------------------
-- | Set of actions handled by the 'Subscription' model.
data Action
    = Connect UUID Connect
      -- ^ Subscription connection.
    | Confirmed Confirmed
      -- ^ Subscription action confirmation.
    | Unsubscribed UUID
      -- ^ Subscription no longer exist.
    | PersistAction Text Text UUID PersistAction
      -- ^ Add a new persist action.

--------------------------------------------------------------------------------
-- | Subscription connection information.
data Connect
    = ConnectReg Text Bool
      --         |    |---- Resolve TOS link.
      --         |--------- Stream name.
    | ConnectPersist Text Text Int32
      --             |    |    |---- Buffer size.
      --             |    |--------- Stream name.
      --             |-------------- Group name.

--------------------------------------------------------------------------------
-- | Information related to a confirmed 'Subscription'.
data Meta
    = RegularMeta Int64 (Maybe Int32)
      --          |     |------------- Last commit position.
      --          |------------------- Last event number.
    | PersistMeta Text Int64 (Maybe Int32)
      --          |    |     |------------- Subscription Id.
      --          |    |------------------- Last commit position.
      --          |------------------------ Last event number.

--------------------------------------------------------------------------------
-- | Subscription action confirmation.
data Confirmed
    = ConfirmedConnection UUID Meta
      -- ^ Confirms a 'Subscription' connection has handled successfully.
    | ConfirmedPersistAction UUID
      -- ^ Confirms a persist action has been handled successfully.

--------------------------------------------------------------------------------
-- | Retrieves a running 'Subscription'.
querySubscription :: UUID -> Model -> Maybe Running
querySubscription u (Model k) = k $ Query $ QuerySub u

--------------------------------------------------------------------------------
-- | Retrieves an ongoing persistent action.
queryPersistentAction :: UUID -> Model -> Maybe PendingAction
queryPersistentAction u (Model k) = k $ Query $ QueryAction u

--------------------------------------------------------------------------------
-- | Registers a regular 'Subscription' request.
connectReg :: Text -> Bool -> UUID -> Model -> Model
connectReg n t u (Model k) = k $ Execute $ Connect u (ConnectReg n t)

--------------------------------------------------------------------------------
-- | Registers a persistent 'Subscription' request.
connectPersist :: Text -> Text -> Int32 -> UUID -> Model -> Model
connectPersist g n b u (Model k) =
    k $ Execute $ Connect u (ConnectPersist g n b)

--------------------------------------------------------------------------------
-- | Registers a persistent action.
persistAction :: Text -> Text -> UUID -> PersistAction -> Model -> Model
persistAction g n u a (Model k) = k $ Execute $ PersistAction g n u a

--------------------------------------------------------------------------------
-- | Confirms a subscription.
confirmedSubscription :: UUID -> Meta -> Model -> Model
confirmedSubscription u m (Model k) =
    k $ Execute $ Confirmed $ ConfirmedConnection u m

--------------------------------------------------------------------------------
-- | Confirms a persistent action. It doesn't assume if the action went well.
confirmedAction :: UUID -> Model -> Model
confirmedAction u (Model k) = k $ Execute $ Confirmed $ ConfirmedPersistAction u

--------------------------------------------------------------------------------
-- | Remove a 'Subscription'.
unsubscribed :: Running -> Model -> Model
unsubscribed r (Model k) = k $ Execute $ Unsubscribed $ runningUUID r

--------------------------------------------------------------------------------
-- | 'Subscription' model internal state.
data State =
    State
    { _stPending :: !(Register Pending)
      -- ^ Holds all pending 'Subscription's
    , _stRunning :: !(Register Running)
      -- ^ Holds all 'Subscription's that are currently running.
    , _stAction  :: !(Register PendingAction)
      -- ^ Holds all pending persistent actions.
    }

--------------------------------------------------------------------------------
emptyState :: State
emptyState = State H.empty H.empty H.empty

--------------------------------------------------------------------------------
newtype Model = Model (forall a. Request a -> a)

--------------------------------------------------------------------------------
-- | Creates a new 'Subscription' model.
newModel :: Model
newModel = Model $ modelHandle emptyState

--------------------------------------------------------------------------------
-- | Main model handler.
modelHandle :: State -> Request a -> a
modelHandle s (Execute e) =
    case e of
        Connect u c ->
            case c of
                ConnectReg n tos ->
                    let p      = PendingReg n tos
                        nxt_ps = H.insert u p $ _stPending s
                        nxt_s  = s { _stPending = nxt_ps } in
                    Model $ modelHandle nxt_s
                ConnectPersist g n b ->
                    let p      = PendingPersist g n b
                        nxt_ps = H.insert u p $ _stPending s
                        nxt_s  = s { _stPending = nxt_ps } in
                    Model $ modelHandle nxt_s
        Confirmed c ->
            case c of
                ConfirmedConnection u tpe ->
                    case tpe of
                        RegularMeta lc le ->
                            case H.lookup u $ _stPending s of
                              Just (PendingReg n tos) ->
                                  let r      = RunningReg u n tos lc le
                                      nxt_rs = H.insert u r $ _stRunning s
                                      nxt_s  = s { _stRunning = nxt_rs } in
                                  Model $ modelHandle nxt_s
                              _ -> Model $ modelHandle s
                        PersistMeta sb lc le ->
                            case H.lookup u $ _stPending s of
                                Just (PendingPersist g n b) ->
                                    let r      = RunningPersist u g n b sb lc le
                                        nxt_rs = H.insert u r $ _stRunning s
                                        nxt_s  = s { _stRunning = nxt_rs } in
                                    Model $ modelHandle nxt_s
                                _ -> Model $ modelHandle s
                ConfirmedPersistAction u ->
                    case H.lookup u $ _stAction s of
                        Just (PendingAction{}) ->
                            let nxt_as = H.delete u $ _stAction s
                                nxt_s  = s { _stAction = nxt_as } in
                            Model $ modelHandle nxt_s
                        _ -> Model $ modelHandle s
        Unsubscribed u ->
            let nxt_ps = H.delete u $ _stRunning s
                nxt_s  = s { _stRunning = nxt_ps } in
            Model $ modelHandle nxt_s
        PersistAction g n u t ->
            let a      = PendingAction g n t
                nxt_as = H.insert u a $ _stAction s
                nxt_s  = s { _stAction = nxt_as } in
            Model $ modelHandle nxt_s
modelHandle s (Query q) =
    case q of
        QuerySub u    -> H.lookup u $ _stRunning s
        QueryAction u -> H.lookup u $ _stAction s
