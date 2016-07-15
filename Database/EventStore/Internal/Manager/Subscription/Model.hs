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
    , runningLastEventNumber
    , runningLastCommitPosition
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
import ClassyPrelude
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Type of persistent action.
data PersistAction
    = PersistCreate PersistentSubscriptionSettings
    | PersistUpdate PersistentSubscriptionSettings
    | PersistDelete

--------------------------------------------------------------------------------
-- | Represents an persistent action that hasn't been completed yet.
data PendingAction =
    PendingAction
    { _paGroup  :: !Text
    , _paStream :: !Text
    , _paTpe    :: !PersistAction
    }

--------------------------------------------------------------------------------
type Register a = HashMap UUID a

--------------------------------------------------------------------------------
-- | Represents a 'Subscription' which is about to be confirmed.
data Pending
    = PendingReg Text Bool
      -- ^ Related to regular subscription. In order of appearance:
      --
      --   * Stream name.
      --
      --   * Resolve Link TOS.
    | PendingPersist Text Text Int32
      -- ^ Related to persistent subscription. In order of appearance:
      --
      --   * Group name.
      --
      --   * Stream name.
      --
      --   * Buffer size.
      deriving Show

--------------------------------------------------------------------------------
-- | Represents a running subscription. Gathers useful information.
data Running
    = RunningReg UUID Text Bool Int64 (Maybe Int32)
      -- ^ Related regular subscription. In order of appearance:
      --
      --   * Subscription id.
      --
      --   * Stream name.
      --
      --   * Resolve Link TOS.
      --
      --   * Last commit position.
      --
      --   * Last event number.
    | RunningPersist UUID Text Text Int32 Text Int64 (Maybe Int32)
      -- ^ Related to persistent subscription. In order of appearance:
      --
      --   * Subscription id.
      --
      --   * Group name.
      --
      --   * Stream name.
      --
      --   * Buffer size.
      --
      --   * Persistence subscription id.
      --
      --   * Last commit position.
      --
      --   * Last event number.
      deriving Show

--------------------------------------------------------------------------------
-- | Gets the event number of a running subscription.
runningLastEventNumber :: Running -> Maybe Int32
runningLastEventNumber (RunningReg _ _ _ _ i) = i
runningLastEventNumber (RunningPersist _ _ _ _ _ _ i) = i

--------------------------------------------------------------------------------
-- | Gets the commit position of a running subscription.
runningLastCommitPosition :: Running -> Int64
runningLastCommitPosition (RunningReg _ _ _ i _) = i
runningLastCommitPosition (RunningPersist _ _ _ _ _ i _) = i

--------------------------------------------------------------------------------
-- | Gets the 'UUID' of a running subscription.
runningUUID :: Running -> UUID
runningUUID (RunningReg i _ _ _ _)         = i
runningUUID (RunningPersist i _ _ _ _ _ _) = i

--------------------------------------------------------------------------------
-- | Type of requests handled by the model.
data Request a where
    -- Read request.
    Query :: Query a -> Request a
    -- Write request.
    Execute :: Action -> Request Model

--------------------------------------------------------------------------------
-- | Set of a piece of information we can query from the 'Subscription' model.
data Query a where
    -- Query a running 'Subscription'.
    QuerySub :: UUID -> Query (Maybe Running)
    -- Query a pending persistent action.
    QueryAction :: UUID -> Query (Maybe PendingAction)

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
emptyState = State mempty mempty mempty

--------------------------------------------------------------------------------
-- | Subscription operations state machine. Keeps every information related to
--   subscription updated.
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
                        nxt_ps = insertMap u p $ _stPending s
                        nxt_s  = s { _stPending = nxt_ps } in
                    Model $ modelHandle nxt_s
                ConnectPersist g n b ->
                    let p      = PendingPersist g n b
                        nxt_ps = insertMap u p $ _stPending s
                        nxt_s  = s { _stPending = nxt_ps } in
                    Model $ modelHandle nxt_s
        Confirmed c ->
            case c of
                ConfirmedConnection u tpe ->
                    case tpe of
                        RegularMeta lc le ->
                            case lookup u $ _stPending s of
                              Just (PendingReg n tos) ->
                                  let r      = RunningReg u n tos lc le
                                      nxt_rs = insertMap u r $ _stRunning s
                                      nxt_s  = s { _stRunning = nxt_rs } in
                                  Model $ modelHandle nxt_s
                              _ -> Model $ modelHandle s
                        PersistMeta sb lc le ->
                            case lookup u $ _stPending s of
                                Just (PendingPersist g n b) ->
                                    let r      = RunningPersist u g n b sb lc le
                                        nxt_rs = insertMap u r $ _stRunning s
                                        nxt_s  = s { _stRunning = nxt_rs } in
                                    Model $ modelHandle nxt_s
                                _ -> Model $ modelHandle s
                ConfirmedPersistAction u ->
                    case lookup u $ _stAction s of
                        Just (PendingAction{}) ->
                            let nxt_as = deleteMap u $ _stAction s
                                nxt_s  = s { _stAction = nxt_as } in
                            Model $ modelHandle nxt_s
                        _ -> Model $ modelHandle s
        Unsubscribed u ->
            let nxt_ps = deleteMap u $ _stRunning s
                nxt_s  = s { _stRunning = nxt_ps } in
            Model $ modelHandle nxt_s
        PersistAction g n u t ->
            let a      = PendingAction g n t
                nxt_as = insertMap u a $ _stAction s
                nxt_s  = s { _stAction = nxt_as } in
            Model $ modelHandle nxt_s
modelHandle s (Query q) =
    case q of
        QuerySub u    -> lookup u $ _stRunning s
        QueryAction u -> lookup u $ _stAction s
