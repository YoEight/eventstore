{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Subscription
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
-- Main subscription state machine declaration module. It also declares every
-- functions required to drive a 'Subscription'.
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Subscription
    ( Regular(..)
    , Persistent(..)
    , Catchup(..)
    , CatchupParams(..)
    , Running(..)
    , Checkpoint(..)
    , Subscription
    , SubscriptionId
    , SubscriptionClosed(..)
    , SubEnv(..)
    , PushCmd(..)
    , AckCmd(..)
    , catchupSub
    , regularSub
    , persistentSub
    , hasCaughtUp
    , getSubId
    , getSubStream
    , unsubscribe
    , isSubscribedToAll
    , getSubLastCommitPos
    , getSubLastEventNumber
    , nextEventMaybeSTM
    , nextEvent
    , nextEventMaybe
    , waitConfirmation
    , getSubResolveLinkTos
    , waitTillCatchup
    , hasCaughtUpSTM
    , notifyEventsProcessed
    , acknowledge
    , acknowledgeEvents
    , failed
    , eventsFailed
    , notifyEventsFailed
    , unsubscribeConfirmed
    , waitUnsubscribeConfirmed
    , unsubscribeConfirmedSTM
    ) where

--------------------------------------------------------------------------------
import Data.Int

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Sequence (ViewL(..), viewl, dropWhileL)
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.EndPoint
import Database.EventStore.Internal.Manager.Subscription.Driver hiding (unsubscribe)
import Database.EventStore.Internal.Manager.Subscription.Model
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Catchup
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Also referred as volatile subscription. For example, if a stream has 100
--   events in it when a subscriber connects, the subscriber can expect to see
--   event number 101 onwards until the time the subscription is closed or
--   dropped.
data Regular = Regular { _subTos :: Bool }

--------------------------------------------------------------------------------
-- | This kind of subscription specifies a starting point, in the form of an
--   event number or transaction file position. The given function will be
--   called for events from the starting point until the end of the stream, and
--   then for subsequently written events.
--
--   For example, if a starting point of 50 is specified when a stream has 100
--   events in it, the subscriber can expect to see events 51 through 100, and
--   then any events subsequently written until such time as the subscription is
--   dropped or closed.
data Catchup = Catchup

--------------------------------------------------------------------------------
-- | The server remembers the state of the subscription. This allows for many
--   different modes of operations compared to a regular or catchup subscription
--   where the client holds the subscription state.
--   (Need EventStore >= v3.1.0).
data Persistent = Persistent { _perGroup  :: Text }

--------------------------------------------------------------------------------
-- | Represents the different type of inputs a subscription state-machine can
--   handle.
data Input t a where
    -- A event has written to the stream. Subscription state machine should
    -- store that event withing its state.
    Arrived :: ResolvedEvent -> Input t (SubStateMachine t)
    -- The user asks for the next event coming from the server.
    ReadNext :: Input t (Maybe (ResolvedEvent, SubStateMachine t))
    -- A batch read has been made. It's only use for 'Catchup' subscription
    -- type. It gives the list of read events and indicates if it reaches the
    -- end of the stream along with the next checkpoint to point at.
    BatchRead :: [ResolvedEvent]
              -> Bool
              -> Checkpoint
              -> Input Catchup (SubStateMachine Catchup)
    -- Used only for 'Catchup' subscription type. Asks if the subscription
    -- read every events up to the checkpoint given by the user.
    CaughtUp :: Input Catchup Bool

    -- Returns the last event number read by the user.
    LastEventNum :: Input Catchup (Int32, Maybe Position)

--------------------------------------------------------------------------------
-- | Main subscription state machine.
newtype SubStateMachine t = SubStateMachine (forall a. Input t a -> a)

--------------------------------------------------------------------------------
type PushOp a = (Either OperationError a -> IO ()) -> Operation a -> IO ()

--------------------------------------------------------------------------------
type PushConnect = (SubConnectEvent -> IO ()) -> PushCmd -> IO ()

--------------------------------------------------------------------------------
data PushCmd
    = PushRegular Text Bool
    | PushPersistent Text Text Int32

--------------------------------------------------------------------------------
data AckCmd = AckCmd | NakCmd NakAction (Maybe Text)

--------------------------------------------------------------------------------
data SubEnv =
    SubEnv { subSettings :: Settings
           , subPushOp :: forall a. PushOp a
           , subPushConnect :: PushConnect
           , subPushUnsub :: Running -> IO ()
           , subAckCmd :: AckCmd -> Running -> [UUID] -> IO ()
           , subForceReconnect :: NodeEndPoints -> IO ()
           }

--------------------------------------------------------------------------------
data SubState t
    = SubOnline (SubStateMachine t)
    | SubDropped SubDropReason
    | forall e. Exception e => SubException e
    | SubUserUnsubscribed

--------------------------------------------------------------------------------
-- | Modifies 'SubState' internal state machine, letting any 'SubDropReason'
--   untouched.
modifySubSM :: (SubStateMachine a -> SubStateMachine a)
            -> SubState a
            -> SubState a
modifySubSM k (SubOnline sm) = SubOnline (k sm)
modifySubSM _ s = s

--------------------------------------------------------------------------------
data CatchupParams =
    CatchupParams { catchupResLnkTos :: !Bool
                  , catchupState :: !CatchupState
                  , catchupBatchSize :: !(Maybe Int32)
                  }

--------------------------------------------------------------------------------
-- | Represents a subscription life cycle.
data SubLifeCycle a =
    SubLifeCycle
    {
      onConfirm :: Running -> IO ()
      -- ^ When the server's confirmed this subscription's been created.
    , readState :: STM (SubState a)
      -- ^ Reads this subscription internal state.
    , writeState :: SubState a -> STM ()
      -- ^ Modifies this subscription internal state.
    , onError :: SubDropReason -> IO ()
      -- ^ When an error's occured.
    , onUserUnsubscribed :: IO ()
      -- ^ When the server confirmed the subscription is no longer live.
      --   This action is triggered because the user asks to unsubscribe.
    , retrySub :: IO ()
     -- ^ Retry the all subscription, this behavior is transparent to the user.
    }

--------------------------------------------------------------------------------
-- | It's possible to subscribe to a stream and be notified when new events are
--   written to that stream. There are three types of subscription which are
--   available, all of which can be useful in different situations.
--
--     * 'Regular'
--
--     * 'Catchup'
--
--     * 'Persistent'
data Subscription t =
    Subscription { subStream :: Text
                 , subLifeCycle :: SubLifeCycle t
                 , subEnv :: SubEnv
                 , subRun :: TMVar Running
                 , subType :: t
                 }

--------------------------------------------------------------------------------
-- | This exception is raised when the user tries to get the next event from a
--   'Subscription' that is already closed.
data SubscriptionClosed
    = SubscriptionClosed SubDropReason
    | SubscriptionUnsubscribedByUser
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception SubscriptionClosed

--------------------------------------------------------------------------------
catchupSub :: SubEnv -> CatchupParams -> IO (Subscription Catchup)
catchupSub env params = do
    mvarRun <- newEmptyTMVarIO
    mvarState <- newEmptyTMVarIO

    let streamId = catchupStreamName $ catchupState params
        pushCmd = PushRegular streamId (catchupResLnkTos params)
        lcycle =
            SubLifeCycle
            { onConfirm = confirmSub mvarRun
            , readState = readTMVar mvarState
            , writeState = \s -> () <$ swapTMVar mvarState s
            , onError = \e ->
                case e of
                    SubAborted ->
                        tryRetryCatcupSubscription pushCmd env mvarState
                            lcycle params
                    SubNotHandled reason infoM ->
                        subNotHandledMsg env lcycle reason infoM
                    _ -> atomically $ do
                        s <- takeTMVar mvarState
                        case s of
                            SubOnline{} -> putTMVar mvarState $ SubDropped e
                            _ -> putTMVar mvarState s
            , onUserUnsubscribed = atomically $ do
                  _ <- takeTMVar mvarState
                  putTMVar mvarState SubUserUnsubscribed
            , retrySub = tryRetryCatcupSubscription pushCmd env mvarState
                             lcycle params
            }

        op = createCatchupOperation env params

    subPushOp env (catchupOpEventHandler mvarState) op
    subPushConnect env (subEventHandler lcycle) pushCmd
    return $ Subscription streamId lcycle env mvarRun Catchup

--------------------------------------------------------------------------------
tryRetryCatcupSubscription :: PushCmd
                           -> SubEnv
                           -> TMVar (SubState Catchup)
                           -> SubLifeCycle Catchup
                           -> CatchupParams
                           -> IO ()
tryRetryCatcupSubscription pushCmd env mvarState lcycle params = do
    state <- atomically $ readTMVar mvarState

    case state of
        -- In this case, we do our best to re-engage the
        -- catchup subscription where it was at before
        -- losing the connection with the server.
        SubOnline sm -> do
            let (num, posM) = lastEventNumSM sm
                newStart =
                  case catchupState params of
                      RegularCatchup stream _ ->
                          RegularCatchup stream num
                      AllCatchup{} ->
                          case posM of
                              Just (Position npc npp) ->
                                  AllCatchup npc npp
                              _ -> catchupState params

                newParams = params { catchupState = newStart }

                newOp = createCatchupOperation env newParams

            subPushOp env (catchupOpEventHandler mvarState)
                newOp
            subPushConnect env (subEventHandler lcycle)
                pushCmd
        _ -> return ()

--------------------------------------------------------------------------------
regularSub :: SubEnv -> Text -> Bool -> IO (Subscription Regular)
regularSub env streamId resLnkTos = do
    mvarRun <- newEmptyTMVarIO
    varState <- newTVarIO $ SubOnline regularSubscription
    let lcycle =
            SubLifeCycle
            { onConfirm = confirmSub mvarRun
            , readState = readTVar varState
            , writeState = writeTVar varState
            , onError = \r -> atomically $ do
                  s <- readTVar varState
                  case s of
                      SubOnline{} -> writeTVar varState $ SubDropped r
                      _ -> return ()
            , onUserUnsubscribed =
                  atomically $ writeTVar varState SubUserUnsubscribed
            , retrySub = do
                  atomically $ do
                      state <- readState lcycle
                      case state of
                          SubOnline{} -> return ()
                          _ -> writeState lcycle $ SubOnline regularSubscription
                  subPushConnect env (subEventHandler lcycle)
                                     (PushRegular streamId resLnkTos)
            }

    subPushConnect env (subEventHandler lcycle) (PushRegular streamId resLnkTos)
    return $ Subscription streamId lcycle env mvarRun (Regular resLnkTos)

--------------------------------------------------------------------------------
persistentSub :: SubEnv -> Text -> Text -> Int32 -> IO (Subscription Persistent)
persistentSub env grp stream bufSize = do
    mvarRun <- newEmptyTMVarIO
    varState <- newTVarIO $ SubOnline persistentSubscription
    let lcycle =
            SubLifeCycle
            { onConfirm = confirmSub mvarRun
            , readState = readTVar varState
            , writeState = writeTVar varState
            , onError = \r -> atomically $ do
                  s <- readTVar varState
                  case s of
                      SubOnline{} -> writeTVar varState $ SubDropped r
                      _ -> return ()
            , onUserUnsubscribed =
                  atomically $ writeTVar varState SubUserUnsubscribed
            , retrySub = do
                  atomically $ writeState lcycle
                             $ SubOnline persistentSubscription
                  subPushConnect env (subEventHandler lcycle) pushCmd
            }

        pushCmd = PushPersistent grp stream bufSize

    subPushConnect env (subEventHandler lcycle) pushCmd
    return $ Subscription stream lcycle env mvarRun (Persistent grp)

--------------------------------------------------------------------------------
-- | Makes sure to not cause deadlock because the subscription already been
-- confirmed but because of a connection drop, need to be recconfirmed again.
confirmSub :: TMVar Running -> Running -> IO ()
confirmSub mvarRun r = atomically $ do
  emptyVar <- isEmptyTMVar mvarRun
  if emptyVar
    then putTMVar mvarRun r
    else () <$ swapTMVar mvarRun r

--------------------------------------------------------------------------------
subNotHandledMsg :: SubEnv
                 -> SubLifeCycle s
                 -> NotHandledReason
                 -> Maybe MasterInfo
                 -> IO ()
subNotHandledMsg env _ N_NotMaster (Just info) =
    subForceReconnect env $ masterInfoNodeEndPoints info
subNotHandledMsg _ lcycle N_NotMaster _ =
    atomically $ writeState lcycle
               $ SubDropped $ SubServerError (Just msg)
  where
    msg = "Been asked to connect to new master node \
          \ but no master info been sent."
subNotHandledMsg _ lcycle N_NotReady _ = retrySub lcycle
subNotHandledMsg _ lcycle N_TooBusy _ = retrySub lcycle

--------------------------------------------------------------------------------
createCatchupOperation :: SubEnv -> CatchupParams -> Operation CatchupOpResult
createCatchupOperation env params =
  catchup (subSettings env)
          (catchupState params)
          (catchupResLnkTos params)
          (catchupBatchSize params)

--------------------------------------------------------------------------------
-- | We want to notify the user that something went wrong in the first phase of
--   a catchup subscription (e.g. reading the stream forward until we catchup to
--   stream's end). This prevents a deadlock on user side in case where the user
--   calls `waitTillCatchup` on a stream that doesn't exist.
catchupOpEventHandler :: TMVar (SubState Catchup)
                      -> Either OperationError CatchupOpResult
                      -> IO ()
catchupOpEventHandler mvarState (Left e) = atomically $ do
    isEmpty <- isEmptyTMVar mvarState
    if isEmpty
        then putTMVar mvarState (SubException e)
        else () <$ swapTMVar mvarState (SubException e)
catchupOpEventHandler mvarState (Right res) = atomically $ do
    -- When a catchup subscription receives events for the
    -- first time.
    whenM (isEmptyTMVar mvarState) $ do
        let initState = SubOnline catchupSubscription
        putTMVar mvarState initState

    subState <- takeTMVar mvarState
    let cmd = batchReadSM (catchupReadEvents res)
                          (catchupEndOfStream res)
                          (catchupCheckpoint res)

        nxtSubState = modifySubSM cmd subState

    putTMVar mvarState nxtSubState

--------------------------------------------------------------------------------
-- | Subscription event handler. Used during a subscription lifetime.
subEventHandler :: SubLifeCycle a -> SubConnectEvent -> IO ()
subEventHandler lcycle (SubConfirmed run) = onConfirm lcycle run
subEventHandler lcycle (EventAppeared e) = atomically $ do
    st <- readState lcycle
    case st of
        SubOnline sm ->
            writeState lcycle $ SubOnline $ eventArrivedSM e sm
        SubException _ ->
            -- At this moment [07 October 2016], this can only happen during
            -- the first phase of a catchup subscription where the user
            -- asked for a subscription on a stream that doesn't exist.
            return ()
        _ -> error "Impossible: subEventHandler"
subEventHandler lcycle Unsubscribed = onUserUnsubscribed lcycle
subEventHandler lcycle (Dropped r) = onError lcycle r

--------------------------------------------------------------------------------
-- | Submit a new event to the subscription state machine. Internally,
--   that event should be stored into the subscription buffer.
eventArrivedSM :: ResolvedEvent -> SubStateMachine t -> SubStateMachine t
eventArrivedSM e (SubStateMachine k) = k (Arrived e)

--------------------------------------------------------------------------------
-- | Reads the next available event. Returns 'Nothing' it there is any. When
--   returning an event, it will be removed from the subscription buffer.
readNextSM :: SubStateMachine t -> Maybe (ResolvedEvent, SubStateMachine t)
readNextSM (SubStateMachine k) = k ReadNext

--------------------------------------------------------------------------------
-- | Submits a list of events read from a stream. It's only used by a 'Catchup'
--   subscription.
batchReadSM :: [ResolvedEvent]
            -> Bool -- ^ If it reaches the end of the stream.
            -> Checkpoint
            -> SubStateMachine Catchup
            -> SubStateMachine Catchup
batchReadSM es eos nxt (SubStateMachine k) = k (BatchRead es eos nxt)

--------------------------------------------------------------------------------
-- | Indicates if the subscription caught up the end of the stream, meaning the
--   subscription is actually live. Only used by 'Catchup' subscription.
hasCaughtUpSM :: SubStateMachine Catchup -> Bool
hasCaughtUpSM (SubStateMachine k) = k CaughtUp

--------------------------------------------------------------------------------
-- | Last event number read by the user.
lastEventNumSM :: SubStateMachine Catchup -> (Int32, Maybe Position)
lastEventNumSM (SubStateMachine k) = k LastEventNum

--------------------------------------------------------------------------------
-- | Main 'Regular' subscription state machine.
regularSubscription :: SubStateMachine Regular
regularSubscription = baseSubStateMachine

--------------------------------------------------------------------------------
-- | Main 'Persistent' subscription state machine.
persistentSubscription :: SubStateMachine Persistent
persistentSubscription = baseSubStateMachine

--------------------------------------------------------------------------------
-- | Depending either if the subscription concerns a regular stream or $all,
--  indicates if an event number (or 'Position') is lesser that the current the
--  given 'CheckPoint'.
beforeChk :: Checkpoint -> ResolvedEvent -> Bool
beforeChk (CheckpointNumber num) re =
    recordedEventNumber (resolvedEventOriginal re) < num
beforeChk (CheckpointPosition pos) re =
    maybe False (< pos) $ resolvedEventPosition re

--------------------------------------------------------------------------------
-- | This data structure is only used by catchup subscription state machine.
data CatchupSMState =
    CatchupSMState { csmReadSeq :: !(Seq ResolvedEvent)
                   -- ^ This sequence is used to pack events coming from reading
                   --   a stream forward.
                   , csmLiveSeq :: !(Seq ResolvedEvent)
                   -- ^ This sequence is used to pack events coming from live
                   --   subscription.
                   , csmLastNum :: !(Maybe Int32)
                   -- ^ Tracks the last event read.
                   , csmLastPos :: !(Maybe Position)
                   }

--------------------------------------------------------------------------------
initialCatchupSMState :: CatchupSMState
initialCatchupSMState = CatchupSMState empty empty Nothing Nothing

--------------------------------------------------------------------------------
insertReadEvents :: [ResolvedEvent]
                 -> Checkpoint
                 -> CatchupSMState
                 -> CatchupSMState
insertReadEvents es chp s = result
  where
    temp = s { csmReadSeq = foldl' snoc (csmReadSeq s) es
             , csmLiveSeq = dropWhileL (beforeChk chp) (csmLiveSeq s)
             }

    result =
      case chp of
        CheckpointNumber n -> temp { csmLastNum = Just n }
        CheckpointPosition p -> temp { csmLastPos = Just p }

--------------------------------------------------------------------------------
insertLiveEvent :: ResolvedEvent -> CatchupSMState -> CatchupSMState
insertLiveEvent e s = s { csmLiveSeq = csmLiveSeq s `snoc` e }

--------------------------------------------------------------------------------
readNextFromBatchSeq :: CatchupSMState -> Maybe (ResolvedEvent, CatchupSMState)
readNextFromBatchSeq s =
    case viewl $ csmReadSeq s of
        EmptyL -> Nothing
        e :< rest ->
            let newLast = recordedEventNumber $ resolvedEventOriginal e
                nxtS = s { csmReadSeq = rest
                         , csmLastNum = Just newLast
                         , csmLastPos = resolvedEventPosition e
                         } in
            Just (e, nxtS)

--------------------------------------------------------------------------------
readNextFromLiveSeq :: CatchupSMState -> Maybe (ResolvedEvent, CatchupSMState)
readNextFromLiveSeq s =
    case viewl $ csmLiveSeq s of
        EmptyL -> Nothing
        e :< rest ->
            let newLast = recordedEventNumber $ resolvedEventOriginal e
                nxtS = s { csmLiveSeq = rest
                         , csmLastNum = Just newLast
                         , csmLastPos = resolvedEventPosition e
                         } in
            Just (e, nxtS)

--------------------------------------------------------------------------------
lastEventNumber :: CatchupSMState -> (Int32, Maybe Position)
lastEventNumber s = (fromMaybe 0 $ csmLastNum s, csmLastPos s)

--------------------------------------------------------------------------------
isBatchReqEmpty :: CatchupSMState -> Bool
isBatchReqEmpty s =
    case viewl $ csmReadSeq s of
        EmptyL -> True
        _ -> False

--------------------------------------------------------------------------------
-- | That subscription state machine accumulates events coming from batch read
--   and any real time change made on a stream. That state machine will not
--   served any recent change made on the stream until it reaches the end of the
--   stream. On every batch read, it makes sure events contained in that batch
--   are deleted from the subscription buffer in order to avoid duplicates. That
--   implemention has been chosen to avoid potential message lost between the
--   moment with reach the end of the stream and the delay required by asking
--   for a subscription.
catchupSubscription :: SubStateMachine Catchup
catchupSubscription = SubStateMachine $ catchingUp initialCatchupSMState
  where
    catchingUp :: forall a. CatchupSMState -> Input Catchup a -> a
    catchingUp s (Arrived e) =
        SubStateMachine $ catchingUp $ insertLiveEvent e s
    catchingUp s ReadNext =
        let _F (e, sm) = (e, SubStateMachine $ catchingUp sm) in
        fmap _F $ readNextFromBatchSeq s
    catchingUp s (BatchRead es eos chk) =
        let nxtS = insertReadEvents es chk s in
        SubStateMachine $
            if eos
            then caughtUp nxtS
            else catchingUp nxtS
    catchingUp _ CaughtUp = False
    catchingUp s LastEventNum = lastEventNumber s

    caughtUp :: forall a. CatchupSMState -> Input Catchup a -> a
    caughtUp s (Arrived e) = SubStateMachine $ caughtUp $ insertLiveEvent e s
    caughtUp s ReadNext =
        case readNextFromBatchSeq s of
            Nothing -> live s ReadNext
            Just (e, nxtS) ->
                if isBatchReqEmpty nxtS
                then Just (e, SubStateMachine $ live s)
                else Just (e, SubStateMachine $ caughtUp nxtS)
    caughtUp s BatchRead{} = SubStateMachine $ caughtUp s
    caughtUp _ CaughtUp = False
    caughtUp s LastEventNum = lastEventNumber s

    live :: forall a. CatchupSMState -> Input Catchup a -> a
    live s (Arrived e) = SubStateMachine $ live $ insertLiveEvent e s
    live s ReadNext =
        let _F (e, sm) = (e, SubStateMachine $ live sm) in
        fmap _F $ readNextFromLiveSeq s
    live s BatchRead{} = SubStateMachine $ live s
    live _ CaughtUp = True
    live s LastEventNum = lastEventNumber s

--------------------------------------------------------------------------------
-- | Base subscription used for 'Regular' or 'Persistent' subscription.
baseSubStateMachine :: forall t. SubStateMachine t
baseSubStateMachine = SubStateMachine $ go empty
  where
    go :: forall a. Seq ResolvedEvent -> Input t a -> a
    go s (Arrived e) = SubStateMachine $ go (s `snoc` e)
    go s ReadNext =
        case viewl s of
            EmptyL    -> Nothing
            e :< rest -> Just (e, SubStateMachine $ go rest)
    go _ _ = error "impossible: base subscription"

--------------------------------------------------------------------------------
-- Subscription API
--------------------------------------------------------------------------------
-- | Represents a subscription id.
newtype SubscriptionId = SubId UUID deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- | Gets the ID of the subscription.
getSubId :: Subscription a -> IO SubscriptionId
getSubId Subscription{..} = atomically $ do
    run <- readTMVar subRun
    return $ SubId $ runningUUID run

--------------------------------------------------------------------------------
-- | Gets the subscription stream name.
getSubStream :: Subscription a -> Text
getSubStream = subStream

--------------------------------------------------------------------------------
-- | Asynchronously unsubscribe from the the stream.
unsubscribe :: Subscription a -> IO ()
unsubscribe Subscription{..} = do
    run <- atomically $ readTMVar subRun
    subPushUnsub subEnv run

--------------------------------------------------------------------------------
-- | If the subscription is on the $all stream.
isSubscribedToAll :: Subscription a -> Bool
isSubscribedToAll = (== "") . getSubStream

--------------------------------------------------------------------------------
-- | The last commit position seen on the subscription (if this a subscription
--   to $all stream).
getSubLastCommitPos :: Subscription a -> IO Int64
getSubLastCommitPos Subscription{..} = atomically $ do
    run <- readTMVar subRun
    return $ runningLastCommitPosition run

--------------------------------------------------------------------------------
-- | The last event number seen on the subscription (if this is a subscription
--   to a single stream).
getSubLastEventNumber :: Subscription a -> IO (Maybe Int32)
getSubLastEventNumber Subscription{..} = atomically $ do
    run <- readTMVar subRun
    return $ runningLastEventNumber run

--------------------------------------------------------------------------------
-- | Asks for the next incoming event like 'nextEventMaybe' while still being
--   in the the 'STM'.
nextEventMaybeSTM :: Subscription a -> STM (Maybe ResolvedEvent)
nextEventMaybeSTM Subscription{..} = do
    st <- readState subLifeCycle
    case st of
        SubException e -> throwSTM e
        SubDropped r -> throwSTM $ SubscriptionClosed r
        SubOnline sub -> do
            case readNextSM sub of
                Just (e, nxt) ->
                    Just e <$ writeState subLifeCycle (SubOnline nxt)
                _ -> return Nothing
        SubUserUnsubscribed -> throwSTM SubscriptionUnsubscribedByUser

--------------------------------------------------------------------------------
-- | Awaits for the next event.
nextEvent :: Subscription a -> IO ResolvedEvent
nextEvent sub = atomically $ do
    m <- nextEventMaybeSTM sub
    case m of
        Nothing -> retrySTM
        Just e  -> return e

--------------------------------------------------------------------------------
-- | Non blocking version of 'nextEvent'.
nextEventMaybe :: Subscription a -> IO (Maybe ResolvedEvent)
nextEventMaybe = atomically . nextEventMaybeSTM

--------------------------------------------------------------------------------
-- | Waits until the `Subscription` has been confirmed.
waitConfirmation :: Subscription a -> IO ()
waitConfirmation s = atomically $ do
    _ <- readTMVar $ subRun s
    return ()

--------------------------------------------------------------------------------
-- | Determines whether or not any link events encontered in the stream will be
--   resolved.
getSubResolveLinkTos :: Subscription Regular -> Bool
getSubResolveLinkTos = _subTos . subType

--------------------------------------------------------------------------------
-- | Non blocking version of `waitTillCatchup`.
hasCaughtUp :: Subscription Catchup -> IO Bool
hasCaughtUp sub = atomically $ hasCaughtUpSTM sub

--------------------------------------------------------------------------------
-- | Waits until 'CatchupSubscription' subscription catch-up its stream.
waitTillCatchup :: Subscription Catchup -> IO ()
waitTillCatchup sub = atomically $
    unlessM (hasCaughtUpSTM sub) retrySTM

--------------------------------------------------------------------------------
-- | Like 'hasCaughtUp' but lives in 'STM' monad.
hasCaughtUpSTM :: Subscription Catchup -> STM Bool
hasCaughtUpSTM Subscription{..} = do
    res <- readState subLifeCycle
    case res of
        SubOnline sm -> return $ hasCaughtUpSM sm
        SubException e -> throwSTM e
        SubDropped r -> throwSTM $ SubscriptionClosed r
        SubUserUnsubscribed -> throwSTM SubscriptionUnsubscribedByUser

--------------------------------------------------------------------------------
-- | Like 'unsubscribeConfirmed' but lives in 'STM' monad.
unsubscribeConfirmedSTM :: Subscription a -> STM Bool
unsubscribeConfirmedSTM Subscription{..} = do
    res <- readState subLifeCycle
    case res of
        SubOnline _ -> return False
        SubException e -> throwSTM e
        SubDropped r -> throwSTM $ SubscriptionClosed r
        SubUserUnsubscribed -> return True

--------------------------------------------------------------------------------
-- | Non blocking version of `waitUnsubscribeConfirmed`.
unsubscribeConfirmed :: Subscription a -> IO Bool
unsubscribeConfirmed = atomically . unsubscribeConfirmedSTM

--------------------------------------------------------------------------------
-- | Wait until unsubscription has been confirmed by the server.
waitUnsubscribeConfirmed :: Subscription a -> IO ()
waitUnsubscribeConfirmed sub = atomically $
    unlessM (unsubscribeConfirmedSTM sub) retrySTM

--------------------------------------------------------------------------------
-- | Acknowledges those event ids have been successfully processed.
notifyEventsProcessed :: Subscription Persistent -> [UUID] -> IO ()
notifyEventsProcessed Subscription{..} evts = do
    run <- atomically $ readTMVar subRun
    subAckCmd subEnv AckCmd run evts

--------------------------------------------------------------------------------
-- | Acknowledges that 'ResolvedEvent' has been successfully processed.
acknowledge :: Subscription Persistent -> ResolvedEvent -> IO ()
acknowledge sub e = notifyEventsProcessed sub [resolvedEventOriginalId e]

--------------------------------------------------------------------------------
-- | Acknowledges those 'ResolvedEvent's have been successfully processed.
acknowledgeEvents :: Subscription Persistent -> [ResolvedEvent] -> IO ()
acknowledgeEvents sub = notifyEventsProcessed sub . fmap resolvedEventOriginalId

--------------------------------------------------------------------------------
-- | Mark a message that has failed processing. The server will take action
--   based upon the action parameter.
failed :: Subscription Persistent
       -> ResolvedEvent
       -> NakAction
       -> Maybe Text
       -> IO ()
failed sub e a r = notifyEventsFailed sub a r [resolvedEventOriginalId e]

--------------------------------------------------------------------------------
-- | Mark messages that have failed processing. The server will take action
--   based upon the action parameter.
eventsFailed :: Subscription Persistent
             -> [ResolvedEvent]
             -> NakAction
             -> Maybe Text
             -> IO ()
eventsFailed sub evts a r =
    notifyEventsFailed sub a r $ fmap resolvedEventOriginalId evts

--------------------------------------------------------------------------------
-- | Acknowledges those event ids have failed to be processed successfully.
notifyEventsFailed :: Subscription Persistent
                   -> NakAction
                   -> Maybe Text
                   -> [UUID]
                   -> IO ()
notifyEventsFailed Subscription{..} act res evts = do
    run <- atomically $ readTMVar subRun
    subAckCmd subEnv (NakCmd act res) run evts
