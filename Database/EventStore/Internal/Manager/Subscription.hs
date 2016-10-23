{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Manager.Subscription
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
module Database.EventStore.Internal.Manager.Subscription
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
    ) where

--------------------------------------------------------------------------------
import Data.Int

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Sequence (ViewL(..), viewl, dropWhileL)
import Data.UUID

--------------------------------------------------------------------------------
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
    ReadNext :: Input t (Maybe ResolvedEvent, SubStateMachine t)
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
           }

--------------------------------------------------------------------------------
data SubState t
    = SubOnline (SubStateMachine t)
    | SubDropped SubDropReason
    | forall e. Exception e => SubException e

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
data SubscriptionClosed =
    SubscriptionClosed SubDropReason
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception SubscriptionClosed

--------------------------------------------------------------------------------
catchupSub :: SubEnv -> CatchupParams -> IO (Subscription Catchup)
catchupSub env params = do
    mvarRun <- newEmptyTMVarIO
    mvarState <- newEmptyTMVarIO

    let lcycle =
            SubLifeCycle
            { onConfirm = atomically . putTMVar mvarRun
            , readState = readTMVar mvarState
            , writeState = \s -> () <$ swapTMVar mvarState s
            , onError = \e -> atomically $ do
                  s <- takeTMVar mvarState
                  case s of
                      SubOnline{} -> putTMVar mvarState $ SubDropped e
                      _ -> putTMVar mvarState s
            }

        op = createCatchupOperation env params

    subPushOp env (catchupOpEventHandler mvarState) op
    let streamId = catchupStreamName $ catchupState params
        pushCmd = PushRegular streamId (catchupResLnkTos params)
    subPushConnect env (subEventHandler lcycle) pushCmd
    return $ Subscription streamId lcycle env mvarRun Catchup

--------------------------------------------------------------------------------
regularSub :: SubEnv -> Text -> Bool -> IO (Subscription Regular)
regularSub env streamId resLnkTos = do
    mvarRun <- newEmptyTMVarIO
    varState <- newTVarIO $ SubOnline regularSubscription
    let lcycle =
            SubLifeCycle
            { onConfirm = atomically . putTMVar mvarRun
            , readState = readTVar varState
            , writeState = writeTVar varState
            , onError = \r -> atomically $ do
                  s <- readTVar varState
                  case s of
                      SubOnline{} -> writeTVar varState $ SubDropped r
                      _ -> return ()
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
            { onConfirm = atomically . putTMVar mvarRun
            , readState = readTVar varState
            , writeState = writeTVar varState
            , onError = \r -> atomically $ do
                  s <- readTVar varState
                  case s of
                      SubOnline{} -> writeTVar varState $ SubDropped r
                      _ -> return ()
            }

        pushCmd = PushPersistent grp stream bufSize

    subPushConnect env (subEventHandler lcycle) pushCmd
    return $ Subscription stream lcycle env mvarRun (Persistent grp)

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
        SubDropped _ -> error "Impossible: subEventHandler"
subEventHandler lcycle (Dropped r) = onError lcycle r

--------------------------------------------------------------------------------
-- | Submit a new event to the subscription state machine. Internally,
--   that event should be stored into the subscription buffer.
eventArrivedSM :: ResolvedEvent -> SubStateMachine t -> SubStateMachine t
eventArrivedSM e (SubStateMachine k) = k (Arrived e)

--------------------------------------------------------------------------------
-- | Reads the next available event. Returns 'Nothing' it there is any. When
--   returning an event, it will be removed from the subscription buffer.
readNextSM :: SubStateMachine t -> (Maybe ResolvedEvent, SubStateMachine t)
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
-- | That subscription state machine accumulates events coming from batch read
--   and any real time change made on a stream. That state machine will not
--   served any recent change made on the stream until it reaches the end of the
--   stream. On every batch read, it makes sure events contained in that batch
--   are deleted from the subscription buffer in order to avoid duplicates. That
--   implemention has been chosen to avoid potential message lost between the
--   moment with reach the end of the stream and the delay required by asking
--   for a subscription.
catchupSubscription :: SubStateMachine Catchup
catchupSubscription = SubStateMachine $ catchingUp empty empty
  where
    catchingUp :: forall a. Seq ResolvedEvent
               -> Seq ResolvedEvent
               -> Input Catchup a
               -> a
    catchingUp b s (Arrived e) =
        SubStateMachine $ catchingUp b (s `snoc` e)
    catchingUp b s ReadNext =
        case viewl b of
            EmptyL    -> (Nothing, SubStateMachine $ catchingUp b s)
            e :< rest -> (Just e, SubStateMachine $ catchingUp rest s)
    catchingUp b s (BatchRead es eos nxt_pt) =
        let nxt_b = foldl' snoc b es
            nxt_s = dropWhileL (beforeChk nxt_pt) s
            nxt   = if eos
                    then SubStateMachine $ caughtUp nxt_b nxt_s
                    else SubStateMachine $ catchingUp nxt_b nxt_s in
        nxt
    catchingUp _ _ CaughtUp = False

    caughtUp :: forall a. Seq ResolvedEvent
             -> Seq ResolvedEvent
             -> Input Catchup a
             -> a
    caughtUp  b s (Arrived e) = SubStateMachine $ caughtUp b (s `snoc` e)
    caughtUp b s  ReadNext =
        case viewl b of
            EmptyL -> live s ReadNext
            e :< rest ->
                case viewl rest of
                    EmptyL -> (Just e, SubStateMachine $ live s)
                    _      -> (Just e, SubStateMachine $ caughtUp rest s)
    caughtUp b s (BatchRead _ _ _) = SubStateMachine $ caughtUp b s
    caughtUp _ _ CaughtUp = False

    live :: forall a. Seq ResolvedEvent -> Input Catchup a -> a
    live s (Arrived e) = SubStateMachine $ live (s `snoc` e)
    live s ReadNext =
        case viewl s of
            EmptyL    -> (Nothing, SubStateMachine $ live s)
            e :< rest -> (Just e, SubStateMachine $ live rest)
    live s (BatchRead _ _ _) = SubStateMachine $ live s
    live _ CaughtUp = True

--------------------------------------------------------------------------------
-- | Base subscription used for 'Regular' or 'Persistent' subscription.
baseSubStateMachine :: forall t. SubStateMachine t
baseSubStateMachine = SubStateMachine $ go empty
  where
    go :: forall a. Seq ResolvedEvent -> Input t a -> a
    go s (Arrived e) = SubStateMachine $ go (s `snoc` e)
    go s ReadNext =
        case viewl s of
            EmptyL    -> (Nothing, SubStateMachine $ go s)
            e :< rest -> (Just e, SubStateMachine $ go rest)
    go _ BatchRead{} = error "impossible: base subscription"
    go _ CaughtUp    = error "impossible: base subscription"

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
            let (res, nxt) = readNextSM sub
            if isJust res
                then res <$ writeState subLifeCycle (SubOnline nxt)
                else return res

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
