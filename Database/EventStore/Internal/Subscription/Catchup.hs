{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wno-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Subscription.Catchup
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Subscription.Catchup where

--------------------------------------------------------------------------------
import Control.Monad.Fix
import Safe (fromJustNote)

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Callback
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Control
import Database.EventStore.Internal.Exec
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Catchup
import Database.EventStore.Internal.Operation.Volatile
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Subscription.Api
import Database.EventStore.Internal.Subscription.Types
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Phase
  = CatchingUp
  | Running SubDetails
  | Closed (Either SomeException SubDropReason)

--------------------------------------------------------------------------------
receivedAlready :: StreamId t -> t -> ResolvedEvent -> Bool
receivedAlready StreamName{} old e =
    EventNumber (resolvedEventOriginalEventNumber e) < old
receivedAlready All old e =
    let pos =
            fromJustNote
                "Position is always defined when reading events from $all stream"
                $ resolvedEventPosition e in
    pos < old

--------------------------------------------------------------------------------
nextTarget :: StreamId t -> ResolvedEvent -> t
nextTarget StreamName{} e =
    EventNumber (resolvedEventOriginalEventNumber e)
nextTarget All e =
    fromJustNote
        "Position is always defined when reading events from $all stream"
        $ resolvedEventPosition e

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
data CatchupSubscription t =
  CatchupSubscription { _catchupExec   :: Exec
                      , _catchupStream :: StreamId t
                      , _catchupPhase  :: TVar Phase
                      , _catchupTrack  :: TVar t
                      , _catchupNext   :: STM (Maybe ResolvedEvent)
                      }

--------------------------------------------------------------------------------
instance Subscription (CatchupSubscription t) where
  nextEventMaybeSTM = _catchupNext

  getSubscriptionDetailsSTM s = do
    p <- readTVar (_catchupPhase s)
    case p of
      Running details -> return details
      Closed r        -> throwClosed r
      _               -> retrySTM

  unsubscribe s = subUnsubscribe (_catchupExec s) s

--------------------------------------------------------------------------------
instance SubscriptionStream (CatchupSubscription t) t where
    subscriptionStream = _catchupStream

--------------------------------------------------------------------------------
newCatchupSubscription :: Exec
                       -> Bool
                       -> Maybe Int32
                       -> Maybe Credentials
                       -> StreamId t
                       -> t
                       -> IO (CatchupSubscription t)
newCatchupSubscription exec tos batch cred streamId seed = do
  phaseVar <- newTVarIO CatchingUp
  queue    <- newTQueueIO
  track    <- newTVarIO seed

  let sub = CatchupSubscription exec streamId phaseVar track $ do
        p       <- readTVar phaseVar
        isEmpty <- isEmptyTQueue queue
        if isEmpty
          then
            case p of
              Closed r -> throwClosed r
              _        -> return Nothing
          else Just <$> readTQueue queue

      callback cb (Left e) =
        case fromException e of
          Just opE ->
            case opE of
              StreamNotFound{} -> do
                let op = volatile streamId tos cred
                publishWith exec (SubmitOperation cb op)
              _ -> atomically $ writeTVar phaseVar (Closed $ Left e)
          _ -> atomically $ writeTVar phaseVar (Closed $ Left e)
      callback _ (Right action) =
        case action of
          Confirmed details -> atomically $ writeTVar phaseVar (Running details)
          Dropped r ->
            atomically $ writeTVar phaseVar (Closed $ Right r)
          Submit e -> atomically $ do
            tracker <- readTVar track
            unless (receivedAlready streamId tracker e) $ do
              writeTVar track (nextTarget streamId e)
              writeTQueue queue e
          ConnectionReset -> do
            chk <- readTVarIO track
            let newOp = catchup (execSettings exec) streamId chk tos batch cred

            newCb <- mfix $ \self -> newCallback (callback self)
            publishWith exec (SubmitOperation newCb newOp)

  cb <- mfix $ \self -> newCallback (callback self)
  let op = catchup (execSettings exec) streamId seed tos batch cred
  publishWith exec (SubmitOperation cb op)
  return sub

--------------------------------------------------------------------------------
throwClosed :: Either SomeException SubDropReason -> STM a
throwClosed (Left e)  = throwSTM e
throwClosed (Right r) = throwSTM (SubscriptionClosed $ Just r)

--------------------------------------------------------------------------------
-- | Non blocking version of `waitTillCatchup`.
hasCaughtUp :: CatchupSubscription t -> IO Bool
hasCaughtUp sub = atomically $ hasCaughtUpSTM sub

--------------------------------------------------------------------------------
-- | Waits until 'CatchupSubscription' subscription catch-up its stream.
waitTillCatchup :: CatchupSubscription t -> IO ()
waitTillCatchup sub = atomically $ unlessM (hasCaughtUpSTM sub) retrySTM

--------------------------------------------------------------------------------
-- | Like 'hasCaughtUp' but lives in 'STM' monad.
hasCaughtUpSTM :: CatchupSubscription t -> STM Bool
hasCaughtUpSTM CatchupSubscription{..} = do
  p <- readTVar _catchupPhase
  case p of
    CatchingUp -> return False
    Running{}  -> return True
    Closed tpe ->
      case tpe of
        Left e  -> throwSTM e
        Right r -> throwSTM (SubscriptionClosed $ Just r)
