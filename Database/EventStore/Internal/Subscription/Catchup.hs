{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Callback
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Exec
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Catchup
import Database.EventStore.Internal.Operation.Volatile
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
data Chk = Chk !Int32 !Position

--------------------------------------------------------------------------------
fromCheckPoint :: Checkpoint -> Chk
fromCheckPoint (CheckpointNumber num)   = Chk num positionStart
fromCheckPoint (CheckpointPosition pos) = Chk 0 pos

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
data CatchupSubscription =
  CatchupSubscription { _catchupExec   :: Exec
                      , _catchupStream :: StreamName
                      , _catchupPhase  :: TVar Phase
                      , _catchupNext   :: STM (Maybe ResolvedEvent)
                      }

--------------------------------------------------------------------------------
instance Subscription CatchupSubscription where
  nextEventMaybeSTM = _catchupNext

  getSubscriptionDetailsSTM s = do
    p <- readTVar (_catchupPhase s)
    case p of
      Running details -> return details
      Closed r        -> throwClosed r
      _               -> retrySTM

  subscriptionStream = _catchupStream

  unsubscribe s = subUnsubscribe (_catchupExec s) s

--------------------------------------------------------------------------------
streamName :: CatchupState -> StreamName
streamName (RegularCatchup stream _) = StreamName stream
streamName _                         = "$all"

--------------------------------------------------------------------------------
streamText :: StreamName -> Text
streamText (StreamName s) = s
streamText _              = ""

--------------------------------------------------------------------------------
newCatchupSubscription :: Exec
                       -> Bool
                       -> Maybe Int32
                       -> CatchupState
                       -> IO CatchupSubscription
newCatchupSubscription exec tos batch state = do
  phaseVar <- newTVarIO CatchingUp
  queue    <- newTQueueIO

  let stream = streamName state
      sub = CatchupSubscription exec (streamName state) phaseVar $ do
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
                let op = volatile (streamText stream) tos
                publish exec (SubmitOperation cb op)
              _ -> atomically $ writeTVar phaseVar (Closed $ Left e)
          _ -> atomically $ writeTVar phaseVar (Closed $ Left e)
      callback _ (Right action) = atomically $
        case action of
          Confirmed details -> writeTVar phaseVar (Running details)
          Dropped r         -> writeTVar phaseVar (Closed $ Right r)
          Submit e          -> writeTQueue queue e

  cb <- mfix $ \self -> newCallback (callback self)
  let op = catchup (execSettings exec) state tos batch
  publish exec (SubmitOperation cb op)
  return sub

--------------------------------------------------------------------------------
throwClosed :: Either SomeException SubDropReason -> STM a
throwClosed (Left e)  = throwSTM e
throwClosed (Right r) = throwSTM (SubscriptionClosed $ Just r)

--------------------------------------------------------------------------------
-- | Non blocking version of `waitTillCatchup`.
hasCaughtUp :: CatchupSubscription -> IO Bool
hasCaughtUp sub = atomically $ hasCaughtUpSTM sub

--------------------------------------------------------------------------------
-- | Waits until 'CatchupSubscription' subscription catch-up its stream.
waitTillCatchup :: CatchupSubscription -> IO ()
waitTillCatchup sub = atomically $ unlessM (hasCaughtUpSTM sub) retrySTM

--------------------------------------------------------------------------------
-- | Like 'hasCaughtUp' but lives in 'STM' monad.
hasCaughtUpSTM :: CatchupSubscription -> STM Bool
hasCaughtUpSTM CatchupSubscription{..} = do
  p <- readTVar _catchupPhase
  case p of
    CatchingUp -> return False
    Running{}  -> return True
    Closed tpe ->
      case tpe of
        Left e  -> throwSTM e
        Right r -> throwSTM (SubscriptionClosed $ Just r)
