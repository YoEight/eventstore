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
data CatchupTrack
  = CatchupRegular !(Maybe Int64)
  | CatchupAll !(Maybe Position)

--------------------------------------------------------------------------------
catchupTrack :: CatchupState -> CatchupTrack
catchupTrack RegularCatchup{} = CatchupRegular Nothing
catchupTrack AllCatchup{}     = CatchupAll Nothing

--------------------------------------------------------------------------------
receivedAlready :: CatchupTrack -> ResolvedEvent -> Bool
receivedAlready (CatchupRegular old) e =
  maybe False (resolvedEventOriginalEventNumber e <=) old
receivedAlready (CatchupAll old) e =
  fromMaybe False ((<=) <$> resolvedEventPosition e <*> old)

--------------------------------------------------------------------------------
updateTrack :: ResolvedEvent -> CatchupTrack -> CatchupTrack
updateTrack e (CatchupRegular _) =
  CatchupRegular (Just $ resolvedEventOriginalEventNumber e)
updateTrack e (CatchupAll _) =
  CatchupAll (resolvedEventPosition e)

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
                      , _catchupTrack  :: TVar CatchupTrack
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
                       -> Maybe Credentials
                       -> CatchupState
                       -> IO CatchupSubscription
newCatchupSubscription exec tos batch cred state = do
  phaseVar <- newTVarIO CatchingUp
  queue    <- newTQueueIO
  track    <- newTVarIO $ catchupTrack state

  let stream = streamName state
      sub = CatchupSubscription exec stream phaseVar track $ do
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
                let op = volatile (streamText stream) tos cred
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
            unless (receivedAlready tracker e) $ do
              writeTVar track (updateTrack e tracker)
              writeTQueue queue e
          ConnectionReset -> do
            tpe <- readTVarIO track
            let newState =
                  case tpe of
                    CatchupRegular old ->
                      case old of
                        Just n -> RegularCatchup (streamText stream) n
                        _      -> state
                    CatchupAll old ->
                      case old of
                        Just p -> AllCatchup p
                        _      -> state

                newOp = catchup (execSettings exec) newState tos batch cred

            newCb <- mfix $ \self -> newCallback (callback self)
            publishWith exec (SubmitOperation newCb newOp)

  cb <- mfix $ \self -> newCallback (callback self)
  let op = catchup (execSettings exec) state tos batch cred
  publishWith exec (SubmitOperation cb op)
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
