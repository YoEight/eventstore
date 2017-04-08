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
import ClassyPrelude

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Callback
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Exec
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.Operation.Catchup
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Subscription.Api
import Database.EventStore.Internal.Subscription.Types
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Phase
  = CatchingUp
  | Pending
  | Running SubDetails
  | Closed (Maybe SubDropReason)

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
      Closed r        -> throwSTM (SubscriptionClosed r)
      _               -> retrySTM

  subscriptionStream = _catchupStream

  unsubscribe s = subUnsubscribe (_catchupExec s) s

--------------------------------------------------------------------------------
newCatchupSubscription :: Exec
                       -> Bool
                       -> Maybe Int32
                       -> CatchupState
                       -> IO CatchupSubscription
newCatchupSubscription exec tos batch state = do
  phaseVar <- newTVarIO CatchingUp
  queue    <- newTQueueIO
  chkVar   <- newTVarIO $
    case state of
      RegularCatchup _ evtNum -> Chk evtNum positionStart
      AllCatchup c p          -> Chk 0 (Position c p)


  let stream =
        case state of
           RegularCatchup s _ -> StreamName s
           _                  -> AllStream

      name = streamNameRaw stream

      readNext = do
        p       <- readTVar phaseVar
        isEmpty <- isEmptyTQueue queue
        if isEmpty
          then
            case p of
              Closed r -> throwSTM (SubscriptionClosed r)
              _        -> return Nothing
          else do
            e <- readTQueue queue
            let evtNum = recordedEventNumber . resolvedEventOriginal $ e
                evtPos = fromMaybe positionStart (resolvedEventPosition e)

            writeTVar chkVar (Chk evtNum evtPos)
            return (Just e)

      sub = CatchupSubscription exec stream phaseVar readNext

      callback (Confirmed details) = atomically $
        writeTVar phaseVar (Running details)
      callback (Dropped r) = atomically $
        writeTVar phaseVar (Closed $ Just r)
      callback (Submit e) = atomically $ do
        p <- readTVar phaseVar
        case p of
          Running{} -> writeTQueue queue e
          _         -> return ()
      callback Reconnect = do
        Chk num (Position cpos ppos) <- atomically $ do
          writeTVar phaseVar CatchingUp
          readTVar chkVar
        let newState =
              case state of
                RegularCatchup{} -> RegularCatchup name num
                AllCatchup{}     -> AllCatchup cpos ppos

            newOp = catchup (execSettings exec) newState  tos batch

        cb <- newCallback opCallback
        publish exec (SubmitOperation cb newOp)

      opCallback (Left (_ :: SomeException)) = do
        atomically $ writeTVar phaseVar Pending
        cb <- newCallbackSimple callback
        publish exec (ConnectStream cb name tos)
      opCallback (Right res) = do
        atomically $ do
          writeTVar chkVar (fromCheckPoint $ catchupCheckpoint res)
          when (catchupEndOfStream res) $
            writeTVar phaseVar Pending

          traverse_ (writeTQueue queue) (catchupReadEvents res)

        when (catchupEndOfStream res) $ do
          cb <- newCallbackSimple callback
          publish exec (ConnectStream cb name tos)

      op = catchup (execSettings exec) state tos batch

  cb <- newCallback opCallback
  publish exec (SubmitOperation cb op)

  return sub

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
    Pending    -> return True
    Running{}  -> return True
    Closed r   -> throwSTM (SubscriptionClosed r)
