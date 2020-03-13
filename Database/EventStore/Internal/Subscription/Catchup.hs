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
import Safe (fromJustNote)

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Control
import Database.EventStore.Internal.Exec
import Database.EventStore.Internal.Operation.Catchup
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Subscription.Api
import Database.EventStore.Internal.Subscription.Types
import Database.EventStore.Internal.Subscription.Packages
import Database.EventStore.Internal.Types

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
  CatchupSubscription
    { _catchupExec :: Exec
    , _catchupStream :: StreamId t
    , _catchupSub :: TVar (Maybe UUID)
    , _catchupChan :: Chan SubAction
    }

--------------------------------------------------------------------------------
instance Subscription (CatchupSubscription s) where
  nextSubEvent s = readChan (_catchupChan s)

  unsubscribe s
    = do subId <- atomically $
           do idMay <- readTVar (_catchupSub s)
              case idMay of
                Nothing -> retrySTM
                Just sid -> pure sid

         let pkg = createUnsubscribePackage subId
         publishWith (_catchupExec s) (SendPackage pkg)

--------------------------------------------------------------------------------
instance SubscriptionStream (CatchupSubscription t) t where
    subscriptionStream = _catchupStream

--------------------------------------------------------------------------------
newCatchupSubscription
  :: Exec
  -> Bool
  -> Maybe Int32
  -> Maybe Credentials
  -> StreamId t
  -> t
  -> IO (CatchupSubscription t)
newCatchupSubscription exec tos batch cred streamId seed
  = do (var, chan) <- catchup (execSettings exec) exec streamId seed tos batch cred
       let sub =
             CatchupSubscription
             { _catchupExec = exec
             , _catchupStream = streamId
             , _catchupSub = var
             , _catchupChan = chan
             }

       pure sub
