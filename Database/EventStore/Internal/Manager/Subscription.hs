{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
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
    ( module Database.EventStore.Internal.Manager.Subscription.Driver
    , Regular(..)
    , Persistent(..)
    , Catchup(..)
    , Subscription
    , Running(..)
    , Checkpoint(..)
    , regularSubscription
    , catchupSubscription
    , persistentSubscription
    , eventArrived
    , readNext
    , batchRead
    , hasCaughtUp
    , runningUUID
    , runningLastEventNumber
    , runningLastCommitPosition
    ) where

--------------------------------------------------------------------------------
import Data.Int

--------------------------------------------------------------------------------
import Data.Sequence
import Data.Text (Text)

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Manager.Subscription.Driver
import Database.EventStore.Internal.Manager.Subscription.Model
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
    Arrived :: ResolvedEvent -> Input t (Subscription t)
    -- The user asks for the next event coming from the server.
    ReadNext :: Input t (Maybe ResolvedEvent, Subscription t)
    -- A batch read has been made. It's only use for 'Catchup' subscription
    -- type. It gives the list of read events and indicates if it reaches the
    -- end of the stream along with the next checkpoint to point at.
    BatchRead :: [ResolvedEvent]
              -> Bool
              -> Checkpoint
              -> Input Catchup (Subscription Catchup)
    -- Used only for 'Catchup' subscription type. Asks if the subscription
    -- read every events up to the checkpoint given by the user.
    CaughtUp :: Input Catchup Bool

--------------------------------------------------------------------------------
-- | Main subscription state machine.
newtype Subscription t = Subscription (forall a. Input t a -> a)

--------------------------------------------------------------------------------
-- | Submit a new event to the subscription state machine. Internally,
--   that event should be stored into the subscription buffer.
eventArrived :: ResolvedEvent -> Subscription t -> Subscription t
eventArrived e (Subscription k) = k (Arrived e)

--------------------------------------------------------------------------------
-- | Reads the next available event. Returns 'Nothing' it there is any. When
--   returning an event, it will be removed from the subscription buffer.
readNext :: Subscription t -> (Maybe ResolvedEvent, Subscription t)
readNext (Subscription k) = k ReadNext

--------------------------------------------------------------------------------
-- | Submits a list of events read from a stream. It's only used by a 'Catchup'
--   subscription.
batchRead :: [ResolvedEvent]
          -> Bool -- ^ If it reaches the end of the stream.
          -> Checkpoint
          -> Subscription Catchup
          -> Subscription Catchup
batchRead es eos nxt (Subscription k) = k (BatchRead es eos nxt)

--------------------------------------------------------------------------------
-- | Indicates if the subscription caught up the end of the stream, meaning the
--   subscription is actually live. Only used by 'Catchup' subscription.
hasCaughtUp :: Subscription Catchup -> Bool
hasCaughtUp (Subscription k) = k CaughtUp

--------------------------------------------------------------------------------
-- | Main 'Regular' subscription state machine.
regularSubscription :: Subscription Regular
regularSubscription = baseSubscription

--------------------------------------------------------------------------------
-- | Main 'Persistent' subscription state machine.
persistentSubscription :: Subscription Persistent
persistentSubscription = baseSubscription

--------------------------------------------------------------------------------
-- | Represents the next checkpoint to reach on a catchup subscription. Wheither
--   it's a regular stream or the $all stream, it either point to an 'Int32' or
--   a 'Position'.
data Checkpoint = CheckpointNumber Int32 | CheckpointPosition Position

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
catchupSubscription :: Subscription Catchup
catchupSubscription = Subscription $ catchingUp empty empty
  where
    catchingUp :: forall a. Seq ResolvedEvent
               -> Seq ResolvedEvent
               -> Input Catchup a
               -> a
    catchingUp b s (Arrived e) =
        Subscription $ catchingUp b (s |> e)
    catchingUp b s ReadNext =
        case viewl b of
            EmptyL    -> (Nothing, Subscription $ catchingUp b s)
            e :< rest -> (Just e, Subscription $ catchingUp rest s)
    catchingUp b s (BatchRead es eos nxt_pt) =
        let nxt_b = foldl (|>) b es
            nxt_s = dropWhileL (beforeChk nxt_pt) s
            nxt   = if eos
                    then Subscription $ caughtUp nxt_b nxt_s
                    else Subscription $ catchingUp nxt_b nxt_s in
        nxt
    catchingUp _ _ CaughtUp = False

    caughtUp :: forall a. Seq ResolvedEvent
             -> Seq ResolvedEvent
             -> Input Catchup a
             -> a
    caughtUp  b s (Arrived e) = Subscription $ caughtUp b (s |> e)
    caughtUp b s  ReadNext =
        case viewl b of
            EmptyL -> live s ReadNext
            e :< rest ->
                case viewl rest of
                    EmptyL -> (Just e, Subscription $ live s)
                    _      -> (Just e, Subscription $ caughtUp rest s)
    caughtUp b s (BatchRead _ _ _) = Subscription $ caughtUp b s
    caughtUp _ _ CaughtUp = False

    live :: forall a. Seq ResolvedEvent -> Input Catchup a -> a
    live s (Arrived e) = Subscription $ live (s |> e)
    live s ReadNext =
        case viewl s of
            EmptyL    -> (Nothing, Subscription $ live s)
            e :< rest -> (Just e, Subscription $ live rest)
    live s (BatchRead _ _ _) = Subscription $ live s
    live _ CaughtUp = True

--------------------------------------------------------------------------------
-- | Base subscription used for 'Regular' or 'Persistent' subscription.
baseSubscription :: forall t. Subscription t
baseSubscription = Subscription $ go empty
  where
    go :: forall a. Seq ResolvedEvent -> Input t a -> a
    go s (Arrived e) = Subscription $ go (s |> e)
    go s ReadNext =
        case viewl s of
            EmptyL    -> (Nothing, Subscription $ go s)
            e :< rest -> (Just e, Subscription $ go rest)
    go _ BatchRead{} = error "impossible: base subscription"
    go _ CaughtUp    = error "impossible: base subscription"
