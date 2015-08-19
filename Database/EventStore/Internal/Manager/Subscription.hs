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
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Manager.Subscription
    ( module Database.EventStore.Internal.Manager.Subscription.Driver
    , module Database.EventStore.Internal.Manager.Subscription.Model
    , Regular
    , Persistent
    , Catchup
    , Subscription
    , regularSubscription
    , catchupSubscription
    , persistentSubscription
    , eventArrived
    , readNext
    , batchRead
    , hasCaughtUp
    ) where

--------------------------------------------------------------------------------
import Data.Sequence

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Manager.Subscription.Driver
import Database.EventStore.Internal.Manager.Subscription.Model
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Regular
data Persistent
data Catchup

--------------------------------------------------------------------------------
data Input t a where
    Arrived   :: ResolvedEvent -> Input t (Subscription t)
    ReadNext  :: Input t (Maybe ResolvedEvent, Subscription t)
    BatchRead :: [ResolvedEvent] -> Bool -> Input Catchup (Subscription Catchup)
    CaughtUp  :: Input Catchup Bool

--------------------------------------------------------------------------------
newtype Subscription t = Subscription (forall a. Input t a -> a)

--------------------------------------------------------------------------------
eventArrived :: ResolvedEvent -> Subscription t -> Subscription t
eventArrived e (Subscription k) = k (Arrived e)

--------------------------------------------------------------------------------
readNext :: Subscription t -> (Maybe ResolvedEvent, Subscription t)
readNext (Subscription k) = k ReadNext

--------------------------------------------------------------------------------
batchRead :: [ResolvedEvent]
          -> Bool
          -> Subscription Catchup
          -> Subscription Catchup
batchRead es eos (Subscription k) = k (BatchRead es eos)

--------------------------------------------------------------------------------
hasCaughtUp :: Subscription Catchup -> Bool
hasCaughtUp (Subscription k) = k CaughtUp

--------------------------------------------------------------------------------
regularSubscription :: Subscription Regular
regularSubscription = baseSubscription

--------------------------------------------------------------------------------
persistentSubscription :: Subscription Persistent
persistentSubscription = baseSubscription

--------------------------------------------------------------------------------
catchupSubscription :: Subscription Catchup
catchupSubscription = Subscription $ catchingUp empty empty
  where
    catchingUp :: forall a. Seq ResolvedEvent
               -> Seq ResolvedEvent
               -> Input Catchup a
               -> a
    catchingUp b s (Arrived e) = Subscription $ catchingUp b (s |> e)
    catchingUp b s ReadNext =
        case viewl b of
            EmptyL    -> (Nothing, Subscription $ catchingUp b s)
            e :< rest -> (Just e, Subscription $ catchingUp rest s)
    catchingUp b s (BatchRead es eos) =
        let nxt_b = foldl (|>) b es
            nxt   = if eos
                    then Subscription $ caughtUp nxt_b s
                    else Subscription $ catchingUp nxt_b s in
        nxt
    catchingUp _ _ CaughtUp = False

    caughtUp :: forall a. Seq ResolvedEvent
             -> Seq ResolvedEvent
             -> Input Catchup a
             -> a
    caughtUp  b s (Arrived e) = Subscription $ caughtUp b (s |> e)
    caughtUp b s ReadNext =
        case viewl b of
            EmptyL -> error "impossible: catch-up subscription"
            e :< rest ->
                case viewl rest of
                    EmptyL -> (Just e, Subscription $ live s)
                    _      -> (Just e, Subscription $ caughtUp rest s)
    caughtUp b s (BatchRead _ _) = Subscription $ caughtUp b s
    caughtUp _ _ CaughtUp = False

    live :: forall a. Seq ResolvedEvent -> Input Catchup a -> a
    live s (Arrived e) = Subscription $ live (s |> e)
    live s ReadNext =
        case viewl s of
            EmptyL    -> (Nothing, Subscription $ live s)
            e :< rest -> (Just e, Subscription $ live rest)
    live s (BatchRead _ _) = Subscription $ live s
    live _ CaughtUp = True

--------------------------------------------------------------------------------
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