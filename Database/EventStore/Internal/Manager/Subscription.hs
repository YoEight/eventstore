{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -fcontext-stack=26 #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Manager.Subscription
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Manager.Subscription
    ( DropReason (..)
    , Subscription(..)
    , subscriptionNetwork
    ) where

--------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.Fix
import           Data.ByteString (ByteString)
import           Data.Int
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Traversable (for)
import           Data.Word
import           GHC.Generics (Generic)

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Serialize
import Data.Text
import Data.UUID
import FRP.Sodium
import FRP.Sodium.IO
import System.Random

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Types hiding (Event, newEvent)
import Database.EventStore.Internal.Util.Sodium

--------------------------------------------------------------------------------
data SubscribeToStream
    = SubscribeToStream
      { subscribeStreamId       :: Required 1 (Value Text)
      , subscribeResolveLinkTos :: Required 2 (Value Bool)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode SubscribeToStream

--------------------------------------------------------------------------------
subscribeToStream :: Text -> Bool -> SubscribeToStream
subscribeToStream stream_id res_link_tos =
    SubscribeToStream
    { subscribeStreamId       = putField stream_id
    , subscribeResolveLinkTos = putField res_link_tos
    }

--------------------------------------------------------------------------------
data SubscriptionConfirmation
    = SubscriptionConfirmation
      { subscribeLastCommitPos   :: Required 1 (Value Int64)
      , subscribeLastEventNumber :: Optional 2 (Value Int32)
      }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode SubscriptionConfirmation

--------------------------------------------------------------------------------
data StreamEventAppeared
    = StreamEventAppeared
      { streamResolvedEvent :: Required 1 (Message ResolvedEventBuf) }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode StreamEventAppeared

--------------------------------------------------------------------------------
data DropReason
    = D_Unsubscribed
    | D_AccessDenied
    | D_NotFound
    | D_PersistentSubscriptionDeleted
    deriving (Enum, Eq, Show)

--------------------------------------------------------------------------------
data SubscriptionDropped
    = SubscriptionDropped
      { dropReason :: Optional 1 (Enumeration DropReason) }
    deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode SubscriptionDropped

--------------------------------------------------------------------------------
data UnsubscribeFromStream = UnsubscribeFromStream deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode UnsubscribeFromStream

--------------------------------------------------------------------------------
data Pending
    = Pending
      { penConf  :: Int64 -> Maybe Int32 -> Subscription
      , penCb    :: Subscription -> IO ()
      , penEvtCb :: Subscription -> Either DropReason ResolvedEvent -> IO ()
      }

--------------------------------------------------------------------------------
data SubscriptionException
    = StreamAccessDenied
    | StreamNotFound
    | SubscriptionDeleted
    deriving Show

--------------------------------------------------------------------------------
data Subscription
    = Subscription
      { subId              :: !UUID
      , subStream          :: !Text
      , subResolveLinkTos  :: !Bool
      , subLastCommitPos   :: !Int64
      , subLastEventNumber :: !(Maybe Int32)
      , subUnsubscribe     :: IO ()
      }

--------------------------------------------------------------------------------
data Sub
    = Sub
      { _subSub  :: Subscription
      , _subCb   :: Subscription -> Either DropReason ResolvedEvent -> IO ()
      , _subChan :: Chan (IO ())
      , _subAS   :: Async ()
      }

--------------------------------------------------------------------------------
data Manager
    = Manager
      { _pendings      :: !(M.Map UUID Pending)
      , _subscriptions :: !(M.Map UUID Sub)
      }

--------------------------------------------------------------------------------
initManager :: Manager
initManager = Manager M.empty M.empty

--------------------------------------------------------------------------------
-- Handled Packages
--------------------------------------------------------------------------------
subscriptionConfirmed :: Word8
subscriptionConfirmed = 0xC1

--------------------------------------------------------------------------------
streamEventAppeared :: Word8
streamEventAppeared = 0xC2

--------------------------------------------------------------------------------
subscriptionDropped :: Word8
subscriptionDropped = 0xC4

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
maybeDecodeMessage :: Decode a => ByteString -> Maybe a
maybeDecodeMessage bytes =
    case runGet decodeMessage bytes of
        Right a -> Just a
        _       -> Nothing

--------------------------------------------------------------------------------
onConfirmation :: Package -> Maybe Confirmation
onConfirmation Package{..}
    | packageCmd == subscriptionConfirmed =
          fmap (Confirmation packageCorrelation) $
          maybeDecodeMessage packageData
    | otherwise = Nothing

--------------------------------------------------------------------------------
data Appeared
    = Appeared
      { _appSub :: !Subscription
      , _appEvt :: !ResolvedEvent
      , _appCb  :: Subscription -> ResolvedEvent -> IO ()
      }

--------------------------------------------------------------------------------
onEventAppeared :: Package -> Manager -> Maybe Appeared
onEventAppeared Package{..} Manager{..}
    | packageCmd == streamEventAppeared = do
          sub <- M.lookup packageCorrelation _subscriptions
          sea <- maybeDecodeMessage packageData
          let res_evt = getField $ streamResolvedEvent sea
              chan    = _subChan sub
              evt_cb  = \s evt -> writeChan chan ((_subCb sub) s $ Right evt)
              app     = Appeared
                        { _appSub = _subSub sub
                        , _appEvt = newResolvedEventFromBuf res_evt
                        , _appCb  = evt_cb
                        }

          return app
    | otherwise = Nothing

--------------------------------------------------------------------------------
confirmSub :: Confirmation -> Manager -> IO (Maybe Sub)
confirmSub (Confirmation uuid sc) Manager{..} =
    for (M.lookup uuid _pendings) $ \p -> do
        let last_com_pos = getField $ subscribeLastCommitPos sc
            last_evt_num = getField $ subscribeLastEventNumber sc

        let sub = penConf p last_com_pos last_evt_num
        penCb p sub
        chan <- newChan
        as   <- async $ forever $ do
            action <- readChan chan
            action
        return Sub
               { _subSub  = sub
               , _subCb   = penEvtCb p
               , _subChan = chan
               , _subAS   = as
               }

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------
data Subscribe
    = Subscribe
      { _subId             :: !UUID
      , _subCallback       :: Subscription -> IO ()
      , _subStream         :: !Text
      , _subResolveLinkTos :: !Bool
      , _subEventAppeared  :: Subscription
                           -> Either DropReason ResolvedEvent
                           -> IO ()
      }

--------------------------------------------------------------------------------
data Confirmation = Confirmation !UUID !SubscriptionConfirmation

--------------------------------------------------------------------------------
type NewSubscription =
    (Subscription -> IO ())
    -> (Subscription -> Either DropReason ResolvedEvent -> IO ())
    -> Text -> Bool -> IO ()

--------------------------------------------------------------------------------
subscriptionNetwork :: Settings
                    -> (Package -> Reactive ())
                    -> Event Package
                    -> Reactive NewSubscription
subscriptionNetwork sett push_pkg e_pkg = do
    (on_sub, push_sub) <- newEvent
    (on_rem, push_rem) <- newEvent

    mgr_b <- mfix $ \mgr_b -> do
        let on_con     = filterJust $ fmap onConfirmation e_pkg
            on_con_sub = filterJust $ executeSyncIO $ snapshot confirmSub
                                                               on_con
                                                               mgr_b
            send_unsub = push_pkg . createUnsubscribePackage sett

            mgr_e = fmap (subscribe send_unsub) on_sub <>
                    fmap remove on_rem                 <>
                    fmap confirmed on_con_sub

        accum initManager mgr_e

    let on_app  = filterJust $ snapshot onEventAppeared e_pkg mgr_b
        on_drop = filterJust $ execute $ snapshot (dropError push_rem)
                                                      e_pkg
                                                      mgr_b
        push_pkg_io = pushAsync push_pkg

        push_sub_io cb evt_cb stream res_lnk_tos = randomIO >>= \uuid -> sync $
            push_sub Subscribe
                     { _subId             = uuid
                     , _subCallback       = cb
                     , _subStream         = stream
                     , _subResolveLinkTos = res_lnk_tos
                     , _subEventAppeared  = evt_cb
                     }

    _ <- listen on_sub (push_pkg_io . createSubscribePackage sett)

    _ <- listen on_app $ \(Appeared sub evt cb) -> cb sub evt

    _ <- listen on_drop $ \(Dropped reason sub _ cb) -> cb sub reason

    return push_sub_io

--------------------------------------------------------------------------------
createSubscribePackage :: Settings -> Subscribe -> Package
createSubscribePackage Settings{..} Subscribe{..} =
    Package
    { packageCmd         = 0xC0
    , packageCorrelation = _subId
    , packageData        = runPut $ encodeMessage msg
    , packageCred        = s_credentials
    }
  where
    msg = subscribeToStream _subStream _subResolveLinkTos

--------------------------------------------------------------------------------
createUnsubscribePackage :: Settings -> UUID -> Package
createUnsubscribePackage Settings{..} uuid =
    Package
    { packageCmd         = 0xC3
    , packageCorrelation = uuid
    , packageData        = runPut $ encodeMessage UnsubscribeFromStream
    , packageCred        = s_credentials
    }

--------------------------------------------------------------------------------
data Dropped
    = Dropped
      { droppedReason :: !DropReason
      , droppedSub    :: !Subscription
      , droppedId     :: !UUID
      , droppedCb     :: Subscription -> DropReason -> IO ()
      }

--------------------------------------------------------------------------------
dropError :: (UUID -> Reactive ())
          -> Package
          -> Manager
          -> Reactive (Maybe Dropped)
dropError push_rem Package{..} Manager{..} =
    for go $ \d -> do
        push_rem $ droppedId d
        return d
  where
    go | packageCmd == subscriptionDropped = do
             sub <- M.lookup packageCorrelation _subscriptions
             msg <- maybeDecodeMessage packageData
             let reason  = fromMaybe D_Unsubscribed $ getField $ dropReason msg
                 chan    = _subChan sub

                 drop_cb = \s r -> do
                     var <- newEmptyMVar
                     writeChan chan $ do
                         (_subCb sub) s (Left r)
                         putMVar var ()
                     readMVar var
                     cancel $ _subAS sub

                 dropped = Dropped
                           { droppedReason = reason
                           , droppedSub    = _subSub sub
                           , droppedId     = packageCorrelation
                           , droppedCb     = drop_cb
                           }

             return dropped
       | otherwise = Nothing

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------
subscribe :: (UUID -> Reactive ()) -> Subscribe -> Manager -> Manager
subscribe unsub Subscribe{..} s@Manager{..} =
    s { _pendings = M.insert _subId pending _pendings }
  where
    pending =
        Pending
        { penConf  = new_sub
        , penCb    = _subCallback
        , penEvtCb = _subEventAppeared
        }

    new_sub com_pos last_evt =
        Subscription
        { subId              = _subId
        , subStream          = _subStream
        , subResolveLinkTos  = _subResolveLinkTos
        , subLastCommitPos   = com_pos
        , subLastEventNumber = last_evt
        , subUnsubscribe     = sync $ unsub _subId
        }

--------------------------------------------------------------------------------
confirmed :: Sub -> Manager -> Manager
confirmed ss s@Manager{..} =
    s { _pendings      = M.delete sub_id _pendings
      , _subscriptions = M.insert sub_id ss _subscriptions
      }
  where
    sub_id = subId $ _subSub ss

--------------------------------------------------------------------------------
remove :: UUID -> Manager -> Manager
remove uuid s@Manager{..} = s { _subscriptions = M.delete uuid _subscriptions }
