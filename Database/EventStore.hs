{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore
    ( -- * Event
      Event
    , EventData
    , createEvent
    , withJson
    , withJsonAndMetadata
      -- * Connection
    , Connection
    , ConnectionException(..)
    , Credentials
    , Settings(..)
    , Retry
    , atMost
    , keepRetrying
    , credentials
    , defaultSettings
    , connect
    , shutdown
     -- * Read Operations
    , StreamMetadataResult(..)
    , readEvent
    , readAllEventsBackward
    , readAllEventsForward
    , readStreamEventsBackward
    , readStreamEventsForward
    , getStreamMetadata
      -- * Write Operations
    , StreamACL(..)
    , StreamMetadata(..)
    , streamMetadataGetCustomPropertyValue
    , streamMetadataGetCustomProperty
    , emptyStreamACL
    , emptyStreamMetadata
    , deleteStream
    , sendEvent
    , sendEvents
    , setStreamMetadata
      -- * Builder
    , Builder
      -- * Stream ACL Builder
    , StreamACLBuilder
    , buildStreamACL
    , modifyStreamACL
    , setReadRoles
    , setReadRole
    , setWriteRoles
    , setWriteRole
    , setDeleteRoles
    , setDeleteRole
    , setMetaReadRoles
    , setMetaReadRole
    , setMetaWriteRoles
    , setMetaWriteRole
      -- * Stream Metadata Builder
    , StreamMetadataBuilder
    , buildStreamMetadata
    , modifyStreamMetadata
    , setMaxCount
    , setMaxAge
    , setTruncateBefore
    , setCacheControl
    , setACL
    , modifyACL
    , setCustomProperty
      -- * TimeSpan
    , TimeSpan
    , timeSpanTicks
    , timeSpanHoursMinsSecs
    , timeSpanDaysHoursMinsSecs
    , timeSpanDaysHoursMinsSecsMillis
    , timeSpanGetTicks
    , timeSpanGetDays
    , timeSpanGetHours
    , timeSpanGetMinutes
    , timeSpanGetSeconds
    , timeSpanGetMillis
    , timeSpanFromSeconds
    , timeSpanFromMinutes
    , timeSpanFromHours
    , timeSpanFromDays
    , timeSpanTotalMillis
      -- * Transaction
    , Transaction
    , startTransaction
    , transactionId
    , transactionCommit
    , transactionRollback
    , transactionWrite
      -- * Volatile Subscription
    , DropReason(..)
    , Subscription
    , S.Regular
    , S.Catchup
    , S.Persistent
    , subscribe
    , subscribeToAll
    , getSubId
    , getSubStream
    , isSubscribedToAll
    , unsubscribe
    , nextEvent
    , getSubResolveLinkTos
    , getSubLastCommitPos
    , getSubLastEventNumber
      -- * Catch-up Subscription
    , subscribeFrom
    , subscribeToAllFrom
    , waitTillCatchup
    , hasCaughtUp
     -- * Persistent Subscription
    , PersistentSubscriptionSettings(..)
    , SystemConsumerStrategy(..)
    , NakAction(..)
    , notifyEventsProcessed
    , notifyEventsFailed
    , defaultPersistentSubscriptionSettings
    , createPersistentSubscription
    , updatePersistentSubscription
    , deletePersistentSubscription
    , connectToPersistentSubscription
     -- * Results
    , Slice
    , AllSlice
    , Op.DeleteResult(..)
    , WriteResult(..)
    , ReadResult(..)
    , RecordedEvent(..)
    , Op.ReadEvent(..)
    , StreamType(..)
    , StreamSlice
    , Position(..)
    , ReadDirection(..)
    , ResolvedEvent(..)
    , OperationError(..)
    , sliceEvents
    , sliceDirection
    , sliceEOS
    , sliceFrom
    , sliceNext
    , isEventResolvedLink
    , resolvedEventOriginal
    , resolvedEventOriginalStreamId
    , resolvedEventOriginalId
    , recordedEventDataAsJson
    , positionStart
    , positionEnd
      -- * Misc
    , ExpectedVersion
    , anyStream
    , noStream
    , emptyStream
    , exactStream
      -- * Re-export
    , module Control.Concurrent.Async
    , (<>)
    ) where

--------------------------------------------------------------------------------
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (when)
import Data.Int
import Data.Maybe
import Data.Monoid ((<>))
import Data.Typeable

--------------------------------------------------------------------------------
import Control.Concurrent.Async
import Data.Text hiding (group)
import Data.UUID

--------------------------------------------------------------------------------
import           Database.EventStore.Internal.Connection hiding (Connection)
import qualified Database.EventStore.Internal.Manager.Subscription as S
import           Database.EventStore.Internal.Manager.Subscription.Message
import           Database.EventStore.Internal.Operation (OperationError(..))
import qualified Database.EventStore.Internal.Operations as Op
import           Database.EventStore.Internal.Operation.Read.Common
import           Database.EventStore.Internal.Operation.Write.Common
import           Database.EventStore.Internal.Stream
import           Database.EventStore.Internal.TimeSpan
import           Database.EventStore.Internal.Types
import           Database.EventStore.Internal.Execution.Production

--------------------------------------------------------------------------------
-- Connection
--------------------------------------------------------------------------------
-- | Represents a connection to a single EventStore node.
data Connection
    = Connection
      { _prod     :: Production
      , _settings :: Settings
      }

--------------------------------------------------------------------------------
-- | Creates a new 'Connection' to a single node. It maintains a full duplex
--   connection to the EventStore. An EventStore 'Connection' operates quite
--   differently than say a SQL connection. Normally when you use an EventStore
--   connection you want to keep the connection open for a much longer of time
--   than when you use a SQL connection.
--
--   Another difference  is that with the EventStore 'Connection' all operations
--   are handled in a full async manner (even if you call the synchronous
--   behaviors). Many threads can use an EvenStore 'Connection' at the same time
--   or a single thread can make many asynchronous requests. To get the most
--   performance out of the connection it is generally recommended to use it in
--   this way.
connect :: Settings
        -> String   -- ^ HostName
        -> Int      -- ^ Port
        -> IO Connection
connect settings host port = do
    prod <- newExecutionModel settings host port
    return $ Connection prod settings

--------------------------------------------------------------------------------
-- | Asynchronously closes the 'Connection'.
shutdown :: Connection -> IO ()
shutdown Connection{..} = shutdownExecutionModel _prod

--------------------------------------------------------------------------------
-- | Sends a single 'Event' to given stream.
sendEvent :: Connection
          -> Text             -- ^ Stream name
          -> ExpectedVersion
          -> Event
          -> IO (Async WriteResult)
sendEvent mgr evt_stream exp_ver evt =
    sendEvents mgr evt_stream exp_ver [evt]

--------------------------------------------------------------------------------
-- | Represents a subscription id.
newtype SubscriptionId = SubId UUID deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- | Determines whether or not any link events encontered in the stream will be
--   resolved.
getSubResolveLinkTos :: Subscription S.Regular -> Bool
getSubResolveLinkTos = S._subTos . _subInner

--------------------------------------------------------------------------------
-- | Non blocking version of `waitTillCatchup`.
hasCaughtUp :: Subscription S.Catchup -> IO Bool
hasCaughtUp sub = atomically $ _hasCaughtUp sub

--------------------------------------------------------------------------------
-- | Waits until 'CatchupSubscription' subscription catch-up its stream.
waitTillCatchup :: Subscription S.Catchup -> IO ()
waitTillCatchup sub = atomically $ do
    caughtUp <- _hasCaughtUp sub
    when (not caughtUp) retry

--------------------------------------------------------------------------------
_hasCaughtUp :: Subscription S.Catchup -> STM Bool
_hasCaughtUp Subscription{..} = do
    SubState sm _ <- readTVar _subVar
    return $ S.hasCaughtUp sm

--------------------------------------------------------------------------------
-- | Tracks a 'Subcription' lifecycle. It holds a 'Subscription' state machine
--   and `SubDropReason` if any.
data SubState a = SubState (S.Subscription a) (Maybe S.SubDropReason)

--------------------------------------------------------------------------------
data Subscription a =
    Subscription
    { _subVar    :: TVar (SubState a)
    , _subRun    :: TMVar S.Running
    , _subStream :: Text
    , _subProd   :: Production
    , _subInner  :: a
    }

--------------------------------------------------------------------------------
-- | Gets the ID of the subscription.
getSubId :: Subscription a -> IO SubscriptionId
getSubId Subscription{..} = atomically $ do
    run <- readTMVar _subRun
    return $ SubId $ S.runningUUID run

--------------------------------------------------------------------------------
-- | Gets the subscription stream name.
getSubStream :: Subscription a -> Text
getSubStream = _subStream

--------------------------------------------------------------------------------
-- | Asynchronously unsubscribe from the the stream.
unsubscribe :: Subscription a -> IO ()
unsubscribe Subscription{..} = do
    run <- atomically $ readTMVar _subRun
    pushUnsubscribe _subProd run

--------------------------------------------------------------------------------
-- | If the subscription is on the $all stream.
isSubscribedToAll :: Subscription a -> Bool
isSubscribedToAll = (== "") . getSubStream

--------------------------------------------------------------------------------
-- | The last commit position seen on the subscription (if this a subscription
--   to $all stream).
getSubLastCommitPos :: Subscription a -> IO Int64
getSubLastCommitPos Subscription{..} = atomically $ do
    run <- readTMVar _subRun
    return $ S.runningLastCommitPosition run

--------------------------------------------------------------------------------
-- | The last event number seen on the subscription (if this is a subscription
--   to a single stream).
getSubLastEventNumber :: Subscription a -> IO (Maybe Int32)
getSubLastEventNumber Subscription{..} = atomically $ do
    run <- readTMVar _subRun
    return $ S.runningLastEventNumber run

--------------------------------------------------------------------------------
-- | Awaits for the next event.
nextEvent :: Subscription a -> IO ResolvedEvent
nextEvent Subscription{..} = atomically $ do
    SubState sub close <- readTVar _subVar
    run                <- readTMVar _subRun
    let (res, nxt) = S.readNext sub
    case res of
        Nothing -> do
            case close of
                Nothing  -> retry
                Just err -> throwSTM $ SubscriptionClosed run err
        Just e -> do
            writeTVar _subVar $ SubState nxt close
            return e

--------------------------------------------------------------------------------
-- | Acknowledges those event ids have been successfully processed.
notifyEventsProcessed :: Subscription S.Persistent -> [UUID] -> IO ()
notifyEventsProcessed Subscription{..} evts = do
    run <- atomically $ readTMVar _subRun
    pushAckPersist _subProd (return ()) run (S._perGroup _subInner) evts

--------------------------------------------------------------------------------
-- | Acknowledges those event ids have failed to be processed successfully.
notifyEventsFailed :: Subscription S.Persistent
                   -> NakAction
                   -> Maybe Text
                   -> [UUID]
                   -> IO ()
notifyEventsFailed Subscription{..} act res evts = do
    run <- atomically $ readTMVar _subRun
    pushNakPersist _subProd (return ()) run (S._perGroup _subInner) act res evts

--------------------------------------------------------------------------------
-- | Modifies 'SubState' internal state machine, letting any 'SubDropReason'
--   untouched.
modifySubSM :: (S.Subscription a -> S.Subscription a)
            -> SubState a
            -> SubState a
modifySubSM k (SubState sm r) = SubState (k sm) r

--------------------------------------------------------------------------------
data SubscriptionClosed =
    SubscriptionClosed S.Running S.SubDropReason
    deriving (Show, Typeable)

--------------------------------------------------------------------------------

instance Exception SubscriptionClosed
--------------------------------------------------------------------------------
-- | Sends a list of 'Event' to given stream.
sendEvents :: Connection
           -> Text             -- ^ Stream name
           -> ExpectedVersion
           -> [Event]
           -> IO (Async WriteResult)
sendEvents Connection{..} evt_stream exp_ver evts = do
    (k, as)  <- createOpAsync
    let op = Op.writeEvents _settings evt_stream exp_ver evts
    pushOperation _prod k op
    return as

--------------------------------------------------------------------------------
-- | Deletes given stream.
deleteStream :: Connection
             -> Text             -- ^ Stream name
             -> ExpectedVersion
             -> Maybe Bool       -- ^ Hard delete
             -> IO (Async Op.DeleteResult)
deleteStream Connection{..} evt_stream exp_ver hard_del = do
    (k, as) <- createOpAsync
    let op = Op.deleteStream _settings evt_stream exp_ver hard_del
    pushOperation _prod k op
    return as

--------------------------------------------------------------------------------
-- | Represents a multi-request transaction with the EventStore.
data Transaction =
    Transaction
    { _tStream  :: Text
    , _tTransId :: TransactionId
    , _tExpVer  :: ExpectedVersion
    , _tConn    :: Connection
    }

--------------------------------------------------------------------------------
-- | The id of the transaction. This can be used to recover a transaction later
newtype TransactionId =
    TransactionId { _unTransId :: Int64 }
    deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- | Gets the id of a 'Transaction'.
transactionId :: Transaction -> TransactionId
transactionId = _tTransId

--------------------------------------------------------------------------------
-- | Starts a transaction on given stream.
startTransaction :: Connection
                 -> Text            -- ^ Stream name
                 -> ExpectedVersion
                 -> IO (Async Transaction)
startTransaction conn@Connection{..} evt_stream exp_ver = do
    (k, as) <- createOpAsync
    let op = Op.transactionStart _settings evt_stream exp_ver
    pushOperation _prod k op
    let _F trans_id =
            Transaction
            { _tStream  = evt_stream
            , _tTransId = TransactionId trans_id
            , _tExpVer  = exp_ver
            , _tConn    = conn
            }
    return $ fmap _F as

--------------------------------------------------------------------------------
-- | Asynchronously writes to a transaction in the EventStore.
transactionWrite :: Transaction -> [Event] -> IO (Async ())
transactionWrite Transaction{..} evts = do
    (k, as) <- createOpAsync
    let Connection{..} = _tConn
        raw_id = _unTransId _tTransId
        op     = Op.transactionWrite _settings _tStream _tExpVer raw_id evts
    pushOperation _prod k op
    return as

--------------------------------------------------------------------------------
-- | Asynchronously commits this transaction.
transactionCommit :: Transaction -> IO (Async WriteResult)
transactionCommit Transaction{..} = do
    (k, as) <- createOpAsync
    let Connection{..} = _tConn
        raw_id = _unTransId _tTransId
        op     = Op.transactionCommit _settings _tStream _tExpVer raw_id
    pushOperation _prod k op
    return as

--------------------------------------------------------------------------------
-- | There isn't such of thing in EventStore parlance. Basically, if you want to
--   rollback, you just have to not 'transactionCommit' a 'Transaction'.
transactionRollback :: Transaction -> IO ()
transactionRollback _ = return ()

--------------------------------------------------------------------------------
-- | Reads a single event from given stream.
readEvent :: Connection
          -> Text       -- ^ Stream name
          -> Int32      -- ^ Event number
          -> Bool       -- ^ Resolve Link Tos
          -> IO (Async (ReadResult 'RegularStream Op.ReadEvent))
readEvent Connection{..} stream_id evt_num res_link_tos = do
    (k, as) <- createOpAsync
    let op = Op.readEvent _settings stream_id evt_num res_link_tos
    pushOperation _prod k op
    return as

--------------------------------------------------------------------------------
-- | Reads events from a given stream forward.
readStreamEventsForward :: Connection
                        -> Text       -- ^ Stream name
                        -> Int32      -- ^ From event number
                        -> Int32      -- ^ Batch size
                        -> Bool       -- ^ Resolve Link Tos
                        -> IO (Async (ReadResult 'RegularStream StreamSlice))
readStreamEventsForward mgr =
    readStreamEventsCommon mgr Forward

--------------------------------------------------------------------------------
-- | Reads events from a given stream backward.
readStreamEventsBackward :: Connection
                         -> Text       -- ^ Stream name
                         -> Int32      -- ^ From event number
                         -> Int32      -- ^ Batch size
                         -> Bool       -- ^ Resolve Link Tos
                         -> IO (Async (ReadResult 'RegularStream StreamSlice))
readStreamEventsBackward mgr =
    readStreamEventsCommon mgr Backward

--------------------------------------------------------------------------------
readStreamEventsCommon :: Connection
                       -> ReadDirection
                       -> Text
                       -> Int32
                       -> Int32
                       -> Bool
                       -> IO (Async (ReadResult 'RegularStream StreamSlice))
readStreamEventsCommon Connection{..} dir stream_id start cnt res_link_tos = do
    (k, as) <- createOpAsync
    let op = Op.readStreamEvents _settings dir stream_id start cnt res_link_tos
    pushOperation _prod k op
    return as

--------------------------------------------------------------------------------
-- | Reads events from the $all stream forward.
readAllEventsForward :: Connection
                     -> Position
                     -> Int32      -- ^ Batch size
                     -> Bool       -- ^ Resolve Link Tos
                     -> IO (Async AllSlice)
readAllEventsForward mgr =
    readAllEventsCommon mgr Forward

--------------------------------------------------------------------------------
-- | Reads events from the $all stream backward
readAllEventsBackward :: Connection
                      -> Position
                      -> Int32      -- ^ Batch size
                      -> Bool       -- ^ Resolve Link Tos
                      -> IO (Async AllSlice)
readAllEventsBackward mgr =
    readAllEventsCommon mgr Backward

--------------------------------------------------------------------------------
readAllEventsCommon :: Connection
                    -> ReadDirection
                    -> Position
                    -> Int32
                    -> Bool
                    -> IO (Async AllSlice)
readAllEventsCommon Connection{..} dir pos max_c res_link_tos = do
    (k, as) <- createOpAsync
    let op = Op.readAllEvents _settings c_pos p_pos max_c res_link_tos dir
    pushOperation _prod k op
    return as
  where
    Position c_pos p_pos = pos

--------------------------------------------------------------------------------
-- | Subcribes to given stream.
subscribe :: Connection
          -> Text       -- ^ Stream name
          -> Bool       -- ^ Resolve Link Tos
          -> IO (Subscription S.Regular)
subscribe Connection{..} stream_id res_lnk_tos = do
    mvar <- newEmptyTMVarIO
    var  <- newTVarIO $ SubState S.regularSubscription Nothing
    let mk r = putTMVar mvar r
        recv = readTVar var
        send = writeTVar var
        dropped r = do
            SubState sm _ <- readTVar var
            writeTVar var $ SubState sm (Just r)
        cb = createSubAsync mk recv send dropped
    pushConnectStream _prod cb stream_id res_lnk_tos
    return $ Subscription var mvar stream_id _prod (S.Regular res_lnk_tos)

--------------------------------------------------------------------------------
-- | Subcribes to $all stream.
subscribeToAll :: Connection
               -> Bool       -- ^ Resolve Link Tos
               -> IO (Subscription S.Regular)
subscribeToAll conn res_lnk_tos = subscribe conn "" res_lnk_tos

--------------------------------------------------------------------------------
-- | Subscribes to given stream. If last checkpoint is defined, this will
--   'readStreamEventsForward' from that event number, otherwise from the
--   beginning. Once last stream event reached up, a subscription request will
--   be sent using 'subscribe'.
subscribeFrom :: Connection
              -> Text        -- ^ Stream name
              -> Bool        -- ^ Resolve Link Tos
              -> Maybe Int32 -- ^ Last checkpoint
              -> Maybe Int32 -- ^ Batch size
              -> IO (Subscription S.Catchup)
subscribeFrom conn stream_id res_lnk_tos last_chk_pt batch_m =
    subscribeFromCommon conn stream_id res_lnk_tos batch_m tpe
  where
    tpe = Op.RegularCatchup stream_id (fromMaybe 0 last_chk_pt)

--------------------------------------------------------------------------------
-- | Same as 'subscribeFrom' but applied to $all stream.
subscribeToAllFrom :: Connection
                   -> Bool           -- ^ Resolve Link Tos
                   -> Maybe Position -- ^ Last checkpoint
                   -> Maybe Int32    -- ^ Batch size
                   -> IO (Subscription S.Catchup)
subscribeToAllFrom conn res_lnk_tos last_chk_pt batch_m =
    subscribeFromCommon conn "" res_lnk_tos batch_m tpe
  where
    Position c_pos p_pos = fromMaybe positionStart last_chk_pt
    tpe = Op.AllCatchup c_pos p_pos

--------------------------------------------------------------------------------
subscribeFromCommon :: Connection
                    -> Text
                    -> Bool
                    -> Maybe Int32
                    -> Op.CatchupType
                    -> IO (Subscription S.Catchup)
subscribeFromCommon Connection{..} stream_id res_lnk_tos batch_m tpe = do
    mvar <- newEmptyTMVarIO
    var  <- newTVarIO $ SubState S.catchupSubscription Nothing
    let readFrom res =
            case res of
                Left _ -> return ()
                Right (xs, eos) -> atomically $ do
                    s <- readTVar var
                    let nxt_s = modifySubSM (S.batchRead xs eos) s
                    writeTVar var nxt_s
        mk   = putTMVar mvar
        rcv  = readTVar var
        send = writeTVar var
        dropped r = do
            SubState sm _ <- readTVar var
            writeTVar var $ SubState sm (Just r)
        op  = Op.catchup _settings tpe res_lnk_tos batch_m
        cb  = createSubAsync mk rcv send dropped

    pushOperation _prod readFrom op
    pushConnectStream _prod cb stream_id res_lnk_tos
    return $ Subscription var mvar stream_id _prod S.Catchup

--------------------------------------------------------------------------------
-- | Asynchronously sets the metadata for a stream.
setStreamMetadata :: Connection
                  -> Text
                  -> ExpectedVersion
                  -> StreamMetadata
                  -> IO (Async WriteResult)
setStreamMetadata Connection{..} evt_stream exp_ver metadata = do
    (k, as) <- createOpAsync
    let op = Op.setMetaStream _settings evt_stream exp_ver metadata
    pushOperation _prod k op
    return as

--------------------------------------------------------------------------------
-- | Asynchronously gets the metadata of a stream.
getStreamMetadata :: Connection -> Text -> IO (Async StreamMetadataResult)
getStreamMetadata Connection{..} evt_stream = do
    (k, as) <- createOpAsync
    let op = Op.readMetaStream _settings evt_stream
    pushOperation _prod k op
    return as

--------------------------------------------------------------------------------
-- | Asynchronously create a persistent subscription group on a stream.
createPersistentSubscription :: Connection
                             -> Text
                             -> Text
                             -> PersistentSubscriptionSettings
                             -> IO (Async (Maybe S.PersistActionException))
createPersistentSubscription Connection{..} group stream sett = do
    mvar <- newEmptyTMVarIO
    let _F res = atomically $
            case res of
                Left e -> putTMVar mvar (Just e)
                _      -> putTMVar mvar Nothing
    pushCreatePersist _prod _F group stream sett
    async $ atomically $ readTMVar mvar

--------------------------------------------------------------------------------
-- | Asynchronously update a persistent subscription group on a stream.
updatePersistentSubscription :: Connection
                             -> Text
                             -> Text
                             -> PersistentSubscriptionSettings
                             -> IO (Async (Maybe S.PersistActionException))
updatePersistentSubscription Connection{..} group stream sett = do
    mvar <- newEmptyTMVarIO
    let _F res = atomically $
            case res of
                Left e -> putTMVar mvar (Just e)
                _      -> putTMVar mvar Nothing
    pushUpdatePersist _prod _F group stream sett
    async $ atomically $ readTMVar mvar

--------------------------------------------------------------------------------
-- | Asynchronously delete a persistent subscription group on a stream.
deletePersistentSubscription :: Connection
                             -> Text
                             -> Text
                             -> IO (Async (Maybe S.PersistActionException))
deletePersistentSubscription Connection{..} group stream = do
    mvar <- newEmptyTMVarIO
    let _F res = atomically $
            case res of
                Left e -> putTMVar mvar (Just e)
                _      -> putTMVar mvar Nothing
    pushDeletePersist _prod _F group stream
    async $ atomically $ readTMVar mvar

--------------------------------------------------------------------------------
-- | Asynchronously connect to a persistent subscription given a group on a
--   stream.
connectToPersistentSubscription :: Connection
                                -> Text
                                -> Text
                                -> Int32
                                -> IO (Subscription S.Persistent)
connectToPersistentSubscription Connection{..} group stream bufSize = do
    mvar <- newEmptyTMVarIO
    var  <- newTVarIO $ SubState S.persistentSubscription Nothing
    let mk r = putTMVar mvar r
        recv = readTVar var
        send = writeTVar var
        dropped r = do
            SubState sm _ <- readTVar var
            writeTVar var $ SubState sm (Just r)
        cb = createSubAsync mk recv send dropped
    pushConnectPersist _prod cb group stream bufSize
    return $ Subscription var mvar stream _prod (S.Persistent group)

--------------------------------------------------------------------------------
createOpAsync :: IO (Either OperationError a -> IO (), Async a)
createOpAsync = do
    mvar <- newEmptyMVar
    as   <- async $ do
        res <- readMVar mvar
        either throwIO return res
    return (putMVar mvar, as)

--------------------------------------------------------------------------------
createSubAsync :: (S.Running -> STM ())
               -> STM (SubState a)
               -> (SubState a -> STM ())
               -> (S.SubDropReason -> STM ())
               -> (S.SubConnectEvent -> IO ())
createSubAsync mk rcv send quit = go
  where
    go (S.SubConfirmed run) = atomically $ mk run
    go (S.EventAppeared e) = atomically $ do
        SubState sm close <- rcv
        let nxt = S.eventArrived e sm
        send $ SubState nxt close
    go (S.Dropped r) = atomically $ quit r
