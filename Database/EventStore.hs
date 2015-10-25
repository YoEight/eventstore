{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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
    , RegularSubscription
    , CatchupSubscription
    , PersistentSubscription
    , subscribe
    , subscribeToAll
    , subStreamId
    , isSubscribedToAll
    , unsubscribe
    , nextEvent
    -- , subResolveLinkTos
    -- , subLastCommitPos
    -- , subLastEventNumber
      -- * Catch-up Subscription
    , subscribeFrom
    , subscribeToAllFrom
    -- , waitTillCatchup
    -- , hasCaughtUp
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
    -- , AllEventsSlice(..)
    , Op.DeleteResult(..)
    , WriteResult(..)
    , ReadResult(..)
    , RecordedEvent(..)
    , StreamSlice(..)
    , Position(..)
    , ReadDirection(..)
    , ResolvedEvent(..)
    , OperationError(..)
    , eventResolved
    , resolvedEventOriginal
    , resolvedEventOriginalStreamId
    , resolvedEventOriginalId
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
import Data.ByteString.Lazy (fromStrict)
import Data.Int
import Data.Maybe
import Data.Monoid ((<>))

--------------------------------------------------------------------------------
import Control.Concurrent.Async
import Data.Aeson (decode)
import Data.Text hiding (group)
import Data.UUID
import Data.UUID.V4

--------------------------------------------------------------------------------
import           Database.EventStore.Internal.Connection hiding (Connection)
import qualified Database.EventStore.Internal.Manager.Subscription as S
import           Database.EventStore.Internal.Manager.Subscription.Message
import           Database.EventStore.Internal.Operation hiding (retry)
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
class Subscription a where
    subStreamId       :: a -> Text
    isSubscribedToAll :: a -> Bool
    unsubscribe       :: a -> IO ()
    nextEvent         :: a -> IO ResolvedEvent

--------------------------------------------------------------------------------
data RegularSubscription =
    RegSub
    { _regSubVar    :: TVar (SubState S.Regular)
    , _regSubRun    :: TMVar S.Running
    , _regSubStream :: Text
    , _regProd      :: Production
    }

--------------------------------------------------------------------------------
data CatchupSubscription =
    Catchup
    { _catchVar    :: TVar (SubState S.Catchup)
    , _catchRun    :: TMVar S.Running
    , _catchStream :: Text
    , _catchProd   :: Production
    }

--------------------------------------------------------------------------------
data PersistentSubscription =
    PersistentSub
    { _perVar    :: TVar (SubState S.Persistent)
    , _perRun    :: TMVar S.Running
    , _perStream :: Text
    , _perGroup  :: Text
    , _perProd   :: Production
    }

--------------------------------------------------------------------------------
-- | Acknowledges those event ids have been successfully processed.
notifyEventsProcessed :: PersistentSubscription -> [UUID] -> IO ()
notifyEventsProcessed PersistentSub{..} evts = do
    run <- atomically $ readTMVar _perRun
    pushAckPersist _perProd (return ()) run _perGroup evts

--------------------------------------------------------------------------------
-- | Acknowledges those event ids have failed to be processed successfully.
notifyEventsFailed :: PersistentSubscription
                   -> NakAction
                   -> Maybe Text
                   -> [UUID]
                   -> IO ()
notifyEventsFailed PersistentSub{..} act res evts = do
    run <- atomically $ readTMVar _perRun
    pushNakPersist _perProd (return ()) run _perGroup act res evts

--------------------------------------------------------------------------------
instance Subscription RegularSubscription where
    subStreamId = _regSubStream

    isSubscribedToAll = (== "") . subStreamId

    unsubscribe RegSub{..} = do
        run <- atomically $ readTMVar _regSubRun
        pushUnsubscribe _regProd run

    nextEvent RegSub{..} = atomically $ do
        SubState sub close <- readTVar _regSubVar
        run                <- readTMVar _regSubRun
        let (res, nxt) = S.readNext sub
        case res of
            Nothing -> do
                case close of
                    Nothing  -> retry
                    Just err -> throwSTM $ SubscriptionClosed run err
            Just e -> do
                writeTVar _regSubVar $ SubState nxt close
                return e

--------------------------------------------------------------------------------
instance Subscription CatchupSubscription where
    subStreamId = _catchStream

    unsubscribe Catchup{..} = do
        run <- atomically $ readTMVar _catchRun
        pushUnsubscribe _catchProd run

    isSubscribedToAll = (== "") . subStreamId

    nextEvent Catchup{..} = atomically $ do
        SubState sub close <- readTVar _catchVar
        run                <- readTMVar _catchRun
        let (res, nxt) = S.readNext sub
        case res of
            Nothing -> do
                case close of
                    Nothing  -> retry
                    Just err -> throwSTM $ SubscriptionClosed run err
            Just e -> do
                writeTVar _catchVar $ SubState nxt close
                return e

--------------------------------------------------------------------------------
instance Subscription PersistentSubscription where
    subStreamId = _perStream

    isSubscribedToAll _ = False

    unsubscribe PersistentSub{..} = do
        run <- atomically $ readTMVar _perRun
        pushUnsubscribe _perProd run

    nextEvent PersistentSub{..} = atomically $ do
        SubState sub close <- readTVar _perVar
        run                <- readTMVar _perRun
        let (res, nxt) = S.readNext sub
        case res of
            Nothing -> do
                case close of
                    Nothing  -> retry
                    Just err -> throwSTM $ SubscriptionClosed run err
            Just e -> do
                writeTVar _perVar $ SubState nxt close
                return e

--------------------------------------------------------------------------------
-- | Tracks a 'Subcription' lifecycle. It holds a 'Subscription' state machine
--   and `SubDropReason` if any.
data SubState a = SubState (S.Subscription a) (Maybe S.SubDropReason)

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
    deriving Show

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
          -> IO RegularSubscription
subscribe Connection{..} stream_id res_lnk_tos = do
    mvar <- newEmptyTMVarIO
    var  <- newTVarIO $ SubState S.regularSubscription Nothing
    as   <- async $ atomically $ readTMVar mvar
    let mk r = putTMVar mvar r
        recv = readTVar var
        send = writeTVar var
        dropped r = do
            SubState sm _ <- readTVar var
            writeTVar var $ SubState sm (Just r)
        cb = createSubAsync mk recv send dropped
    pushConnectStream _prod cb stream_id res_lnk_tos
    return $ RegSub var mvar stream_id _prod

--------------------------------------------------------------------------------
-- | Subcribes to $all stream.
subscribeToAll :: Connection
               -> Bool       -- ^ Resolve Link Tos
               -> IO RegularSubscription
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
              -> IO CatchupSubscription
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
                   -> IO CatchupSubscription
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
                    -> IO CatchupSubscription
subscribeFromCommon Connection{..} stream_id res_lnk_tos batch_m tpe = do
    mvar <- newEmptyTMVarIO
    var  <- newTVarIO $ SubState S.catchupSubscription Nothing
    as   <- async $ atomically $ readTMVar mvar
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
    return $ Catchup var mvar stream_id _prod

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
                                -> IO PersistentSubscription
connectToPersistentSubscription Connection{..} group stream bufSize = do
    mvar <- newEmptyTMVarIO
    var  <- newTVarIO $ SubState S.persistentSubscription Nothing
    as   <- async $ atomically $ readTMVar mvar
    let mk r = putTMVar mvar r
        recv = readTVar var
        send = writeTVar var
        dropped r = do
            SubState sm _ <- readTVar var
            writeTVar var $ SubState sm (Just r)
        cb = createSubAsync mk recv send dropped
    pushConnectPersist _prod cb group stream bufSize
    return $ PersistentSub var mvar stream group _prod

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
    go (S.Dropped _ r) = atomically $ quit r
