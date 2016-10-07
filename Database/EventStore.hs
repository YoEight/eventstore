{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
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
    ( -- * Connection
      Connection
    , ConnectionType(..)
    , ConnectionException(..)
    , Credentials
    , Settings(..)
    , Retry
    , atMost
    , keepRetrying
    , credentials
    , defaultSettings
    , defaultSSLSettings
    , connect
    , shutdown
    , waitTillClosed
    , connectionSettings
      -- * Cluster Connection
    , ClusterSettings(..)
    , DnsServer(..)
    , GossipSeed
    , gossipSeed
    , gossipSeedWithHeader
    , gossipSeedHost
    , gossipSeedHeader
    , gossipSeedPort
    , gossipSeedClusterSettings
    , dnsClusterSettings
      -- * Event
    , Event
    , EventData
    , createEvent
    , withJson
    , withJsonAndMetadata
    , withBinary
    , withBinaryAndMetadata
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
    , getCustomPropertyValue
    , getCustomProperty
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
      -- * Transaction
    , Transaction
    , TransactionId
    , startTransaction
    , transactionId
    , transactionCommit
    , transactionRollback
    , transactionWrite
      -- * Subscription
    , SubscriptionClosed(..)
    , SubscriptionId
    , Subscription
    , S.Running(..)
    , S.SubDropReason(..)
    , waitConfirmation
      -- * Volatile Subscription
    , S.Regular
    , subscribe
    , subscribeToAll
    , getSubId
    , getSubStream
    , isSubscribedToAll
    , unsubscribe
    , nextEvent
    , nextEventMaybe
    , getSubResolveLinkTos
    , getSubLastCommitPos
    , getSubLastEventNumber
      -- * Catch-up Subscription
    , S.Catchup
    , subscribeFrom
    , subscribeToAllFrom
    , waitTillCatchup
    , hasCaughtUp
     -- * Persistent Subscription
    , S.Persistent
    , PersistentSubscriptionSettings(..)
    , SystemConsumerStrategy(..)
    , NakAction(..)
    , S.PersistActionException(..)
    , acknowledge
    , acknowledgeEvents
    , failed
    , eventsFailed
    , notifyEventsProcessed
    , notifyEventsFailed
    , defaultPersistentSubscriptionSettings
    , createPersistentSubscription
    , updatePersistentSubscription
    , deletePersistentSubscription
    , connectToPersistentSubscription
     -- * Results
    , Slice(..)
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
    , StreamName(..)
    , isEventResolvedLink
    , resolvedEventOriginal
    , resolvedEventDataAsJson
    , resolvedEventOriginalStreamId
    , resolvedEventOriginalId
    , recordedEventDataAsJson
    , positionStart
    , positionEnd
      -- * Misc
    , Command
    , DropReason(..)
    , ExpectedVersion
    , anyVersion
    , noStreamVersion
    , emptyStreamVersion
    , exactEventVersion
    , streamExists
      -- * Re-export
    , waitAsync
    , (<>)
    , NonEmpty(..)
    , nonEmpty
    , TLSSettings
    ) where

--------------------------------------------------------------------------------
import Data.Int
import Data.Maybe

--------------------------------------------------------------------------------
import ClassyPrelude hiding (Builder, group)
import Data.List.NonEmpty(NonEmpty(..), nonEmpty)
import Data.UUID
import Network.Connection (TLSSettings)

--------------------------------------------------------------------------------
import           Database.EventStore.Internal.Command
import           Database.EventStore.Internal.Connection
import           Database.EventStore.Internal.Discovery
import qualified Database.EventStore.Internal.Manager.Subscription as S
import           Database.EventStore.Internal.Manager.Subscription.Message
import           Database.EventStore.Internal.Operation (OperationError(..))
import qualified Database.EventStore.Internal.Operations as Op
import           Database.EventStore.Internal.Operation.Read.Common
import           Database.EventStore.Internal.Operation.Write.Common
import           Database.EventStore.Internal.Stream
import           Database.EventStore.Internal.Types
import           Database.EventStore.Internal.Execution.Production

--------------------------------------------------------------------------------
-- Connection
--------------------------------------------------------------------------------
-- | Gathers every connection type handled by the client.
data ConnectionType
    = Static String Int
      -- ^ HostName and Port.
    | Cluster ClusterSettings
    | Dns ByteString (Maybe DnsServer) Int
      -- ^ Domain name, optional DNS server and port.

--------------------------------------------------------------------------------
-- | Represents a connection to a single EventStore node.
data Connection
    = Connection
      { _prod     :: Production
      , _settings :: Settings
      , _type     :: ConnectionType
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
connect :: Settings -> ConnectionType -> IO Connection
connect settings tpe = do
    disc <- case tpe of
        Static host port -> return $ staticEndPointDiscovery host port
        Cluster setts    -> clusterDnsEndPointDiscovery setts
        Dns dom srv port -> return $ simpleDnsEndPointDiscovery dom srv port
    prod <- newExecutionModel settings disc
    return $ Connection prod settings tpe

--------------------------------------------------------------------------------
-- | Waits the 'Connection' to be closed.
waitTillClosed :: Connection -> IO ()
waitTillClosed Connection{..} = prodWaitTillClosed _prod

--------------------------------------------------------------------------------
-- | Returns a 'Connection''s 'Settings'.
connectionSettings :: Connection -> Settings
connectionSettings = _settings

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
    when (not caughtUp) retrySTM

--------------------------------------------------------------------------------
_hasCaughtUp :: Subscription S.Catchup -> STM Bool
_hasCaughtUp Subscription{..} = do
    res <- _subState
    case res of
        SubState sm _ -> return $ S.hasCaughtUp sm
        SubException e -> throwSTM e

--------------------------------------------------------------------------------
-- | Tracks a 'Subcription' lifecycle. It holds a 'Subscription' state machine
--   and `SubDropReason` if any.
data SubState a
    = SubState (S.Subscription a) (Maybe S.SubDropReason)
    | forall e. Exception e => SubException e
      -- ^ Hack used to cover a special exception that can arise from
      --   a catchup subscription for instance. One example is when asking a
      --   catchup subscription on stream that doesn't exist yet.

--------------------------------------------------------------------------------
-- | It's possible to subscribe to a stream and be notified when new events are
--   written to that stream. There are three types of subscription which are
--   available, all of which can be useful in different situations.
--
--     * 'S.Regular'
--
--     * 'S.Catchup'
--
--     * 'S.Persistent'
data Subscription a =
    Subscription
    { _subState    :: STM (SubState a)
    , _subSetState :: SubState a -> STM ()
    , _subRun      :: TMVar S.Running
    , _subStream   :: Text
    , _subProd     :: Production
    , _subInner    :: a
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
nextEvent sub = atomically $ do
    m <- _nextEventMaybe sub
    case m of
        Nothing -> retrySTM
        Just e  -> return e

--------------------------------------------------------------------------------
-- | Non blocking version of 'nextEvent'.
nextEventMaybe :: Subscription a -> IO (Maybe ResolvedEvent)
nextEventMaybe = atomically . _nextEventMaybe

--------------------------------------------------------------------------------
-- | Waits until the `Subscription` has been confirmed.
waitConfirmation :: Subscription a -> IO ()
waitConfirmation s = atomically $ do
    _ <- readTMVar $ _subRun s
    return ()

--------------------------------------------------------------------------------
_nextEventMaybe :: Subscription a -> STM (Maybe ResolvedEvent)
_nextEventMaybe Subscription{..} = do
    st <- _subState
    case st of
        SubException e -> throwSTM e
        SubState sub close -> do
            run <- readTMVar _subRun
            let (res, nxt) = S.readNext sub
            case res of
                Nothing -> do
                    case close of
                      Nothing  -> return Nothing
                      Just err -> throwSTM $ SubscriptionClosed run err
                Just e -> do
                  _subSetState $ SubState nxt close
                  return $ Just e

--------------------------------------------------------------------------------
-- | Acknowledges those event ids have been successfully processed.
notifyEventsProcessed :: Subscription S.Persistent -> [UUID] -> IO ()
notifyEventsProcessed Subscription{..} evts = do
    run <- atomically $ readTMVar _subRun
    pushAckPersist _subProd run evts

--------------------------------------------------------------------------------
-- | Acknowledges that 'ResolvedEvent' has been successfully processed.
acknowledge :: Subscription S.Persistent -> ResolvedEvent -> IO ()
acknowledge sub e = notifyEventsProcessed sub [resolvedEventOriginalId e]

--------------------------------------------------------------------------------
-- | Acknowledges those 'ResolvedEvent's have been successfully processed.
acknowledgeEvents :: Subscription S.Persistent -> [ResolvedEvent] -> IO ()
acknowledgeEvents sub = notifyEventsProcessed sub . fmap resolvedEventOriginalId

--------------------------------------------------------------------------------
-- | Mark a message that has failed processing. The server will take action
--   based upon the action parameter.
failed :: Subscription S.Persistent
       -> ResolvedEvent
       -> NakAction
       -> Maybe Text
       -> IO ()
failed sub e a r = notifyEventsFailed sub a r [resolvedEventOriginalId e]

--------------------------------------------------------------------------------
-- | Mark messages that have failed processing. The server will take action
--   based upon the action parameter.
eventsFailed :: Subscription S.Persistent
             -> [ResolvedEvent]
             -> NakAction
             -> Maybe Text
             -> IO ()
eventsFailed sub evts a r =
    notifyEventsFailed sub a r $ fmap resolvedEventOriginalId evts

--------------------------------------------------------------------------------
-- | Acknowledges those event ids have failed to be processed successfully.
notifyEventsFailed :: Subscription S.Persistent
                   -> NakAction
                   -> Maybe Text
                   -> [UUID]
                   -> IO ()
notifyEventsFailed Subscription{..} act res evts = do
    run <- atomically $ readTMVar _subRun
    pushNakPersist _subProd run act res evts

--------------------------------------------------------------------------------
-- | Modifies 'SubState' internal state machine, letting any 'SubDropReason'
--   untouched.
modifySubSM :: (S.Subscription a -> S.Subscription a)
            -> SubState a
            -> SubState a
modifySubSM k (SubState sm r) = SubState (k sm) r
modifySubSM _ s = s

--------------------------------------------------------------------------------
-- | This exception is raised when the user tries to get the next event from a
--   'Subscription' that is already closed.
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
-- | The id of a 'Transaction'.
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
    let getState = readTVar var
        setState = writeTVar var
    return $ Subscription getState setState mvar stream_id _prod
      (S.Regular res_lnk_tos)

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
                    -> Op.CatchupState
                    -> IO (Subscription S.Catchup)
subscribeFromCommon Connection{..} stream_id res_lnk_tos batch_m tpe = do
    mvarRun <- newEmptyTMVarIO
    mvarSub <- newEmptyTMVarIO
    let readFrom res =
            case res of
                -- We want to notify the user that something went wrong in the
                -- first phase of a catchup subscription (e.g. reading the
                -- stream forward until we catchup to stream's end). This
                -- prevents a deadlock on user side in case where the user calls
                -- `waitTillCatchup` on a stream that doesn't exist.
                Left e -> atomically $ do
                  isEmpty <- isEmptyTMVar mvarSub
                  if isEmpty
                      then putTMVar mvarSub (SubException e)
                      else () <$ swapTMVar mvarSub (SubException e)

                Right (xs, eos, chk) -> atomically $ do
                    -- When a catchup subscription receives events for the
                    -- first time.
                    whenM (isEmptyTMVar mvarSub) $ do
                        let initState = SubState S.catchupSubscription Nothing
                        putTMVar mvarSub initState

                    subState <- takeTMVar mvarSub
                    let nxtSubState =
                          modifySubSM (S.batchRead xs eos chk) subState

                    putTMVar mvarSub nxtSubState
        mk   = putTMVar mvarRun
        rcv  = readTMVar mvarSub
        send = \x -> () <$ swapTMVar mvarSub x
        dropped r = do
            SubState sm _ <- takeTMVar mvarSub
            putTMVar mvarSub $ SubState sm (Just r)
        op  = Op.catchup _settings tpe res_lnk_tos batch_m
        cb  = createSubAsync mk rcv send dropped

    pushOperation _prod readFrom op
    pushConnectStream _prod cb stream_id res_lnk_tos
    let getState = readTMVar mvarSub
        setState = \x -> () <$ swapTMVar mvarSub x
    return $ Subscription getState setState mvarRun stream_id _prod S.Catchup

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
    let getState = readTVar var
        setState = writeTVar var
    return $ Subscription getState setState mvar stream _prod
      (S.Persistent group)

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
          st <- rcv
          case st of
              SubState sm close ->
                  let nxt = S.eventArrived e sm in
                  send $ SubState nxt close
              SubException _ ->
                  -- At this moment [07 October 2016], this can only happen during
                  -- the first phase of a catchup subscription where the user
                  -- asked for a subscription on a stream that doesn't exist.
                return ()
    go (S.Dropped r) = atomically $ quit r
