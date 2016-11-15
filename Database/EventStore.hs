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
    , EventType(..)
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
    , SubDropReason(..)
    , waitConfirmation
    , unsubscribeConfirmed
    , unsubscribeConfirmedSTM
    , waitUnsubscribeConfirmed
      -- * Volatile Subscription
    , Regular
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
    , Catchup
    , subscribeFrom
    , subscribeToAllFrom
    , waitTillCatchup
    , hasCaughtUp
    , hasCaughtUpSTM
     -- * Persistent Subscription
    , Persistent
    , PersistentSubscriptionSettings(..)
    , SystemConsumerStrategy(..)
    , NakAction(..)
    , PersistActionException(..)
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
import Network.Connection (TLSSettings)

--------------------------------------------------------------------------------
import           Database.EventStore.Internal.Command
import           Database.EventStore.Internal.Connection
import           Database.EventStore.Internal.Discovery
import           Database.EventStore.Internal.Subscription
import           Database.EventStore.Internal.Manager.Subscription.Driver hiding (unsubscribe)
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
mkSubEnv :: Connection -> SubEnv
mkSubEnv Connection{..} =
    SubEnv
    { subSettings = _settings
    , subPushOp = pushOperation _prod
    , subPushConnect = \k cmd ->
          case cmd of
              PushRegular stream tos ->
                  pushConnectStream _prod k stream tos
              PushPersistent group stream size ->
                  pushConnectPersist _prod k group stream size
    , subPushUnsub = pushUnsubscribe _prod
    , subAckCmd = \cmd run uuids ->
          case cmd of
              AckCmd -> pushAckPersist _prod run uuids
              NakCmd act res -> pushNakPersist _prod run act res uuids
    , subForceReconnect = \node ->
          pushForceReconnect _prod node
    }

--------------------------------------------------------------------------------
-- | Subcribes to given stream.
subscribe :: Connection
          -> Text       -- ^ Stream name
          -> Bool       -- ^ Resolve Link Tos
          -> IO (Subscription Regular)
subscribe conn streamId resLnkTos =
    regularSub (mkSubEnv conn) streamId resLnkTos

--------------------------------------------------------------------------------
-- | Subcribes to $all stream.
subscribeToAll :: Connection
               -> Bool       -- ^ Resolve Link Tos
               -> IO (Subscription Regular)
subscribeToAll conn = subscribe conn ""

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
              -> IO (Subscription Catchup)
subscribeFrom conn streamId resLnkTos lastChkPt batch =
    subscribeFromCommon conn resLnkTos batch tpe
  where
    tpe = Op.RegularCatchup streamId (fromMaybe 0 lastChkPt)

--------------------------------------------------------------------------------
-- | Same as 'subscribeFrom' but applied to $all stream.
subscribeToAllFrom :: Connection
                   -> Bool           -- ^ Resolve Link Tos
                   -> Maybe Position -- ^ Last checkpoint
                   -> Maybe Int32    -- ^ Batch size
                   -> IO (Subscription Catchup)
subscribeToAllFrom conn resLnkTos lastChkPt batch =
    subscribeFromCommon conn resLnkTos batch tpe
  where
    Position cPos pPos = fromMaybe positionStart lastChkPt
    tpe = Op.AllCatchup cPos pPos

--------------------------------------------------------------------------------
subscribeFromCommon :: Connection
                    -> Bool
                    -> Maybe Int32
                    -> Op.CatchupState
                    -> IO (Subscription Catchup)
subscribeFromCommon conn resLnkTos batch tpe =
    catchupSub (mkSubEnv conn) params
  where
    params = CatchupParams { catchupResLnkTos = resLnkTos
                           , catchupState = tpe
                           , catchupBatchSize = batch
                           }

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
                             -> IO (Async (Maybe PersistActionException))
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
                             -> IO (Async (Maybe PersistActionException))
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
                             -> IO (Async (Maybe PersistActionException))
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
                                -> IO (Subscription Persistent)
connectToPersistentSubscription conn group stream bufSize =
    persistentSub (mkSubEnv conn) group stream bufSize

--------------------------------------------------------------------------------
createOpAsync :: IO (Either OperationError a -> IO (), Async a)
createOpAsync = do
    mvar <- newEmptyMVar
    as   <- async $ do
        res <- readMVar mvar
        either throwIO return res
    return (putMVar mvar, as)
