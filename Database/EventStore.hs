{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
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
    , Credentials
    , Settings(..)
    , LoggerSettings(..)
    , LogLevel(..)
    , LogType(..)
    , Retry
    , atMost
    , keepRetrying
    , credentials
    , defaultSettings
    , defaultSSLSettings
    , defaultLoggerSettings
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
     -- * Common Operation types
    , OperationMaxAttemptReached(..)
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
    , RegularSubscription
    , subscribe
    , subscribeToAll
    , getSubscriptionId
    , subscriptionStream
    , isSubscribedToAll
    , unsubscribe
    , nextEvent
    , nextEventMaybe
      -- * Catch-up Subscription
    , CatchupSubscription
    , subscribeFrom
    , subscribeToAllFrom
    , waitTillCatchup
    , hasCaughtUp
    , hasCaughtUpSTM
     -- * Persistent Subscription
    , PersistentSubscription
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
import           Database.EventStore.Internal.Communication
import           Database.EventStore.Internal.Discovery
import           Database.EventStore.Internal.Exec
import           Database.EventStore.Internal.Subscription.Api
import           Database.EventStore.Internal.Subscription.Catchup
import           Database.EventStore.Internal.Subscription.Message
import           Database.EventStore.Internal.Subscription.Persistent
import           Database.EventStore.Internal.Subscription.Types
import           Database.EventStore.Internal.Subscription.Regular
import           Database.EventStore.Internal.Logger
import           Database.EventStore.Internal.Messaging hiding (subscribe)
import           Database.EventStore.Internal.Operation (OperationError(..))
import qualified Database.EventStore.Internal.Operations as Op
import           Database.EventStore.Internal.Operation.Read.Common
import           Database.EventStore.Internal.Operation.Write.Common
import           Database.EventStore.Internal.Manager.Operation.Registry
import           Database.EventStore.Internal.Callback
import           Database.EventStore.Internal.Stream
import           Database.EventStore.Internal.Types

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
      { _exec     :: Exec
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
    exec <- newExec settings disc
    return $ Connection exec settings tpe

--------------------------------------------------------------------------------
-- | Waits the 'Connection' to be closed.
waitTillClosed :: Connection -> IO ()
waitTillClosed Connection{..} = execWaitTillClosed _exec

--------------------------------------------------------------------------------
-- | Returns a 'Connection''s 'Settings'.
connectionSettings :: Connection -> Settings
connectionSettings = _settings

--------------------------------------------------------------------------------
-- | Asynchronously closes the 'Connection'.
shutdown :: Connection -> IO ()
shutdown Connection{..} = publish _exec SystemShutdown

--------------------------------------------------------------------------------
-- | Sends a single 'Event' to given stream.
sendEvent :: Connection
          -> StreamName              -- ^ Stream name
          -> ExpectedVersion
          -> Event
          -> IO (Async WriteResult)
sendEvent mgr evt_stream exp_ver evt =
    sendEvents mgr evt_stream exp_ver [evt]

--------------------------------------------------------------------------------
-- | Sends a list of 'Event' to given stream.
sendEvents :: Connection
           -> StreamName             -- ^ Stream name
           -> ExpectedVersion
           -> [Event]
           -> IO (Async WriteResult)
sendEvents Connection{..} evt_stream exp_ver evts = do
    p <- newPromise
    let op = Op.writeEvents _settings (streamNameRaw evt_stream) exp_ver evts
    publish _exec (SubmitOperation p op)
    async (retrieve p)

--------------------------------------------------------------------------------
-- | Deletes given stream.
deleteStream :: Connection
             -> StreamName             -- ^ Stream name
             -> ExpectedVersion
             -> Maybe Bool       -- ^ Hard delete
             -> IO (Async Op.DeleteResult)
deleteStream Connection{..} evt_stream exp_ver hard_del = do
    p <- newPromise
    let op = Op.deleteStream _settings (streamNameRaw evt_stream) exp_ver hard_del
    publish _exec (SubmitOperation p op)
    async (retrieve p)

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
                 -> StreamName            -- ^ Stream name
                 -> ExpectedVersion
                 -> IO (Async Transaction)
startTransaction conn@Connection{..} evt_stream exp_ver = do
    p <- newPromise
    let op = Op.transactionStart _settings (streamNameRaw evt_stream) exp_ver
    publish _exec (SubmitOperation p op)
    async $ do
        tid <- retrieve p
        return Transaction
               { _tStream  = streamNameRaw evt_stream
               , _tTransId = TransactionId tid
               , _tExpVer  = exp_ver
               , _tConn    = conn
               }

--------------------------------------------------------------------------------
-- | Asynchronously writes to a transaction in the EventStore.
transactionWrite :: Transaction -> [Event] -> IO (Async ())
transactionWrite Transaction{..} evts = do
    p <- newPromise
    let Connection{..} = _tConn
        raw_id = _unTransId _tTransId
        op     = Op.transactionWrite _settings _tStream _tExpVer raw_id evts
    publish _exec (SubmitOperation p op)
    async (retrieve p)

--------------------------------------------------------------------------------
-- | Asynchronously commits this transaction.
transactionCommit :: Transaction -> IO (Async WriteResult)
transactionCommit Transaction{..} = do
    p <- newPromise
    let Connection{..} = _tConn
        raw_id = _unTransId _tTransId
        op     = Op.transactionCommit _settings _tStream _tExpVer raw_id
    publish _exec (SubmitOperation p op)
    async (retrieve p)

--------------------------------------------------------------------------------
-- | There isn't such of thing in EventStore parlance. Basically, if you want to
--   rollback, you just have to not 'transactionCommit' a 'Transaction'.
transactionRollback :: Transaction -> IO ()
transactionRollback _ = return ()

--------------------------------------------------------------------------------
-- | Reads a single event from given stream.
readEvent :: Connection
          -> StreamName       -- ^ Stream name
          -> Int32      -- ^ Event number
          -> Bool       -- ^ Resolve Link Tos
          -> IO (Async (ReadResult 'RegularStream Op.ReadEvent))
readEvent Connection{..} stream_id evt_num res_link_tos = do
    p <- newPromise
    let op = Op.readEvent _settings (streamNameRaw stream_id) evt_num res_link_tos
    publish _exec (SubmitOperation p op)
    async (retrieve p)

--------------------------------------------------------------------------------
-- | Reads events from a given stream forward.
readStreamEventsForward :: Connection
                        -> StreamName       -- ^ Stream name
                        -> Int32      -- ^ From event number
                        -> Int32      -- ^ Batch size
                        -> Bool       -- ^ Resolve Link Tos
                        -> IO (Async (ReadResult 'RegularStream StreamSlice))
readStreamEventsForward mgr =
    readStreamEventsCommon mgr Forward

--------------------------------------------------------------------------------
-- | Reads events from a given stream backward.
readStreamEventsBackward :: Connection
                         -> StreamName       -- ^ Stream name
                         -> Int32      -- ^ From event number
                         -> Int32      -- ^ Batch size
                         -> Bool       -- ^ Resolve Link Tos
                         -> IO (Async (ReadResult 'RegularStream StreamSlice))
readStreamEventsBackward mgr =
    readStreamEventsCommon mgr Backward

--------------------------------------------------------------------------------
readStreamEventsCommon :: Connection
                       -> ReadDirection
                       -> StreamName
                       -> Int32
                       -> Int32
                       -> Bool
                       -> IO (Async (ReadResult 'RegularStream StreamSlice))
readStreamEventsCommon Connection{..} dir stream_id start cnt res_link_tos = do
    p <- newPromise
    let name = streamNameRaw stream_id
        op   = Op.readStreamEvents _settings dir name start cnt res_link_tos
    publish _exec (SubmitOperation p op)
    async (retrieve p)

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
    p <- newPromise
    let op = Op.readAllEvents _settings c_pos p_pos max_c res_link_tos dir
    publish _exec (SubmitOperation p op)
    async (retrieve p)
  where
    Position c_pos p_pos = pos

--------------------------------------------------------------------------------
-- | Subcribes to given stream.
subscribe :: Connection
          -> StreamName       -- ^ Stream name
          -> Bool       -- ^ Resolve Link Tos
          -> IO RegularSubscription
subscribe Connection{..} stream resLnkTos =
    newRegularSubscription _exec stream resLnkTos

--------------------------------------------------------------------------------
-- | Subcribes to $all stream.
subscribeToAll :: Connection
               -> Bool       -- ^ Resolve Link Tos
               -> IO RegularSubscription
subscribeToAll conn = subscribe conn AllStream

--------------------------------------------------------------------------------
-- | Subscribes to given stream. If last checkpoint is defined, this will
--   'readStreamEventsForward' from that event number, otherwise from the
--   beginning. Once last stream event reached up, a subscription request will
--   be sent using 'subscribe'.
subscribeFrom :: Connection
              -> StreamName        -- ^ Stream name
              -> Bool        -- ^ Resolve Link Tos
              -> Maybe Int32 -- ^ Last checkpoint
              -> Maybe Int32 -- ^ Batch size
              -> IO CatchupSubscription
subscribeFrom conn streamId resLnkTos lastChkPt batch =
    subscribeFromCommon conn resLnkTos batch tpe
  where
    tpe = Op.RegularCatchup (streamNameRaw streamId) (fromMaybe 0 lastChkPt)

--------------------------------------------------------------------------------
-- | Same as 'subscribeFrom' but applied to $all stream.
subscribeToAllFrom :: Connection
                   -> Bool           -- ^ Resolve Link Tos
                   -> Maybe Position -- ^ Last checkpoint
                   -> Maybe Int32    -- ^ Batch size
                   -> IO CatchupSubscription
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
                    -> IO CatchupSubscription
subscribeFromCommon Connection{..} resLnkTos batch tpe =
    newCatchupSubscription _exec resLnkTos batch tpe

--------------------------------------------------------------------------------
-- | Asynchronously sets the metadata for a stream.
setStreamMetadata :: Connection
                  -> StreamName
                  -> ExpectedVersion
                  -> StreamMetadata
                  -> IO (Async WriteResult)
setStreamMetadata Connection{..} evt_stream exp_ver metadata = do
    p <- newPromise
    let name = streamNameRaw evt_stream
        op = Op.setMetaStream _settings name exp_ver metadata
    publish _exec (SubmitOperation p op)
    async (retrieve p)

--------------------------------------------------------------------------------
-- | Asynchronously gets the metadata of a stream.
getStreamMetadata :: Connection -> StreamName -> IO (Async StreamMetadataResult)
getStreamMetadata Connection{..} evt_stream = do
    p <- newPromise
    let op = Op.readMetaStream _settings (streamNameRaw evt_stream)
    publish _exec (SubmitOperation p op)
    async (retrieve p)

--------------------------------------------------------------------------------
-- | Asynchronously create a persistent subscription group on a stream.
createPersistentSubscription :: Connection
                             -> Text
                             -> StreamName
                             -> PersistentSubscriptionSettings
                             -> IO (Async (Maybe PersistActionException))
createPersistentSubscription Connection{..} group stream sett = do
    p <- newPromise
    publish _exec (CreatePersist p group (streamNameRaw stream) sett)
    async $ do
        outcome <- tryRetrieve p
        case outcome of
            Right _ -> return Nothing
            Left e  -> return $ fromException e

--------------------------------------------------------------------------------
-- | Asynchronously update a persistent subscription group on a stream.
updatePersistentSubscription :: Connection
                             -> Text
                             -> StreamName
                             -> PersistentSubscriptionSettings
                             -> IO (Async (Maybe PersistActionException))
updatePersistentSubscription Connection{..} group stream sett = do
    p <- newPromise
    publish _exec (UpdatePersist p group (streamNameRaw stream) sett)
    async $ do
        outcome <- tryRetrieve p
        case outcome of
            Right _ -> return Nothing
            Left e  -> return $ fromException e

--------------------------------------------------------------------------------
-- | Asynchronously delete a persistent subscription group on a stream.
deletePersistentSubscription :: Connection
                             -> Text
                             -> StreamName
                             -> IO (Async (Maybe PersistActionException))
deletePersistentSubscription Connection{..} group stream = do
    p <- newPromise
    publish _exec (DeletePersist p group (streamNameRaw stream))
    async $ do
        outcome <- tryRetrieve p
        case outcome of
            Right _ -> return Nothing
            Left e  -> return $ fromException e

--------------------------------------------------------------------------------
-- | Asynchronously connect to a persistent subscription given a group on a
--   stream.
connectToPersistentSubscription :: Connection
                                -> Text
                                -> StreamName
                                -> Int32
                                -> IO PersistentSubscription
connectToPersistentSubscription Connection{..} group stream bufSize =
    newPersistentSubscription _exec group stream bufSize
