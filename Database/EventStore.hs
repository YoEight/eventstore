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
    , transactionStart
    , transactionCommit
    , transactionRollback
    , transactionSendEvents
      -- * Volatile Subscription
    , DropReason(..)
    , Identifiable
    , Subscription
    , NextEvent
    , Regular
    , Catchup
    , Persistent
    , subscribe
    , subscribeToAll
    , subNextEvent
    , subId
    , subStreamId
    , subIsSubscribedToAll
    , subResolveLinkTos
    , subLastCommitPos
    , subLastEventNumber
    , subUnsubscribe
      -- * Catch-up Subscription
    , CatchupError(..)
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
    , AllEventsSlice(..)
    , DeleteResult(..)
    , WriteResult(..)
    , ReadResult(..)
    , RecordedEvent(..)
    , StreamEventsSlice(..)
    , Position(..)
    , ReadDirection(..)
    , ReadAllResult(..)
    , ReadEventResult(..)
    , ResolvedEvent(..)
    , ReadStreamResult(..)
    , OperationException(..)
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
import Control.Concurrent.STM (atomically)
import Control.Exception
import Data.ByteString.Lazy (fromStrict)
import Data.Int
import Data.Monoid ((<>))

--------------------------------------------------------------------------------
import Control.Concurrent.Async
import Data.Aeson (decode)
import Data.Text hiding (group)

--------------------------------------------------------------------------------
import Database.EventStore.Catchup
import Database.EventStore.Internal.Manager.Subscription hiding (ConnectPersist)
import Database.EventStore.Internal.Manager.Subscription.Message
import Database.EventStore.Internal.Operation.DeleteStream
import Database.EventStore.Internal.Operation.ReadAllEvents
import Database.EventStore.Internal.Operation.ReadEvent
import Database.EventStore.Internal.Operation.ReadStreamEvents
import Database.EventStore.Internal.Operation.TransactionStart
import Database.EventStore.Internal.Operation.WriteEvents
import Database.EventStore.Internal.Processor
import Database.EventStore.Internal.TimeSpan
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- Connection
--------------------------------------------------------------------------------
-- | Represents a connection to a single EventStore node.
data Connection
    = Connection
      { _runCmd   :: Cmd -> IO ()
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
    processor <- newProcessor settings
    processor (DoConnect host port)

    return $ Connection processor settings

--------------------------------------------------------------------------------
-- | Asynchronously closes the 'Connection'.
shutdown :: Connection -> IO ()
shutdown Connection{..} = _runCmd DoShutdown

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
    (as, mvar) <- createAsync

    let op = writeEventsOperation _settings mvar evt_stream exp_ver evts

    _runCmd (NewOperation op)
    return as

--------------------------------------------------------------------------------
-- | Deletes given stream.
deleteStream :: Connection
             -> Text             -- ^ Stream name
             -> ExpectedVersion
             -> Maybe Bool       -- ^ Hard delete
             -> IO (Async DeleteResult)
deleteStream Connection{..} evt_stream exp_ver hard_del = do
    (as, mvar) <- createAsync

    let op = deleteStreamOperation _settings mvar evt_stream exp_ver hard_del

    _runCmd (NewOperation op)
    return as

--------------------------------------------------------------------------------
-- | Starts a transaction on given stream.
transactionStart :: Connection
                 -> Text            -- ^ Stream name
                 -> ExpectedVersion
                 -> IO (Async Transaction)
transactionStart Connection{..} evt_stream exp_ver = do
    (as, mvar) <- createAsync

    let op = transactionStartOperation _settings
                                       _runCmd
                                       mvar
                                       evt_stream
                                       exp_ver

    _runCmd (NewOperation op)
    return as

--------------------------------------------------------------------------------
-- | Reads a single event from given stream.
readEvent :: Connection
          -> Text       -- ^ Stream name
          -> Int32      -- ^ Event number
          -> Bool       -- ^ Resolve Link Tos
          -> IO (Async ReadResult)
readEvent Connection{..} stream_id evt_num res_link_tos = do
    (as, mvar) <- createAsync

    let op = readEventOperation _settings mvar stream_id evt_num res_link_tos

    _runCmd (NewOperation op)
    return as

--------------------------------------------------------------------------------
-- | Reads events from a given stream forward.
readStreamEventsForward :: Connection
                        -> Text       -- ^ Stream name
                        -> Int32      -- ^ From event number
                        -> Int32      -- ^ Batch size
                        -> Bool       -- ^ Resolve Link Tos
                        -> IO (Async StreamEventsSlice)
readStreamEventsForward mgr =
    readStreamEventsCommon mgr Forward

--------------------------------------------------------------------------------
-- | Reads events from a given stream backward.
readStreamEventsBackward :: Connection
                         -> Text       -- ^ Stream name
                         -> Int32      -- ^ From event number
                         -> Int32      -- ^ Batch size
                         -> Bool       -- ^ Resolve Link Tos
                         -> IO (Async StreamEventsSlice)
readStreamEventsBackward mgr =
    readStreamEventsCommon mgr Backward

--------------------------------------------------------------------------------
readStreamEventsCommon :: Connection
                       -> ReadDirection
                       -> Text
                       -> Int32
                       -> Int32
                       -> Bool
                       -> IO (Async StreamEventsSlice)
readStreamEventsCommon Connection{..} dir stream_id start cnt res_link_tos = do
    (as, mvar) <- createAsync

    let op = readStreamEventsOperation _settings
                                       dir
                                       mvar
                                       stream_id
                                       start
                                       cnt
                                       res_link_tos

    _runCmd (NewOperation op)
    return as

--------------------------------------------------------------------------------
-- | Reads events from the $all stream forward.
readAllEventsForward :: Connection
                     -> Position
                     -> Int32      -- ^ Batch size
                     -> Bool       -- ^ Resolve Link Tos
                     -> IO (Async AllEventsSlice)
readAllEventsForward mgr =
    readAllEventsCommon mgr Forward

--------------------------------------------------------------------------------
-- | Reads events from the $all stream backward
readAllEventsBackward :: Connection
                      -> Position
                      -> Int32      -- ^ Batch size
                      -> Bool       -- ^ Resolve Link Tos
                      -> IO (Async AllEventsSlice)
readAllEventsBackward mgr =
    readAllEventsCommon mgr Backward

--------------------------------------------------------------------------------
readAllEventsCommon :: Connection
                    -> ReadDirection
                    -> Position
                    -> Int32
                    -> Bool
                    -> IO (Async AllEventsSlice)
readAllEventsCommon Connection{..} dir pos max_c res_link_tos = do
    (as, mvar) <- createAsync

    let op = readAllEventsOperation _settings
                                    dir
                                    mvar
                                    c_pos
                                    p_pos
                                    max_c
                                    res_link_tos

    _runCmd (NewOperation op)
    return as
  where
    Position c_pos p_pos = pos

--------------------------------------------------------------------------------
-- | Subcribes to given stream.
subscribe :: Connection
          -> Text       -- ^ Stream name
          -> Bool       -- ^ Resolve Link Tos
          -> IO (Async (Subscription Regular))
subscribe Connection{..} stream_id res_lnk_tos = do
    tmp <- newEmptyMVar
    _runCmd (NewSub stream_id res_lnk_tos (putMVar tmp))
    async $ readMVar tmp

--------------------------------------------------------------------------------
-- | Subcribes to $all stream.
subscribeToAll :: Connection
               -> Bool       -- ^ Resolve Link Tos
               -> IO (Async (Subscription Regular))
subscribeToAll Connection{..} res_lnk_tos = do
    tmp <- newEmptyMVar
    _runCmd (NewSub "" res_lnk_tos (putMVar tmp))
    async $ readMVar tmp

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
subscribeFrom conn stream_id res_lnk_tos last_chk_pt batch_m = do
    catchupStart evts_fwd get_sub stream_id batch_m last_chk_pt
  where
    evts_fwd cur_num batch_size =
        readStreamEventsForward conn stream_id cur_num batch_size res_lnk_tos

    get_sub = subscribe conn stream_id res_lnk_tos

--------------------------------------------------------------------------------
-- | Same as 'subscribeFrom' but applied to $all stream.
subscribeToAllFrom :: Connection
                   -> Bool           -- ^ Resolve Link Tos
                   -> Maybe Position -- ^ Last checkpoint
                   -> Maybe Int32    -- ^ Batch size
                   -> IO (Subscription Catchup)
subscribeToAllFrom conn res_lnk_tos last_chk_pt batch_m = do
    catchupAllStart evts_fwd get_sub last_chk_pt batch_m
  where
    evts_fwd pos batch_size =
        readAllEventsForward conn pos batch_size res_lnk_tos

    get_sub = subscribeToAll conn res_lnk_tos

--------------------------------------------------------------------------------
-- | Asynchronously sets the metadata for a stream.
setStreamMetadata :: Connection
                  -> Text
                  -> ExpectedVersion
                  -> StreamMetadata
                  -> IO (Async WriteResult)
setStreamMetadata conn evt_stream exp_ver metadata =
    let dat = withJson $ streamMetadataJSON metadata
        evt = createEvent "$metadata" Nothing dat in
    sendEvent conn (metaStreamOf evt_stream) exp_ver evt

--------------------------------------------------------------------------------
-- | Asynchronously gets the metadata of a stream.
getStreamMetadata :: Connection -> Text -> IO (Async StreamMetadataResult)
getStreamMetadata conn evt_stream = do
    as <- readEvent conn (metaStreamOf evt_stream) (-1) False
    async $ atomically $ waitSTM as >>= extractStreamMetadataResult evt_stream

--------------------------------------------------------------------------------
extractStreamMetadataResult :: Monad m
                            => Text
                            -> ReadResult
                            -> m StreamMetadataResult
extractStreamMetadataResult stream rres =
    case readResultStatus rres of
        RE_SUCCESS ->
            case action of
                Just orig ->
                    case decode $ fromStrict $ recordedEventData orig of
                        Just s ->
                            let res = StreamMetadataResult
                                      { streamMetaResultStream  = stream
                                      , streamMetaResultVersion = evt_number
                                      , streamMetaResultData    = s
                                      } in
                            return res
                        Nothing -> fail "StreamMetadata: wrong format."
                Nothing -> fail "impossible: extractStreamMetadataResult"
        RE_STREAM_DELETED -> return $ DeletedStreamMetadataResult stream
        RE_NOT_FOUND      -> return $ NotFoundStreamMetadataResult stream
        RE_NO_STREAM      -> return $ NotFoundStreamMetadataResult stream
        _                 -> fail "unexpected ReadEventResult"

  where
    action     = readResultResolvedEvent rres >>= resolvedEventOriginal
    evt_number = readResultEventNumber rres

--------------------------------------------------------------------------------
-- | Asynchronously create a persistent subscription group on a stream.
createPersistentSubscription :: Connection
                             -> Text
                             -> Text
                             -> PersistentSubscriptionSettings
                             -> IO (Async ())
createPersistentSubscription Connection{..} group stream sett = do
    (as, mvar) <- createAsync
    _runCmd (CreatePersist group stream sett (putMVar mvar))
    return as

--------------------------------------------------------------------------------
-- | Asynchronously update a persistent subscription group on a stream.
updatePersistentSubscription :: Connection
                             -> Text
                             -> Text
                             -> PersistentSubscriptionSettings
                             -> IO (Async ())
updatePersistentSubscription Connection{..} group stream sett = do
    (as, mvar) <- createAsync
    _runCmd (UpdatePersist group stream sett (putMVar mvar))
    return as

--------------------------------------------------------------------------------
-- | Asynchronously delete a persistent subscription group on a stream.
deletePersistentSubscription :: Connection
                             -> Text
                             -> Text
                             -> IO (Async ())
deletePersistentSubscription Connection{..} group stream = do
    (as, mvar) <- createAsync
    _runCmd (DeletePersist group stream (putMVar mvar))
    return as

--------------------------------------------------------------------------------
-- | Asynchronously connect to a persistent subscription given a group on a
--   stream.
connectToPersistentSubscription :: Connection
                                -> Text
                                -> Text
                                -> Int32
                                -> IO (Async (Subscription Persistent))
connectToPersistentSubscription Connection{..} group stream bufSize = do
    mvar <- newEmptyMVar
    _runCmd (ConnectPersist group stream bufSize (putMVar mvar))
    async $ readMVar mvar

--------------------------------------------------------------------------------
createAsync :: IO (Async a, MVar (OperationExceptional a))
createAsync = do
    mvar <- newEmptyMVar
    as   <- async $ do
        res <- readMVar mvar
        either throwIO return res

    return (as, mvar)

--------------------------------------------------------------------------------
metaStreamOf :: Text -> Text
metaStreamOf s = "$$" <> s
