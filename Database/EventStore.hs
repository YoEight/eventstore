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
      -- * Transaction
    , Transaction
    , transactionStart
    , transactionCommit
    , transactionRollback
    , transactionSendEvents
      -- * Volatile Subscription
    , DropReason(..)
    , Subscription
    , subscribe
    , subscribeToAll
    , subAwait
    , subId
    , subStream
    , subResolveLinkTos
    , subLastCommitPos
    , subLastEventNumber
    , subUnsubscribe
      -- * Catch-up Subscription
    , Catchup
    , CatchupError(..)
    , subscribeFrom
    , subscribeToAllFrom
    , catchupAwait
    , catchupStream
    , catchupUnsubscribe
    , waitTillCatchup
    , hasCaughtUp
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
import Data.Text

--------------------------------------------------------------------------------
import Database.EventStore.Catchup
import Database.EventStore.Internal.Processor
import Database.EventStore.Internal.TimeSpan
import Database.EventStore.Internal.Types
import Database.EventStore.Internal.Operation.DeleteStreamOperation
import Database.EventStore.Internal.Operation.ReadAllEventsOperation
import Database.EventStore.Internal.Operation.ReadEventOperation
import Database.EventStore.Internal.Operation.ReadStreamEventsOperation
import Database.EventStore.Internal.Operation.TransactionStartOperation
import Database.EventStore.Internal.Operation.WriteEventsOperation

--------------------------------------------------------------------------------
-- Connection
--------------------------------------------------------------------------------
-- | Represents a connection to a single EventStore node.
data Connection
    = Connection
      { conProcessor :: Processor
      , conSettings  :: Settings
      }

--------------------------------------------------------------------------------
-- | Creates a new 'Connection' to a single node. It maintains a full duplex
--   connection to the EventStore. An EventStore 'Connection' operates quite
--   differently than say a SQL connection. Normally when you use a SQL
--   connection you want to keep the connection open for a much longer of time
--   than when you use a SQL connection.
--
--   Another difference  is that with the EventStore 'Connection' all operation
--   are handled in a full async manner (even if you call the synchronous
--   behaviors). Many threads can use an EvenStore 'Connection' at the same time
--   or a single thread can make many asynchronous requests. To get the most
--   performance out of the connection it is generally recommend to use it in
--   this way.
connect :: Settings
        -> String   -- ^ HostName
        -> Int      -- ^ Port
        -> IO Connection
connect settings host port = do
    processor <- newProcessor settings
    processorConnect processor host port

    return $ Connection processor settings

--------------------------------------------------------------------------------
-- | Asynchronously closes the 'Connection'.
shutdown :: Connection -> IO ()
shutdown Connection{..} = processorShutdown conProcessor

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

    let op = writeEventsOperation conSettings mvar evt_stream exp_ver evts

    processorNewOperation conProcessor op
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

    let op = deleteStreamOperation conSettings mvar evt_stream exp_ver hard_del

    processorNewOperation conProcessor op
    return as

--------------------------------------------------------------------------------
-- | Starts a transaction on given stream.
transactionStart :: Connection
                 -> Text            -- ^ Stream name
                 -> ExpectedVersion
                 -> IO (Async Transaction)
transactionStart Connection{..} evt_stream exp_ver = do
    (as, mvar) <- createAsync

    let op = transactionStartOperation conSettings
                                       conProcessor
                                       mvar
                                       evt_stream
                                       exp_ver

    processorNewOperation conProcessor op
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

    let op = readEventOperation conSettings mvar stream_id evt_num res_link_tos

    processorNewOperation conProcessor op
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

    let op = readStreamEventsOperation conSettings
                                       dir
                                       mvar
                                       stream_id
                                       start
                                       cnt
                                       res_link_tos

    processorNewOperation conProcessor op
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

    let op = readAllEventsOperation conSettings
                                    dir
                                    mvar
                                    c_pos
                                    p_pos
                                    max_c
                                    res_link_tos

    processorNewOperation conProcessor op
    return as
  where
    Position c_pos p_pos = pos

--------------------------------------------------------------------------------
-- | Subcribes to given stream.
subscribe :: Connection
          -> Text       -- ^ Stream name
          -> Bool       -- ^ Resolve Link Tos
          -> IO (Async Subscription)
subscribe Connection{..} stream_id res_lnk_tos = do
    tmp <- newEmptyMVar
    processorNewSubcription conProcessor
                            (putMVar tmp)
                            stream_id
                            res_lnk_tos
    async $ readMVar tmp

--------------------------------------------------------------------------------
-- | Subcribes to $all stream.
subscribeToAll :: Connection
               -> Bool       -- ^ Resolve Link Tos
               -> IO (Async Subscription)
subscribeToAll Connection{..} res_lnk_tos = do
    tmp <- newEmptyMVar
    processorNewSubcription conProcessor
                            (putMVar tmp)
                            ""
                            res_lnk_tos
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
              -> IO Catchup
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
                   -> IO Catchup
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
                                      , streamMetaResultDeleted = False
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
