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
    ( Event
    , EventData
    , EventStoreConnection
    , ExpectedVersion(..)
    , HostName
    , Port
      -- * Result
    , AllEventsSlice(..)
    , DeleteResult(..)
    , WriteResult(..)
    , ReadResult(..)
    , RecordedEvent(..)
    , StreamEventsSlice(..)
    , eventResolved
    , resolvedEventOriginal
    , resolvedEventOriginalStreamId
      -- * Event
    , createEvent
    , withJson
    , withJsonAndMetadata
      -- * Connection manager
    , defaultSettings
    , connect
    , deleteStream
    , readEvent
    , readAllEventsBackward
    , readAllEventsForward
    , readStreamEventsBackward
    , readStreamEventsForward
    , sendEvent
    , sendEvents
    , shutdown
    , transactionStart
      -- * Transaction
    , Transaction
    , transactionCommit
    , transactionRollback
    , transactionSendEvents
      -- * Re-export
    , module Control.Concurrent.Async
    ) where

--------------------------------------------------------------------------------
import Control.Concurrent
import Control.Concurrent.STM
import Data.Int

--------------------------------------------------------------------------------
import Control.Concurrent.Async
import Data.Text

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Processor
import Database.EventStore.Internal.Types
import Database.EventStore.Internal.Operation.DeleteStreamOperation
import Database.EventStore.Internal.Operation.ReadAllEventsOperation
import Database.EventStore.Internal.Operation.ReadEventOperation
import Database.EventStore.Internal.Operation.ReadStreamEventsOperation
import Database.EventStore.Internal.Operation.TransactionStartOperation
import Database.EventStore.Internal.Operation.WriteEventsOperation

--------------------------------------------------------------------------------
type HostName = String
type Port     = Int

--------------------------------------------------------------------------------
-- EventStoreConnection
--------------------------------------------------------------------------------
data EventStoreConnection
    = EventStoreConnection
      { mgrChan     :: TChan Msg
      , mgrSettings :: Settings
      , mgrThreadId :: ThreadId
      }

--------------------------------------------------------------------------------
msgQueue :: EventStoreConnection -> Msg -> IO ()
msgQueue mgr msg = atomically $ writeTChan chan msg
  where
    chan = mgrChan mgr

--------------------------------------------------------------------------------
-- | Creates a new connection to a single node. It maintains a full duplex
--   connection to the EventStore. An @EventStoreConnection@ operates quite
--   differently than say a SQL connection. Normally when you use a SQL
--   connection you want to keep the connection open for a much longer of time
--   than when you use a SQL connection.
--
--   Another difference  is that with the @EventStoreConnection@ all operation
--   are handled in a full async manner (even if you call the synchronous
--   behaviors). Many threads can use an EvenStore connection at the same time
--   or a single thread can make many asynchronous requests. To get the most
--   performance out of the connection it is generally recommend to use it in
--   this way
connect :: Settings -> HostName -> Port -> IO EventStoreConnection
connect settings host port = do
    chan <- newTChanIO
    app  <- newProcessor settings chan host port
    tid  <- forkFinally (appProcess app) (\_ -> appFinalizer app)

    return $ EventStoreConnection chan settings tid

--------------------------------------------------------------------------------
shutdown :: EventStoreConnection -> IO ()
shutdown mgr = killThread tid
  where
    tid = mgrThreadId mgr

--------------------------------------------------------------------------------
sendEvent :: EventStoreConnection
          -> Text             -- ^ Stream
          -> ExpectedVersion
          -> Event
          -> IO (Async WriteResult)
sendEvent mgr evt_stream exp_ver evt =
    sendEvents mgr evt_stream exp_ver [evt]

--------------------------------------------------------------------------------
sendEvents :: EventStoreConnection
           -> Text             -- ^ Stream
           -> ExpectedVersion
           -> [Event]
           -> IO (Async WriteResult)
sendEvents mgr evt_stream exp_ver evts = do
    (as, mvar) <- createAsync

    let op = writeEventsOperation settings mvar evt_stream exp_ver evts

    msgQueue mgr (RegisterOperation op)
    return as
  where
    settings = mgrSettings mgr

--------------------------------------------------------------------------------
deleteStream :: EventStoreConnection
             -> Text
             -> ExpectedVersion
             -> Maybe Bool       -- ^ Hard delete
             -> IO (Async DeleteResult)
deleteStream mgr evt_stream exp_ver hard_del = do
    (as, mvar) <- createAsync

    let op = deleteStreamOperation settings mvar evt_stream exp_ver hard_del

    msgQueue mgr (RegisterOperation op)
    return as
  where
    settings = mgrSettings mgr

--------------------------------------------------------------------------------
transactionStart :: EventStoreConnection
                 -> Text
                 -> ExpectedVersion
                 -> IO (Async Transaction)
transactionStart mgr evt_stream exp_ver = do
    (as, mvar) <- createAsync

    let op = transactionStartOperation settings chan mvar evt_stream exp_ver

    msgQueue mgr (RegisterOperation op)
    return as
  where
    chan     = mgrChan mgr
    settings = mgrSettings mgr

--------------------------------------------------------------------------------
readEvent :: EventStoreConnection
          -> Text
          -> Int32
          -> Bool
          -> IO (Async ReadResult)
readEvent mgr stream_id evt_num res_link_tos = do
    (as, mvar) <- createAsync

    let op = readEventOperation settings mvar stream_id evt_num res_link_tos

    msgQueue mgr (RegisterOperation op)
    return as
  where
    settings = mgrSettings mgr

--------------------------------------------------------------------------------
readStreamEventsForward :: EventStoreConnection
                        -> Text
                        -> Int32
                        -> Int32
                        -> Bool
                        -> IO (Async StreamEventsSlice)
readStreamEventsForward mgr =
    readStreamEventsCommon mgr Forward

--------------------------------------------------------------------------------
readStreamEventsBackward :: EventStoreConnection
                         -> Text
                         -> Int32
                         -> Int32
                         -> Bool
                         -> IO (Async StreamEventsSlice)
readStreamEventsBackward mgr =
    readStreamEventsCommon mgr Backward

--------------------------------------------------------------------------------
readStreamEventsCommon :: EventStoreConnection
                       -> ReadDirection
                       -> Text
                       -> Int32
                       -> Int32
                       -> Bool
                       -> IO (Async StreamEventsSlice)
readStreamEventsCommon mgr dir stream_id start cnt res_link_tos = do
    (as, mvar) <- createAsync

    let op = readStreamEventsOperation settings
                                       dir
                                       mvar
                                       stream_id
                                       start
                                       cnt
                                       res_link_tos

    msgQueue mgr (RegisterOperation op)
    return as
  where
    settings = mgrSettings mgr

--------------------------------------------------------------------------------
readAllEventsForward :: EventStoreConnection
                     -> Int64
                     -> Int64
                     -> Int32
                     -> Bool
                     -> IO (Async AllEventsSlice)
readAllEventsForward mgr =
    readAllEventsCommon mgr Forward

--------------------------------------------------------------------------------
readAllEventsBackward :: EventStoreConnection
                      -> Int64
                      -> Int64
                      -> Int32
                      -> Bool
                      -> IO (Async AllEventsSlice)
readAllEventsBackward mgr =
    readAllEventsCommon mgr Backward

--------------------------------------------------------------------------------
readAllEventsCommon :: EventStoreConnection
                    -> ReadDirection
                    -> Int64
                    -> Int64
                    -> Int32
                    -> Bool
                    -> IO (Async AllEventsSlice)
readAllEventsCommon mgr dir c_pos p_pos max_c res_link_tos = do
    (as, mvar) <- createAsync

    let op = readAllEventsOperation settings
                                    dir
                                    mvar
                                    c_pos
                                    p_pos
                                    max_c
                                    res_link_tos

    msgQueue mgr (RegisterOperation op)
    return as
  where
    settings = mgrSettings mgr

--------------------------------------------------------------------------------
createAsync :: IO (Async a, TMVar (OperationExceptional a))
createAsync = do
    mvar <- atomically newEmptyTMVar
    as   <- async $ atomically $ do
        res <- readTMVar mvar
        either throwSTM return res

    return (as, mvar)
