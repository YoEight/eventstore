{-# LANGUAGE RecordWildCards #-}
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
    , Connection
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
-- Connection
--------------------------------------------------------------------------------
data Connection
    = Connection
      { conProcessor :: Processor
      , conSettings  :: Settings
      }

--------------------------------------------------------------------------------
-- | Creates a new connection to a single node. It maintains a full duplex
--   connection to the EventStore. An EventStore @Connection@ operates quite
--   differently than say a SQL connection. Normally when you use a SQL
--   connection you want to keep the connection open for a much longer of time
--   than when you use a SQL connection.
--
--   Another difference  is that with the EventStore @Connection@ all operation
--   are handled in a full async manner (even if you call the synchronous
--   behaviors). Many threads can use an EvenStore connection at the same time
--   or a single thread can make many asynchronous requests. To get the most
--   performance out of the connection it is generally recommend to use it in
--   this way
connect :: Settings -> HostName -> Port -> IO Connection
connect settings host port = do
    processor <- newProcessor 3
    processorConnect processor host port

    return $ Connection processor settings

--------------------------------------------------------------------------------
shutdown :: Connection -> IO ()
shutdown Connection{..} = processorShutdown conProcessor

--------------------------------------------------------------------------------
sendEvent :: Connection
          -> Text             -- ^ Stream
          -> ExpectedVersion
          -> Event
          -> IO (Async WriteResult)
sendEvent mgr evt_stream exp_ver evt =
    sendEvents mgr evt_stream exp_ver [evt]

--------------------------------------------------------------------------------
sendEvents :: Connection
           -> Text             -- ^ Stream
           -> ExpectedVersion
           -> [Event]
           -> IO (Async WriteResult)
sendEvents Connection{..} evt_stream exp_ver evts = do
    (as, mvar) <- createAsync

    let op = writeEventsOperation conSettings mvar evt_stream exp_ver evts

    processorNewOperation conProcessor op
    return as

--------------------------------------------------------------------------------
deleteStream :: Connection
             -> Text
             -> ExpectedVersion
             -> Maybe Bool       -- ^ Hard delete
             -> IO (Async DeleteResult)
deleteStream Connection{..} evt_stream exp_ver hard_del = do
    (as, mvar) <- createAsync

    let op = deleteStreamOperation conSettings mvar evt_stream exp_ver hard_del

    processorNewOperation conProcessor op
    return as

--------------------------------------------------------------------------------
transactionStart :: Connection
                 -> Text
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
readEvent :: Connection
          -> Text
          -> Int32
          -> Bool
          -> IO (Async ReadResult)
readEvent Connection{..} stream_id evt_num res_link_tos = do
    (as, mvar) <- createAsync

    let op = readEventOperation conSettings mvar stream_id evt_num res_link_tos

    processorNewOperation conProcessor op
    return as

--------------------------------------------------------------------------------
readStreamEventsForward :: Connection
                        -> Text
                        -> Int32
                        -> Int32
                        -> Bool
                        -> IO (Async StreamEventsSlice)
readStreamEventsForward mgr =
    readStreamEventsCommon mgr Forward

--------------------------------------------------------------------------------
readStreamEventsBackward :: Connection
                         -> Text
                         -> Int32
                         -> Int32
                         -> Bool
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
readAllEventsForward :: Connection
                     -> Int64
                     -> Int64
                     -> Int32
                     -> Bool
                     -> IO (Async AllEventsSlice)
readAllEventsForward mgr =
    readAllEventsCommon mgr Forward

--------------------------------------------------------------------------------
readAllEventsBackward :: Connection
                      -> Int64
                      -> Int64
                      -> Int32
                      -> Bool
                      -> IO (Async AllEventsSlice)
readAllEventsBackward mgr =
    readAllEventsCommon mgr Backward

--------------------------------------------------------------------------------
readAllEventsCommon :: Connection
                    -> ReadDirection
                    -> Int64
                    -> Int64
                    -> Int32
                    -> Bool
                    -> IO (Async AllEventsSlice)
readAllEventsCommon Connection{..} dir c_pos p_pos max_c res_link_tos = do
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

--------------------------------------------------------------------------------
createAsync :: IO (Async a, TMVar (OperationExceptional a))
createAsync = do
    mvar <- atomically newEmptyTMVar
    as   <- async $ atomically $ do
        res <- readTMVar mvar
        either throwSTM return res

    return (as, mvar)
