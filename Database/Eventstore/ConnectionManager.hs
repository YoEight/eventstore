--------------------------------------------------------------------------------
-- |
-- Module : Database.Eventstore.ConnectionManager
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.Eventstore.ConnectionManager
    ( ConnectionManager
    , Event
    , EventData
    , ExpectedVersion(..)
      -- * Result
    , DeleteResult(..)
    , WriteResult(..)
    , ReadResult(..)
    , RecordedEvent(..)
    , eventResolved
    , resolvedEventOriginal
    , resolvedEventOriginalStreamId
      -- * Event
    , createEvent
    , withJson
    , withJsonAndMetadata
      -- * Connection manager
    , defaultSettings
    , eventStoreConnect
    , eventStoreDeleteStream
    , eventStoreReadEvent
    , eventStoreSendEvent
    , eventStoreSendEvents
    , eventStoreShutdown
    , eventStoreTransactionStart
      -- * Transaction
    , Transaction
    , transactionCommit
    , transactionRollback
    , transactionSendEvents
    ) where

--------------------------------------------------------------------------------
import Control.Concurrent
import Control.Concurrent.STM
import Data.Int

--------------------------------------------------------------------------------
import Control.Concurrent.Async
import Data.Text

--------------------------------------------------------------------------------
import Database.Eventstore.Internal.Processor
import Database.Eventstore.Internal.Types
import Database.Eventstore.Internal.Operation.DeleteStreamOperation
import Database.Eventstore.Internal.Operation.ReadEventOperation
import Database.Eventstore.Internal.Operation.TransactionStartOperation
import Database.Eventstore.Internal.Operation.WriteEventsOperation

--------------------------------------------------------------------------------
type HostName = String
type Port     = Int

--------------------------------------------------------------------------------
-- ConnectionManager
--------------------------------------------------------------------------------
data ConnectionManager
    = ConnectionManager
      { mgrChan     :: TChan Msg
      , mgrSettings :: Settings
      , mgrThreadId :: ThreadId
      }

--------------------------------------------------------------------------------
msgQueue :: ConnectionManager -> Msg -> IO ()
msgQueue mgr msg = atomically $ writeTChan chan msg
  where
    chan = mgrChan mgr

--------------------------------------------------------------------------------
eventStoreConnect :: Settings -> HostName -> Port -> IO ConnectionManager
eventStoreConnect settings host port = do
    chan <- newTChanIO
    app  <- newProcessor settings chan host port
    tid  <- forkFinally (appProcess app) (\_ -> appFinalizer app)

    return $ ConnectionManager chan settings tid

--------------------------------------------------------------------------------
eventStoreShutdown :: ConnectionManager -> IO ()
eventStoreShutdown mgr = killThread tid
  where
    tid = mgrThreadId mgr

--------------------------------------------------------------------------------
eventStoreSendEvent :: ConnectionManager
                    -> Text             -- ^ Stream
                    -> ExpectedVersion
                    -> Event
                    -> IO (Async WriteResult)
eventStoreSendEvent mgr evt_stream exp_ver evt =
    eventStoreSendEvents mgr evt_stream exp_ver [evt]

--------------------------------------------------------------------------------
eventStoreSendEvents :: ConnectionManager
                     -> Text             -- ^ Stream
                     -> ExpectedVersion
                     -> [Event]
                     -> IO (Async WriteResult)
eventStoreSendEvents mgr evt_stream exp_ver evts = do
    (as, mvar) <- createAsync

    let op = writeEventsOperation settings mvar evt_stream exp_ver evts

    msgQueue mgr (RegisterOperation op)
    return as
  where
    settings = mgrSettings mgr

--------------------------------------------------------------------------------
eventStoreDeleteStream :: ConnectionManager
                       -> Text
                       -> ExpectedVersion
                       -> Maybe Bool       -- ^ Hard delete
                       -> IO (Async DeleteResult)
eventStoreDeleteStream mgr evt_stream exp_ver hard_del = do
    (as, mvar) <- createAsync

    let op = deleteStreamOperation settings mvar evt_stream exp_ver hard_del

    msgQueue mgr (RegisterOperation op)
    return as
  where
    settings = mgrSettings mgr

--------------------------------------------------------------------------------
eventStoreTransactionStart :: ConnectionManager
                           -> Text
                           -> ExpectedVersion
                           -> IO (Async Transaction)
eventStoreTransactionStart mgr evt_stream exp_ver = do
    (as, mvar) <- createAsync

    let op = transactionStartOperation settings chan mvar evt_stream exp_ver

    msgQueue mgr (RegisterOperation op)
    return as
  where
    chan     = mgrChan mgr
    settings = mgrSettings mgr

--------------------------------------------------------------------------------
eventStoreReadEvent :: ConnectionManager
                    -> Text
                    -> Int32
                    -> Bool
                    -> IO (Async ReadResult)
eventStoreReadEvent mgr stream_id evt_num res_link_tos = do
    (as, mvar) <- createAsync

    let op = readEventOperation settings mvar stream_id evt_num res_link_tos

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
