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
      -- * Event
    , createEvent
    , withJson
    , withJsonAndMetadata
      -- * Connection manager
    , defaultSettings
    , eventStoreConnect
    , eventStoreSendEvent
    , eventStoreSendEvents
    , eventStoreShutdown
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

    mvar <- atomically newEmptyTMVar
    as   <- async $ atomically $ do
        res <- readTMVar mvar
        either throwSTM return res

    let op = writeEventsOperation settings mvar evt_stream exp_ver evts
    msgQueue mgr (RegisterOperation op)

    return as
  where
    settings = mgrSettings mgr
    require_master = _requireMaster $ mgrSettings mgr
    exp_ver_int32  = expVersionInt32 exp_ver
