{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Processor
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Processor
    ( ConnectionException(..)
    , InternalException(..)
    , Cmd(..)
    , newProcessor
    ) where

--------------------------------------------------------------------------------
import Control.Concurrent
import Control.Exception
import Data.Functor (void)
import Data.Int
import Data.Monoid ((<>))
import Data.Word

--------------------------------------------------------------------------------
import Data.Text (Text)
import Data.UUID
import FRP.Sodium
import Network

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Connection
import Database.EventStore.Internal.Manager.Operation
import Database.EventStore.Internal.Manager.Subscription
import Database.EventStore.Internal.Packages
import Database.EventStore.Internal.Reader
import Database.EventStore.Internal.Types hiding (Event, newEvent)
import Database.EventStore.Internal.Util.Sodium
import Database.EventStore.Internal.Writer
import Database.EventStore.Logging (Log(..), InfoMessage (Disconnected))

--------------------------------------------------------------------------------
-- Processor
--------------------------------------------------------------------------------
type Result a  = a -> IO ()
type EResult a = Result (Either OperationException a)

--------------------------------------------------------------------------------
data Cmd
    = DoConnect HostName Int
    | DoShutdown
    | NewOperation OperationParams
    | NewSub Text Bool (Result (Subscription Regular))
    | CreatePersist Text Text PersistentSubscriptionSettings (EResult ())
    | UpdatePersist Text Text PersistentSubscriptionSettings (EResult ())
    | DeletePersist Text Text (EResult ())
    | ConnectPersist Text Text Int32 (Result (Subscription Persistent))

--------------------------------------------------------------------------------
type Processor = Cmd -> IO ()

--------------------------------------------------------------------------------
newProcessor :: Settings -> IO Processor
newProcessor sett = sync . network sett =<< newChan

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------
data State
    = Offline
    | Online
      { _uuidCon      :: !UUID
      , _packageCount :: !Int
      , _host         :: !HostName
      , _port         :: !Int
      , _cleanup      :: !(IO ())
      }

--------------------------------------------------------------------------------
-- Event
--------------------------------------------------------------------------------
data Connect     = Connect HostName Int
data Connected   = Connected HostName Int UUID (IO ())
data Cleanup     = Cleanup
data Reconnect   = Reconnect
data Reconnected = Reconnected UUID (IO ())

--------------------------------------------------------------------------------
heartbeatRequestCmd :: Word8
heartbeatRequestCmd = 0x01

--------------------------------------------------------------------------------
network :: Settings -> Chan Package -> Reactive Processor
network sett chan = do
    (onConnect, pushConnect)         <- newEvent
    (onConnected, pushConnected)     <- newEvent
    (onCleanup, pushCleanup)         <- newEvent
    (onReconnect, pushReconnect)     <- newEvent
    (onReconnected, pushReconnected) <- newEvent
    (onReceived, pushReceived)       <- newEvent
    (onSend, pushSend)               <- newEvent

    push_new_op <- operationNetwork sett
                                    pushSend
                                    (pushReconnect Reconnect)
                                    onReceived

    runSubCmd <- subscriptionNetwork sett pushSend onReceived

    let stateE = fmap connected  onConnected    <>
                 fmap reconnected onReconnected <>
                 fmap received onReceived

    stateB <- accum Offline stateE

    let heartbeatP pkg = packageCmd pkg == heartbeatRequestCmd
        onlyHeartbeats = filterE heartbeatP onReceived

        con_snap   = fmap connectSnapshot onConnect
        reco_snap  = snapshot reconnectSnapshot onReconnect stateB
        clean_snap = snapshot cleanupSnapshot onCleanup stateB

        full_reco c = do
            pushCleanup Cleanup
            pushReconnect c

        push_reco_io  = pushAsync full_reco Reconnect
        push_recv_io  = \pkg -> sync $ pushReceived pkg
        push_recod_io = pushAsync2 $ \u c -> pushReconnected $ Reconnected u c
        push_send_io  = pushAsync pushSend
        push_con_io   = pushAsync4 $ \h p u c ->
                                      pushConnected $ Connected h p u c

    _ <- listen con_snap $ \(ConnectionSnapshot host port) ->
             connection sett
                        chan
                        push_recv_io
                        (push_con_io host port)
                        push_reco_io
                        host
                        port

    _ <- listen reco_snap $ \(ConnectionSnapshot host port) ->
             connection sett
                        chan
                        push_recv_io
                        push_recod_io
                        push_reco_io
                        host
                        port

    _ <- listen clean_snap $ \(CleanupSnapshot finalizer) -> finalizer

    _ <- listen onlyHeartbeats $ \pkg ->
             push_send_io $ heartbeatResponsePackage (packageCorrelation pkg)

    let runCmd (DoConnect h p) =
            void $ forkIO $ sync $ pushConnect $ Connect h p
        runCmd DoShutdown =
            void $ forkIO $ sync $ pushCleanup Cleanup
        runCmd (NewOperation o) =
            void $ forkIO $ sync $ push_new_op o
        runCmd (NewSub stream tos cb) =
            runSubCmd (SubscribeTo (RegularSub stream tos) cb)
        runCmd (CreatePersist g s stgs cb) =
            runSubCmd (SubmitPersistAction g s (PersistCreate stgs) cb)
        runCmd (UpdatePersist g s stgs cb) =
            runSubCmd (SubmitPersistAction g s (PersistUpdate stgs) cb)
        runCmd (DeletePersist g s cb) =
            runSubCmd (SubmitPersistAction g s PersistDelete cb)
        runCmd (ConnectPersist g s b cb) =
            runSubCmd (SubscribeTo (PersistentSub g s b) cb)

    _ <- listen onSend (writeChan chan)

    return runCmd

--------------------------------------------------------------------------------
-- Observer
--------------------------------------------------------------------------------
data ConnectionSnapshot
    = ConnectionSnapshot
      { _conHost :: !HostName
      , _conPort :: !Int
      }

--------------------------------------------------------------------------------
connectSnapshot :: Connect -> ConnectionSnapshot
connectSnapshot (Connect host port) =
    ConnectionSnapshot
    { _conHost = host
    , _conPort = port
    }

--------------------------------------------------------------------------------
connection :: Settings
           -> Chan Package
           -> (Package -> IO ())
           -> (UUID -> IO () -> IO ())
           -> IO ()
           -> HostName
           -> Int
           -> IO ()
connection sett chan push_pkg push_con push_reco host port = do
    conn <- newConnection sett host port
    rid  <- forkFinally (readerThread sett push_pkg conn) (recovering push_reco)
    wid  <- forkFinally (writerThread chan conn) (recovering push_reco)
    push_con (connUUID conn) $ do
        throwTo rid Stopped
        throwTo wid Stopped
        connClose conn
        _settingsLog sett (Info $ Disconnected $ connUUID conn)

--------------------------------------------------------------------------------
recovering :: IO () -> Either SomeException () -> IO ()
recovering recover (Left some_ex) = do
    case fromException some_ex of
        Just e ->
            case e of
                ConnectionClosedByServer
                    -> recover
                Stopped
                    -> return ()
        _ -> recover
recovering _ _ = return ()

--------------------------------------------------------------------------------
reconnectSnapshot :: Reconnect -> State -> ConnectionSnapshot
reconnectSnapshot _ s =
    ConnectionSnapshot
    { _conHost  = _host s
    , _conPort  = _port s
    }

--------------------------------------------------------------------------------
newtype CleanupSnapshot = CleanupSnapshot (IO ())

--------------------------------------------------------------------------------
cleanupSnapshot :: Cleanup -> State -> CleanupSnapshot
cleanupSnapshot _ s =
    case s of
        Offline {}
            -> CleanupSnapshot (return ())
        Online {}
            -> CleanupSnapshot $ _cleanup s

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------
connected :: Connected -> State -> State
connected (Connected host port uuid cl) s =
    case s of
        Offline
            -> Online
               { _uuidCon      = uuid
               , _packageCount = 0
               , _host         = host
               , _port         = port
               , _cleanup      = cl
               }
        _ -> s

--------------------------------------------------------------------------------
reconnected :: Reconnected -> State -> State
reconnected (Reconnected uuid cl) s =
    case s of
        Online {}
            -> s { _uuidCon = uuid
                 , _cleanup = cl
                 }
        _ -> s

--------------------------------------------------------------------------------
received :: Package -> State -> State
received _ s =
    case s of
        Online {}
            -> let cnt = _packageCount s in s { _packageCount = cnt + 1 }
        _ -> s
