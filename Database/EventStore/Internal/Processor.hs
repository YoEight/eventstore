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
    , Processor(..)
    , newProcessor
    ) where

--------------------------------------------------------------------------------
import Control.Concurrent
import Control.Exception
import Data.Functor (void)
import Data.Int
import Data.Monoid ((<>))
import Data.Word
import Text.Printf

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

--------------------------------------------------------------------------------
-- Processor
--------------------------------------------------------------------------------
data Processor
    = Processor
      { processorConnect        :: HostName -> Int -> IO ()
      , processorShutdown       :: IO ()
      , processorNewOperation   :: OperationParams -> IO ()
      , processorNewSubcription :: (Subscription Regular -> IO ())
                                -> Text
                                -> Bool
                                -> IO ()
      , processorCreatePersistent :: (Either OperationException () -> IO ())
                                  -> Text
                                  -> Text
                                  -> PersistentSubscriptionSettings
                                  -> IO ()
      , processorUpdatePersistent :: (Either OperationException () -> IO ())
                                  -> Text
                                  -> Text
                                  -> PersistentSubscriptionSettings
                                  -> IO ()
      , processorDeletePersistent :: (Either OperationException () -> IO ())
                                  -> Text
                                  -> Text
                                  -> IO ()
      , processorConnectPersist :: (Subscription Persistent -> IO ())
                                -> Text
                                -> Text
                                -> Int32
                                -> IO ()
      }

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

    let processor =
            Processor
            { processorConnect = \h p -> void $ forkIO $
                                         sync $ pushConnect $ Connect h p
            , processorShutdown = void $ forkIO $ sync $
                                        pushCleanup Cleanup
            , processorNewOperation = \o -> void $ forkIO $
                                            sync $ push_new_op o
            , processorNewSubcription = \cb stream tos ->
                runSubCmd (SubscribeTo (RegularSub stream tos) cb)
            , processorCreatePersistent = \cb group stream stgs ->
                runSubCmd $ SubmitPersistAction group
                                                stream
                                                (PersistCreate stgs)
                                                cb
            , processorUpdatePersistent = \cb group stream stgs ->
                runSubCmd $ SubmitPersistAction group
                                                stream
                                                (PersistUpdate stgs)
                                                cb
            , processorDeletePersistent = \cb group stream ->
                runSubCmd $ SubmitPersistAction group stream PersistDelete cb
            , processorConnectPersist = \cb group stream buf ->
                runSubCmd (SubscribeTo (PersistentSub group stream buf) cb)
            }

    _ <- listen onSend (writeChan chan)

    return processor

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
    rid  <- forkFinally (readerThread push_pkg conn) (recovering push_reco)
    wid  <- forkFinally (writerThread chan conn) (recovering push_reco)
    push_con (connUUID conn) $ do
        throwTo rid Stopped
        throwTo wid Stopped
        connClose conn
        printf "Disconnected %s\n" (toString $ connUUID conn)

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
