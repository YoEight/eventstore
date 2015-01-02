{-# LANGUAGE DeriveDataTypeable  #-}
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
    ( InternalException(..)
    , Processor(..)
    , DropReason(..)
    , Subscription(..)
    , NewSubscriptionCB
    , newProcessor
    ) where

--------------------------------------------------------------------------------
import Control.Concurrent
import Control.Exception
import Data.Monoid ((<>))
import Data.Typeable
import Data.Word
import System.IO
import Text.Printf

--------------------------------------------------------------------------------
import Control.Concurrent.Async
import Data.UUID
import FRP.Sodium
import Network
import System.Random

--------------------------------------------------------------------------------
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
      , processorNewSubcription :: NewSubscriptionCB
      }

--------------------------------------------------------------------------------
newProcessor :: Settings -> IO Processor
newProcessor sett = sync $ network sett

--------------------------------------------------------------------------------
-- Exception
--------------------------------------------------------------------------------
data ProcessorException
    = MaxAttempt HostName Int Int -- ^ Hostname Port MaxAttempt value
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception ProcessorException

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------
data State
    = Offline
      { _maxAttempt :: !Int }
    | Online
      { _uuidCon      :: !UUID
      , _maxAttempt   :: !Int
      , _packageCount :: !Int
      , _host         :: !HostName
      , _port         :: !Int
      , _cleanup      :: !(IO ())
      }

--------------------------------------------------------------------------------
initState :: Int -> State
initState max_at = Offline max_at

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
network :: Settings -> Reactive Processor
network sett = do
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

    push_sub <- subscriptionNetwork sett pushSend onReceived

    let stateE = fmap connected  onConnected    <>
                 fmap reconnected onReconnected <>
                 fmap received onReceived

    stateB <- accum (initState $ s_maxRetries sett) stateE

    let heartbeatP pkg = packageCmd pkg == heartbeatRequestCmd
        onlyHeartbeats = filterE heartbeatP onReceived

        con_snap   = snapshot connectSnapshot onConnect stateB
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

    _ <- listen con_snap $ \(ConnectionSnapshot max_a host port) ->
             connection push_recv_io
                        (push_con_io host port)
                        push_reco_io
                        onSend
                        max_a
                        host
                        port

    _ <- listen reco_snap $ \(ConnectionSnapshot max_a host port) ->
             connection push_recv_io
                        push_recod_io
                        push_reco_io
                        onSend
                        max_a
                        host
                        port

    _ <- listen clean_snap $ \(CleanupSnapshot finalizer) -> finalizer

    _ <- listen onlyHeartbeats $ \pkg ->
             push_send_io $ heartbeatResponsePackage (packageCorrelation pkg)

    let processor =
            Processor
            { processorConnect        = \h p -> sync $ pushConnect $ Connect h p
            , processorShutdown       = sync $ pushCleanup Cleanup
            , processorNewOperation   = \o -> sync $ push_new_op o
            , processorNewSubcription = push_sub
            }

    return processor

--------------------------------------------------------------------------------
-- Observer
--------------------------------------------------------------------------------
data ConnectionSnapshot
    = ConnectionSnapshot
      { _conMax  :: !Int
      , _conHost :: !HostName
      , _conPort :: !Int
      }

--------------------------------------------------------------------------------
connectSnapshot :: Connect -> State -> ConnectionSnapshot
connectSnapshot (Connect host port) s =
    ConnectionSnapshot
    { _conMax  = _maxAttempt s
    , _conHost = host
    , _conPort = port
    }

--------------------------------------------------------------------------------
reconnectDelay :: Int
reconnectDelay = 500000

--------------------------------------------------------------------------------
connection :: (Package -> IO ())
           -> (UUID -> IO () -> IO ())
           -> IO ()
           -> Event Package
           -> Int
           -> HostName
           -> Int
           -> IO ()
connection push_pkg push_con push_reco evt_pkg max_a host port = loop 1
  where
    loop att
        | max_a == att =
              throwIO $ MaxAttempt host port max_a
        | otherwise =
              catch (doConnect att) $ \(_ :: SomeException) -> do
                  threadDelay reconnectDelay
                  loop (att + 1)

    doConnect att = do
        printf "Connecting...Attempt %d\n" att
        hdl <- connectTo host (PortNumber $ fromIntegral port)
        hSetBuffering hdl NoBuffering

        uuid  <- randomIO
        chan  <- newChan
        as_rl <- async $ sync $ listen evt_pkg (writeChan chan)
        rid   <- forkFinally (readerThread push_pkg hdl) (recovering push_reco)
        wid   <- forkFinally (writerThread chan hdl) (recovering push_reco)

        push_con uuid $ do
            throwTo rid Stopped
            throwTo wid Stopped
            hClose hdl
            rel_w <- wait as_rl
            rel_w
            printf "Disconnected %s\n" (toString uuid)

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
    { _conMax  = _maxAttempt s
    , _conHost = _host s
    , _conPort = _port s
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
        Offline {}
            -> Online
               { _uuidCon      = uuid
               , _maxAttempt   = _maxAttempt s
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
