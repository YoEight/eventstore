{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Execution.Production
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
-- Production execution model. It's striving for robustness. The model consists
-- on 4 threads. The Reader thread that reads 'Package' from the connection, the
-- Runner thread which executes finalizers submitted by the user (typically what
-- to do on operation completion or when a event has arrived for a subscription)
-- , the writer thread that sends 'Package' to the server, and the Manager
-- thread that handles requests coming both from the user and the Reader
-- thread. If the Reader or Runner threads die, it will be restarted by the
-- Manager thread if the connection hasn't been closed by user in the meantime.
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Execution.Production
    ( Production
    , newExecutionModel
    , pushOperation
    , shutdownExecutionModel
    , pushConnectStream
    , pushConnectPersist
    , pushCreatePersist
    , pushUpdatePersist
    , pushDeletePersist
    , pushAckPersist
    , pushNakPersist
    , pushUnsubscribe
    , prodWaitTillClosed
    ) where

--------------------------------------------------------------------------------
import Control.Exception (AsyncException(..), asyncExceptionFromException)
import Control.Monad.Fix
import Data.Int

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Connection
import Database.EventStore.Internal.Discovery
import Database.EventStore.Internal.Generator
import Database.EventStore.Internal.Manager.Subscription.Driver hiding
    ( submitPackage
    , unsubscribe
    , ackPersist
    , nakPersist
    , abort
    )
import Database.EventStore.Internal.Manager.Subscription.Model
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Processor
import Database.EventStore.Internal.Types
import Database.EventStore.Logging

--------------------------------------------------------------------------------
data Worker
    = Reader ThreadId
    | Runner ThreadId
    | Writer ThreadId
    deriving Show

--------------------------------------------------------------------------------
-- | Used to determine if we hit the end of the queue.
data Slot a = Slot !a | End

--------------------------------------------------------------------------------
-- | A 'TQueue' that can be cycled.
newtype CycleQueue a = CycleQueue (TQueue (Slot a))

--------------------------------------------------------------------------------
-- | Creates an empty 'CycleQueue'.
newCycleQueue :: IO (CycleQueue a)
newCycleQueue = fmap CycleQueue newTQueueIO

--------------------------------------------------------------------------------
-- | Gets an element from the 'CycleQueue'.
readCycleQueue :: CycleQueue a -> STM a
readCycleQueue (CycleQueue q) = do
    Slot a <- readTQueue q
    return a

--------------------------------------------------------------------------------
-- | Writes an element to the 'CycleQueue'.
writeCycleQueue :: CycleQueue a -> a -> STM ()
writeCycleQueue (CycleQueue q) a = writeTQueue q (Slot a)

--------------------------------------------------------------------------------
-- | Empties a 'CycleQueue'.
emptyCycleQueue :: CycleQueue a -> STM ()
emptyCycleQueue (CycleQueue q) = writeTQueue q End >> go
  where
    go = do
        s <- readTQueue q
        case s of
            End -> return ()
            _   -> go

--------------------------------------------------------------------------------
-- | Updates a 'CycleQueue'.
updateCycleQueue :: CycleQueue a -> (a -> STM (Maybe a)) -> STM ()
updateCycleQueue (CycleQueue q) k = writeTQueue q End >> go
  where
    go = do
        s <- readTQueue q
        case s of
            End    -> return ()
            Slot a -> do
                r <- k a
                case r of
                    Nothing -> go
                    Just a' -> writeTQueue q (Slot a') >> go

--------------------------------------------------------------------------------
-- | Indicates if a 'CycleQueue' is empty.
isEmptyCycleQueue :: CycleQueue a -> STM Bool
isEmptyCycleQueue (CycleQueue q) = isEmptyTQueue q

--------------------------------------------------------------------------------
wkUpdState :: Worker -> State -> State
wkUpdState (Reader tid) s = s { _reader = Just tid }
wkUpdState (Runner tid) s = s { _runner = Just tid }
wkUpdState (Writer tid) s = s { _writer = Just tid }

--------------------------------------------------------------------------------
-- | Holds the execution model state.
data Production =
    Prod
    { _submit :: TVar (Msg -> IO ())
      -- ^ The action to call when pushing new command.
    , _waitClosed :: STM ()
      -- ^ Action that attests the execution model has been closed successfully.
      --   It doesn't mean the execution model hasn't been shutdown because of
      --   some random exception.
    }

--------------------------------------------------------------------------------
-- | Main execution environment used among different transitions.
data Env =
    Env
    { _setts :: Settings
      -- ^ Global settings reference.
    , _queue :: CycleQueue Msg
      -- ^ That queue ties the user, the reader thread and the manager thread.
      --   The user and the reader push new messages onto the queue while the
      --   manager dequeue and handles one message at the time.
    , _pkgQueue :: CycleQueue Package
      -- ^ That queue ties the writer thread with the manager thread. The writer
      --   dequeue packages from that queue and sends those to the server. While
      --   the manager pushes new packages on every new submitted operation.
    , _jobQueue :: CycleQueue Job
      -- ^ That queue ties the runner thread with the manager thread. The runner
      --   dequeues IO action from it while the manager pushes new command
      --   finalizers as those arrived.
    , _state :: TVar State
      -- ^ Holds manager thread state.
    , _nextSubmit :: TVar (Msg -> IO ())
      -- ^ Indicates the action to call in order to push new commands.
    , _connRef :: IORef InternalConnection
      -- ^ Connection to the server.
    , _disposed :: TMVar ()
      -- ^ Indicates when the production execution model has been shutdown and
      --   disposed any ongoing operations.
    }

--------------------------------------------------------------------------------
data Msg
    = Stopped Worker SomeException
    | Arrived Package
    | Shutdown
    | forall a.
      NewOperation (Either OperationError a -> IO ()) (Operation a)
    | ConnectStream (SubConnectEvent -> IO ()) Text Bool
    | ConnectPersist (SubConnectEvent -> IO ()) Text Text Int32
    | Unsubscribe Running
    | CreatePersist (Either PersistActionException ConfirmedAction -> IO ())
          Text Text PersistentSubscriptionSettings
    | UpdatePersist (Either PersistActionException ConfirmedAction -> IO ())
          Text Text PersistentSubscriptionSettings
    | DeletePersist (Either PersistActionException ConfirmedAction -> IO ())
          Text Text
    | AckPersist Running [UUID]
    | NakPersist Running NakAction (Maybe Text) [UUID]

--------------------------------------------------------------------------------
pushCmd :: Production -> Msg -> IO ()
pushCmd (Prod _sender _) msg = do
    push <- readTVarIO _sender
    push msg

--------------------------------------------------------------------------------
-- | Asks to shutdown the connection to the server asynchronously.
shutdownExecutionModel :: Production -> IO ()
shutdownExecutionModel prod = pushCmd prod Shutdown

--------------------------------------------------------------------------------
-- | Pushes a new 'Operation' asynchronously.
pushOperation :: Production
             -> (Either OperationError a -> IO ())
             -> Operation a
             -> IO ()
pushOperation prod k op = pushCmd prod (NewOperation k op)

--------------------------------------------------------------------------------
-- | Subscribes to a regular stream.
pushConnectStream :: Production
                  -> (SubConnectEvent -> IO ())
                  -> Text
                  -> Bool
                  -> IO ()
pushConnectStream prod k n tos = pushCmd prod (ConnectStream k n tos)

--------------------------------------------------------------------------------
-- | Subscribes to a persistent subscription.
pushConnectPersist :: Production
                   -> (SubConnectEvent -> IO ())
                   -> Text
                   -> Text
                   -> Int32
                   -> IO ()
pushConnectPersist prod k g n buf = pushCmd prod (ConnectPersist k g n buf)

--------------------------------------------------------------------------------
-- | Creates a persistent subscription.
pushCreatePersist :: Production
                  -> (Either PersistActionException ConfirmedAction -> IO ())
                  -> Text
                  -> Text
                  -> PersistentSubscriptionSettings
                  -> IO ()
pushCreatePersist prod k g n setts = pushCmd prod (CreatePersist k g n setts)

--------------------------------------------------------------------------------
-- | Updates a persistent subscription.
pushUpdatePersist :: Production
                  -> (Either PersistActionException ConfirmedAction -> IO ())
                  -> Text
                  -> Text
                  -> PersistentSubscriptionSettings
                  -> IO ()
pushUpdatePersist prod k g n setts = pushCmd prod (UpdatePersist k g n setts)

--------------------------------------------------------------------------------
-- | Deletes a persistent subscription.
pushDeletePersist :: Production
                  -> (Either PersistActionException ConfirmedAction -> IO ())
                  -> Text
                  -> Text
                  -> IO ()
pushDeletePersist prod k g n = pushCmd prod (DeletePersist k g n)

--------------------------------------------------------------------------------
-- | Acknowledges a set of events has been successfully handled.
pushAckPersist :: Production -> Running -> [UUID] -> IO ()
pushAckPersist prod run evts = pushCmd prod (AckPersist run evts)

--------------------------------------------------------------------------------
-- | Acknowledges a set of events hasn't been handled successfully.
pushNakPersist :: Production
               -> Running
               -> NakAction
               -> Maybe Text
               -> [UUID]
               -> IO ()
pushNakPersist prod run act res evts =
    pushCmd prod (NakPersist run act res evts)

--------------------------------------------------------------------------------
-- | Unsubscribe from a subscription.
pushUnsubscribe :: Production -> Running -> IO ()
pushUnsubscribe prod r = pushCmd prod (Unsubscribe r)

--------------------------------------------------------------------------------
-- | Waits the execution model to close properly.
prodWaitTillClosed :: Production -> IO ()
prodWaitTillClosed (Prod _ disposed) = atomically disposed

--------------------------------------------------------------------------------
newtype Job = Job (IO ())

--------------------------------------------------------------------------------
-- Internal Production state.
--------------------------------------------------------------------------------
data State =
    State
    { _proc   :: !(Processor (IO ()))
    , _reader :: !(Maybe ThreadId)
    , _runner :: !(Maybe ThreadId)
    , _writer :: !(Maybe ThreadId)
    }

--------------------------------------------------------------------------------
emptyState :: Settings -> Generator -> State
emptyState setts gen = State (newProcessor setts gen) Nothing Nothing Nothing

--------------------------------------------------------------------------------
updateProc :: Processor (IO ()) -> State -> State
updateProc p s = s { _proc = p }

--------------------------------------------------------------------------------
-- | Reader thread. Keeps reading 'Package' from the server.
reader :: Settings -> CycleQueue Msg -> InternalConnection -> IO ()
reader sett queue c = forever $ do
    pkg <- connRecv c
    let cmd  = packageCmd pkg
        uuid = packageCorrelation pkg

    atomically $ writeCycleQueue queue (Arrived pkg)
    _settingsLog sett $ Info $ PackageReceived cmd uuid

--------------------------------------------------------------------------------
-- | Writer thread, writes incoming 'Package's
--------------------------------------------------------------------------------
writer :: Settings -> CycleQueue Package -> InternalConnection -> IO ()
writer setts pkg_queue conn = forever $ do
    pkg <- atomically $ readCycleQueue pkg_queue
    connSend conn pkg
    let cmd  = packageCmd pkg
        uuid = packageCorrelation pkg

    _settingsLog setts $ Info $ PackageSent cmd uuid

--------------------------------------------------------------------------------
-- Runner thread. Keeps running job comming from the Manager thread.
--------------------------------------------------------------------------------
runner :: CycleQueue Job -> IO ()
runner job_queue = forever $ do
    Job j <- atomically $ readCycleQueue job_queue
    j

--------------------------------------------------------------------------------
-- | Spawns a new thread worker.
spawn :: Env -> (ThreadId -> Worker) -> IO Worker
spawn Env{..} mk = do
    conn <- readIORef _connRef
    tid  <- mfix $ \tid ->
        let worker = mk tid
            action =
                case worker of
                    Reader _ -> reader _setts _queue conn
                    Runner _ -> runner _jobQueue
                    Writer _ -> writer _setts _pkgQueue conn in
        forkFinally action $ \r ->
            case r of
                Left e ->
                    case asyncExceptionFromException e of
                        Just ThreadKilled -> return ()
                        _ ->  atomically $ writeCycleQueue _queue
                                         $ Stopped worker e
                _      -> return ()
    return $ mk tid

--------------------------------------------------------------------------------
-- | Loops over a 'Processor''s 'Transition' state machine, returning an updated
--   'Processor' model at the end.
runTransition :: Env -> Transition (IO ()) -> STM (Processor (IO ()))
runTransition Env{..} = go
  where
    go (Produce j nxt) = do
        let job = Job j
        writeCycleQueue _jobQueue job
        go nxt
    go (Transmit pkg nxt) = do
        writeCycleQueue _pkgQueue pkg
        go nxt
    go (Await new_proc) = return new_proc

--------------------------------------------------------------------------------
-- | First execution mode. It spawns initial reader, runner and writer threads.
--   Then it switches to 'cruising' mode.
bootstrap :: Env -> IO ()
bootstrap env@Env{..} = do
    rew <- spawn env Reader
    ruw <- spawn env Runner
    wrw <- spawn env Writer
    let _F = wkUpdState rew .
             wkUpdState ruw .
             wkUpdState wrw
    atomically $ modifyTVar' _state _F
    cruising env

--------------------------------------------------------------------------------
-- | Crusing execution mode. Reads and handle message coming from the channel as
--   those are arrived. That mode is used when the connection to the server is
--   still live. We might have deconnection once in a while but at the end, if
--   we managed to reconnect to it, we consider everything is fine.
cruising :: Env -> IO ()
cruising env@Env{..} = do
    msg <- atomically $ readCycleQueue _queue
    s   <- readTVarIO _state
    case msg of
        Stopped _ e -> throwIO e
        Arrived pkg -> do
            let sm = submitPackage pkg $ _proc s
            atomically $ do
                new_proc <- runTransition env sm
                modifyTVar' _state $ updateProc new_proc
            cruising env
        Shutdown -> throwIO ClosedConnection
        NewOperation k op -> do
            let sm = newOperation k op $ _proc s
            atomically $ do
                new_proc <- runTransition env sm
                modifyTVar' _state $ updateProc new_proc
            cruising env
        ConnectStream k n tos -> do
            let sm = connectRegularStream k n tos $ _proc s
            atomically $ do
                new_proc <- runTransition env sm
                modifyTVar' _state $ updateProc new_proc
            cruising env
        ConnectPersist k g n b -> do
            let sm = connectPersistent k g n b $ _proc s
            atomically $ do
                new_proc <- runTransition env sm
                modifyTVar' _state $ updateProc new_proc
            cruising env
        Unsubscribe r -> do
            let sm = unsubscribe r $ _proc s
            atomically $ do
                new_proc <- runTransition env sm
                modifyTVar' _state $ updateProc new_proc
            cruising env
        CreatePersist k g n psetts -> do
            let sm = createPersistent k g n psetts $ _proc s
            atomically $ do
                new_proc <- runTransition env sm
                modifyTVar' _state $ updateProc new_proc
            cruising env
        UpdatePersist k g n psetts -> do
            let sm = updatePersistent k g n psetts $ _proc s
            atomically $ do
                new_proc <- runTransition env sm
                modifyTVar' _state $ updateProc new_proc
            cruising env
        DeletePersist k g n -> do
            let sm = deletePersistent k g n $ _proc s
            atomically $ do
                new_proc <- runTransition env sm
                modifyTVar' _state $ updateProc new_proc
            cruising env
        AckPersist run evts -> do
            let sm = ackPersist (return ()) run evts $ _proc s
            atomically $ do
                new_proc <- runTransition env sm
                modifyTVar' _state $ updateProc new_proc
            cruising env
        NakPersist run act res evts -> do
            let sm = nakPersist (return ()) run act res evts $ _proc s
            atomically $ do
                new_proc <- runTransition env sm
                modifyTVar' _state $ updateProc new_proc
            cruising env

--------------------------------------------------------------------------------
-- | That mode is triggered either because the user asks to shutdown the
--   connection or because the connection to server has been dropped and we
--   can't reconnect.
closing :: Env -> IO ()
closing env@Env{..} = do
    State _ retid rutid  wutid <- readTVarIO _state
    -- We kill reader and writer threads to avoid in fly package in the cleaning
    -- phase.
    traverse_ killThread retid
    traverse_ killThread wutid

    -- Discards every 'Package' that was about to be sent.
    atomically $ emptyCycleQueue _pkgQueue

    -- Takes care of 'Package's that have already arrived. Just in case those
    -- are completing or moving forward ongoing operations. Every ongoing
    -- request is kept for later reconnection. Some transient operations like
    -- Ack, Nak or Unsubscribe are just discard.
    atomically $ updateCycleQueue _queue $ \nxt ->
        case nxt of
            Arrived pkg -> do
                s <- readTVar _state
                let sm = submitPackage pkg $ _proc s
                nxt_proc <- runTransition env sm
                modifyTVar' _state $ updateProc nxt_proc
                return Nothing
            Shutdown -> return Nothing
            AckPersist _ _ -> return Nothing
            NakPersist _ _ _ _ -> return Nothing
            Unsubscribe _ -> return Nothing
            Stopped _ _ -> return Nothing
            _ -> return $ Just nxt

    -- If the connection is already closed, it will throw an exception. We just
    -- make sure it doesn't interfere with the cleaning process.
    conn <- readIORef _connRef
    _    <- try $ connClose conn :: (IO (Either ConnectionException ()))
    atomically $ do
        s <- readTVar _state
        _ <- runTransition env $ abort $ _proc s
        return ()

    -- Waits the runner thread to deal with its jobs list.
    atomically $ do
        end <- isEmptyCycleQueue _jobQueue
        unless end retrySTM

    traverse_ killThread rutid

--------------------------------------------------------------------------------
raiseException :: Exception e => e -> Msg -> IO ()
raiseException e _ = throwIO e

--------------------------------------------------------------------------------
-- | Main Production execution model entry point.
newExecutionModel :: Settings -> Discovery -> IO Production
newExecutionModel setts disc = do
    gen       <- newGenerator
    queue     <- newCycleQueue
    pkg_queue <- newCycleQueue
    job_queue <- newCycleQueue
    conn      <- newConnection setts disc
    conn_ref  <- newIORef conn
    var       <- newTVarIO $ emptyState setts gen
    nxt_sub   <- newTVarIO (atomically . writeCycleQueue queue)
    disposed  <- newEmptyTMVarIO
    let env = Env setts queue pkg_queue job_queue var nxt_sub conn_ref disposed
        handler res = do
            closing env
            case res of
                Left e -> do
                    _settingsLog setts (Error $ UnexpectedException e)
                    case fromException e of
                        Just (_ :: ConnectionException) -> atomically $ do
                            writeTVar nxt_sub (raiseException e)
                            putTMVar disposed ()
                        _ -> do new_conn <- newConnection setts disc
                                writeIORef conn_ref new_conn
                                _ <- forkFinally (bootstrap env) handler
                                return ()

                _ -> atomically $ putTMVar disposed ()
    _ <- forkFinally (bootstrap env) handler
    return $ Prod nxt_sub $ do
        closed <- connIsClosed conn
        unless closed retrySTM
        readTMVar disposed
