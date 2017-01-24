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
    , Cmd(..)
    , pushCmd
    , newExecutionModel
    , prodWaitTillClosed
    , pushShutdown
    , pushForceReconnect
    ) where

--------------------------------------------------------------------------------
import Control.Exception (AsyncException(..), asyncExceptionFromException)
import Control.Monad.Fix
import Data.Int

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Cmd
import Database.EventStore.Internal.Connection
import Database.EventStore.Internal.Discovery
import Database.EventStore.Internal.EndPoint
import Database.EventStore.Internal.Execution.TCQueue
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
    , _queue :: TCQueue Msg
      -- ^ That queue ties the user, the reader thread and the manager thread.
      --   The user and the reader push new messages onto the queue while the
      --   manager dequeue and handles one message at the time.
    , _pkgQueue :: TCQueue Package
      -- ^ That queue ties the writer thread with the manager thread. The writer
      --   dequeue packages from that queue and sends those to the server. While
      --   the manager pushes new packages on every new submitted operation.
    , _jobQueue :: TCQueue Job
      -- ^ That queue ties the runner thread with the manager thread. The runner
      --   dequeues IO action from it while the manager pushes new command
      --   finalizers as those arrived.
    , _state :: TVar State
      -- ^ Holds manager thread state.
    , _nextSubmit :: TVar (Msg -> IO ())
      -- ^ Indicates the action to call in order to push new commands.
    , _connVar :: TVar InternalConnection
      -- ^ Connection to the server.
    , _disposed :: TMVar ()
      -- ^ Indicates when the production execution model has been shutdown and
      --   disposed any ongoing operations.
    }

--------------------------------------------------------------------------------
newEnv :: Settings -> Discovery -> IO Env
newEnv setts disc = mfix $ \env ->
    Env setts <$> newTCQueue
              <*> newTCQueue
              <*> newTCQueue
              <*> newState
              <*> newTVarIO (atomically . writeTCQueue (_queue env))
              <*> newConn
              <*> newEmptyTMVarIO
  where
    newState = do
        gen <- newGenerator
        newTVarIO $ emptyState setts gen

    newConn = newConnection setts disc >>= newTVarIO

--------------------------------------------------------------------------------
data Msg
    = Stopped SomeException
      -- One of a worker thread has died.
    | Arrived Package
      -- 'Package' sent by the server.
    | Shutdown
      -- Asks to shutdown the connection to the server asynchronously.
    | ForceReconnect NodeEndPoints
      -- Force to reconnect on given node points.
    | UserCmd Cmd

--------------------------------------------------------------------------------
pushMsg :: Production -> Msg -> IO ()
pushMsg (Prod _sender _) msg = do
    push <- readTVarIO _sender
    push msg

--------------------------------------------------------------------------------
pushCmd :: Production -> Cmd -> IO ()
pushCmd prod = pushMsg prod . UserCmd

--------------------------------------------------------------------------------
pushShutdown :: Production -> IO ()
pushShutdown prod = pushMsg prod Shutdown

--------------------------------------------------------------------------------
pushForceReconnect :: Production -> NodeEndPoints -> IO ()
pushForceReconnect prod = pushMsg prod . ForceReconnect

--------------------------------------------------------------------------------
-- | Waits the execution model to close properly.
prodWaitTillClosed :: Production -> IO ()
prodWaitTillClosed (Prod _ disposed) = atomically disposed

--------------------------------------------------------------------------------
-- | an 'IO' action submitted by the 'RunnerWorker'.
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
data WorkerType
  = ReaderWorker
  | RunnerWorker
  | WriterWorker

--------------------------------------------------------------------------------
emptyState :: Settings -> Generator -> State
emptyState setts gen = State (newProcessor setts gen) Nothing Nothing Nothing

--------------------------------------------------------------------------------
updateProc :: Processor (IO ()) -> State -> State
updateProc p s = s { _proc = p }

--------------------------------------------------------------------------------
-- | Reader thread. Keeps reading 'Package' from the server.
reader :: Settings -> TCQueue Msg -> InternalConnection -> IO ()
reader sett queue c = forever $ do
    pkg <- connRecv c
    let cmd  = packageCmd pkg
        uuid = packageCorrelation pkg

    atomically $ writeTCQueue queue (Arrived pkg)
    _settingsLog sett $ Info $ PackageReceived cmd uuid

--------------------------------------------------------------------------------
-- | Writer thread, writes incoming 'Package's
--------------------------------------------------------------------------------
writer :: Settings -> TCQueue Package -> InternalConnection -> IO ()
writer setts pkg_queue conn = forever $ do
    pkg <- atomically $ readTCQueue pkg_queue
    connSend conn pkg
    let cmd  = packageCmd pkg
        uuid = packageCorrelation pkg

    _settingsLog setts $ Info $ PackageSent cmd uuid

--------------------------------------------------------------------------------
-- Runner thread. Keeps running job comming from the Manager thread.
--------------------------------------------------------------------------------
runner :: TCQueue Job -> IO ()
runner job_queue = forever $ do
    Job j <- atomically $ readTCQueue job_queue
    j

--------------------------------------------------------------------------------
-- | Spawns a new thread worker.
spawn :: Env -> WorkerType -> IO ()
spawn Env{..} typ = do
    conn <- readTVarIO _connVar
    let action =
           case typ of
               ReaderWorker -> reader _setts _queue conn
               RunnerWorker -> runner _jobQueue
               WriterWorker -> writer _setts _pkgQueue conn
    _ <- forkFinally action $ \r ->
        case r of
            Left e ->
                case asyncExceptionFromException e of
                  Just ThreadKilled -> return ()
                  _ ->  atomically $ writeTCQueue _queue
                                   $ Stopped e
            _      -> return ()
    return ()

--------------------------------------------------------------------------------
-- | Loops over a 'Processor''s 'Transition' state machine, returning an updated
--   'Processor' model at the end.
runTransition :: Env -> Transition (IO ()) -> STM (Processor (IO ()))
runTransition Env{..} = go
  where
    go (Produce j nxt) = do
        let job = Job j
        writeTCQueue _jobQueue job
        go nxt
    go (Transmit pkg nxt) = do
        writeTCQueue _pkgQueue pkg
        go nxt
    go (Await new_proc) = return new_proc
    go (ForceReconnectCmd node nxt) = do
        writeTCQueue _queue (ForceReconnect node)
        go nxt

--------------------------------------------------------------------------------
-- | First execution mode. It spawns initial reader, runner and writer threads.
--   Then it switches to 'cruising' mode.
bootstrap :: Env -> IO ()
bootstrap env@Env{..} = do
    spawn env ReaderWorker
    spawn env RunnerWorker
    spawn env WriterWorker

    cruising env

--------------------------------------------------------------------------------
data ForceReconnectException = ForceReconnectionException NodeEndPoints
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception ForceReconnectException

--------------------------------------------------------------------------------
-- | Crusing execution mode. Reads and handle message coming from the channel as
--   those are arrived. That mode is used when the connection to the server is
--   still live. We might have deconnection once in a while but at the end, if
--   we managed to reconnect to it, we consider everything is fine.
cruising :: Env -> IO ()
cruising env@Env{..} = forever $ do
    msg <- atomically $ readTCQueue _queue
    s   <- readTVarIO _state
    case msg of
        Stopped e -> throwIO e
        Arrived pkg -> do
            let sm = submitPackage pkg $ _proc s
            atomically $ do
                new_proc <- runTransition env sm
                modifyTVar' _state $ updateProc new_proc
        ForceReconnect node -> throwIO $ ForceReconnectionException node
        Shutdown -> throwIO ClosedConnection
        UserCmd cmd -> executeCmd env s cmd

--------------------------------------------------------------------------------
-- | Executes command issued by the user.
executeCmd :: Env -> State -> Cmd -> IO ()
executeCmd env@Env{..} s cmd = do
  case cmd of
      NewOperation k op -> do
          let sm = newOperation k op $ _proc s
          atomically $ do
              new_proc <- runTransition env sm
              modifyTVar' _state $ updateProc new_proc
      SubCmd subCmd ->
          case subCmd of
              ConnectStream k n tos -> do
                  let sm = connectRegularStream k n tos $ _proc s
                  atomically $ do
                      new_proc <- runTransition env sm
                      modifyTVar' _state $ updateProc new_proc
              ConnectPersist k g n b -> do
                  let sm = connectPersistent k g n b $ _proc s
                  atomically $ do
                      new_proc <- runTransition env sm
                      modifyTVar' _state $ updateProc new_proc
              Unsubscribe r -> do
                  let sm = unsubscribe r $ _proc s
                  atomically $ do
                      new_proc <- runTransition env sm
                      modifyTVar' _state $ updateProc new_proc
              CreatePersist k g n psetts -> do
                  let sm = createPersistent k g n psetts $ _proc s
                  atomically $ do
                      new_proc <- runTransition env sm
                      modifyTVar' _state $ updateProc new_proc
              UpdatePersist k g n psetts -> do
                  let sm = updatePersistent k g n psetts $ _proc s
                  atomically $ do
                      new_proc <- runTransition env sm
                      modifyTVar' _state $ updateProc new_proc
              DeletePersist k g n -> do
                  let sm = deletePersistent k g n $ _proc s
                  atomically $ do
                      new_proc <- runTransition env sm
                      modifyTVar' _state $ updateProc new_proc
              AckPersist run evts -> do
                  let sm = ackPersist (return ()) run evts $ _proc s
                  atomically $ do
                      new_proc <- runTransition env sm
                      modifyTVar' _state $ updateProc new_proc
              NakPersist run act res evts -> do
                  let sm = nakPersist (return ()) run act res evts $ _proc s
                  atomically $ do
                      new_proc <- runTransition env sm
                      modifyTVar' _state $ updateProc new_proc

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
    atomically $ clearTCQueue _pkgQueue

    -- Takes care of 'Package's that have already arrived. Just in case those
    -- are completing or moving forward ongoing operations. Every ongoing
    -- request is kept for later reconnection. Some transient operations like
    -- Ack, Nak or Unsubscribe are just discard.
    atomically $ updateTCQueue _queue $ \nxt ->
        case nxt of
            Arrived pkg -> do
                s <- readTVar _state
                let sm = submitPackage pkg $ _proc s
                nxt_proc <- runTransition env sm
                modifyTVar' _state $ updateProc nxt_proc
                return Nothing
            Shutdown -> return Nothing
            Stopped _ -> return Nothing
            UserCmd cmd ->
                case cmd of
                    SubCmd subCmd ->
                        case subCmd of
                            AckPersist{} -> return Nothing
                            NakPersist{} -> return Nothing
                            Unsubscribe{} -> return Nothing
                            _ -> return $ Just nxt
                    _ -> return $ Just nxt
            ForceReconnect{} -> return $ Just nxt

    -- If the connection is already closed, it will throw an exception. We just
    -- make sure it doesn't interfere with the cleaning process.
    conn <- readTVarIO _connVar
    _    <- try $ connClose conn :: (IO (Either ConnectionException ()))
    atomically $ do
        s <- readTVar _state
        _ <- runTransition env $ abort $ _proc s
        return ()

    -- Waits the runner thread to deal with its jobs list.
    atomically $ do
        end <- isEmptyTCQueue _jobQueue
        unless end retrySTM

    traverse_ killThread rutid

--------------------------------------------------------------------------------
raiseException :: Exception e => e -> Msg -> IO ()
raiseException e _ = throwIO e

--------------------------------------------------------------------------------
handles :: SomeException -> [Handler IO a] -> IO a
handles e [] = throwIO e
handles e (Handler k:hs) =
    case fromException e of
        Just t -> k t
        _ -> handles e hs

--------------------------------------------------------------------------------
-- | Main Production execution model entry point.
newExecutionModel :: Settings -> Discovery -> IO Production
newExecutionModel setts disc = do
    env <- newEnv setts disc
    let handler res = do
            closing env
            case res of
                Left e -> do
                    _settingsLog setts (Error $ UnexpectedException e)
                    handles e
                        [ Handler $ \(_ :: ConnectionException) ->
                              atomically $ do
                                  writeTVar (_nextSubmit env) (raiseException e)
                                  putTMVar (_disposed env) ()
                        , Handler $ \(ForceReconnectionException node) -> do
                              newConn <- newConnection setts disc
                              connForceReconnect newConn node
                              atomically $ writeTVar (_connVar env) newConn
                              _ <- forkFinally (bootstrap env) handler
                              return ()
                        , Handler $ \(_ :: SomeException) -> do
                              newConn <- newConnection setts disc
                              atomically $ writeTVar (_connVar env) newConn
                              _ <- forkFinally (bootstrap env) handler
                              return ()
                        ]
                _ -> atomically $ putTMVar (_disposed env) ()
    _ <- forkFinally (bootstrap env) handler
    return $ Prod (_nextSubmit env) $ do
        curConn <- readTVar $ _connVar env
        unlessM (connIsClosed curConn) retrySTM
        readTMVar $ _disposed env
