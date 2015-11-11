{-# LANGUAGE DataKinds                 #-}
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
import Prelude hiding (take)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Data.Int
import Data.Foldable
import Text.Printf

--------------------------------------------------------------------------------
import Data.Serialize.Get hiding (Done)
import Data.Serialize.Put
import Data.Text
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Connection
import Database.EventStore.Internal.Generator
import Database.EventStore.Internal.Manager.Subscription hiding
    ( submitPackage
    , unsubscribe
    , ackPersist
    , nakPersist
    , abort
    )
import Database.EventStore.Internal.Operation hiding (retry)
import Database.EventStore.Internal.Packages
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
wkConstr :: Worker -> (ThreadId -> Worker)
wkConstr (Reader _) = Reader
wkConstr (Runner _) = Runner
wkConstr (Writer _) = Writer

--------------------------------------------------------------------------------
wkUpdState :: Worker -> State -> State
wkUpdState (Reader tid) s = s { _reader = Just tid }
wkUpdState (Runner tid) s = s { _runner = Just tid }
wkUpdState (Writer tid) s = s { _writer = Just tid }

--------------------------------------------------------------------------------
-- | Hols the execution model state.
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
    , _queue :: TQueue Msg
      -- ^ That queue ties the user, the reader thread and the manager thread.
      --   The user and the reader push new messages onto the queue while the
      --   manager dequeue and handles one message at the time.
    , _pkgQueue :: TQueue Package
      -- ^ That queue ties the writer thread with the manager thread. The writer
      --   dequeue packages from that queue and sends those to the server. While
      --   the manager pushes new packages on every new submitted operation.
    , _jobQueue :: TQueue Job
      -- ^ That queue ties the runner thread with the manager thread. The runner
      --   dequeues IO action from it while the manager pushes new command
      --   finalizers as those arrived.
    , _state :: TVar State
      -- ^ Holds manager thread state.
    , _nextSubmit :: TVar (Msg -> IO ())
      -- ^ Indicates the action to call in order to push new commands.
    , _conn :: Connection
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
-- | Reader thread. Keeps reading 'Package' from the connection.
--------------------------------------------------------------------------------
reader :: Settings -> TQueue Msg -> Connection -> IO ()
reader sett queue c = forever $ do
    header_bs <- connRecv c 4
    case runGet getLengthPrefix header_bs of
        Left _              -> _settingsLog sett (Error WrongPackageFraming)
        Right length_prefix -> connRecv c length_prefix >>= parsePackage
  where
    parsePackage bs =
        case runGet getPackage bs of
            Left e    -> _settingsLog sett (Error $ PackageParsingError e)
            Right pkg -> do
                atomically $ writeTQueue queue (Arrived pkg)
                let cmd  = packageCmd pkg
                    uuid = packageCorrelation pkg
                _settingsLog sett $ Info $ PackageReceived cmd uuid

--------------------------------------------------------------------------------
-- | Writer thread, writes incoming 'Package's
--------------------------------------------------------------------------------
writer :: Settings -> TQueue Package -> Connection -> IO ()
writer setts pkg_queue conn = forever $ do
    pkg <- atomically $ readTQueue pkg_queue
    connSend conn $ runPut $ putPackage pkg
    let cmd  = packageCmd pkg
        uuid = packageCorrelation pkg
    _settingsLog setts $ Info $ PackageSent cmd uuid

--------------------------------------------------------------------------------
getLengthPrefix :: Get Int
getLengthPrefix = fmap fromIntegral getWord32le

--------------------------------------------------------------------------------
getPackage :: Get Package
getPackage = do
    cmd  <- getWord8
    flg  <- getFlag
    col  <- getUUID
    cred <- getCredentials flg
    rest <- remaining
    dta  <- getBytes rest

    let pkg = Package
              { packageCmd         = cmd
              , packageCorrelation = col
              , packageData        = dta
              , packageCred        = cred
              }

    return pkg

--------------------------------------------------------------------------------
getFlag :: Get Flag
getFlag = do
    wd <- getWord8
    case wd of
        0x00 -> return None
        0x01 -> return Authenticated
        _    -> fail $ printf "TCP: Unhandled flag value 0x%x" wd

--------------------------------------------------------------------------------
getCredEntryLength :: Get Int
getCredEntryLength = fmap fromIntegral getWord8

--------------------------------------------------------------------------------
getCredentials :: Flag -> Get (Maybe Credentials)
getCredentials None = return Nothing
getCredentials _ = do
    loginLen <- getCredEntryLength
    login    <- getBytes loginLen
    passwLen <- getCredEntryLength
    passw    <- getBytes passwLen
    return $ Just $ credentials login passw

--------------------------------------------------------------------------------
getUUID :: Get UUID
getUUID = do
    bs <- getLazyByteString 16
    case fromByteString bs of
        Just uuid -> return uuid
        _         -> fail "TCP: Wrong UUID format"

--------------------------------------------------------------------------------
-- Runner thread. Keeps running job comming from the Manager thread.
--------------------------------------------------------------------------------
runner :: TQueue Job -> IO ()
runner job_queue = forever $ do
    Job j <- atomically $ readTQueue job_queue
    j

--------------------------------------------------------------------------------
-- | Spawns a new thread worker.
spawn :: Env -> (ThreadId -> Worker) -> IO Worker
spawn Env{..} mk = do
    tid <- mfix $ \tid ->
        let worker = mk tid
            action =
                case worker of
                    Reader _ -> reader _setts _queue _conn
                    Runner _ -> runner _jobQueue
                    Writer _ -> writer _setts _pkgQueue _conn in
        forkFinally action $ \r ->
            case r of
                Left e -> atomically $ writeTQueue _queue (Stopped worker e)
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
        writeTQueue _jobQueue job
        go nxt
    go (Transmit pkg nxt) = do
        writeTQueue _pkgQueue pkg
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
    msg <- atomically $ readTQueue _queue
    s   <- readTVarIO _state
    case msg of
        Stopped w e ->
            case fromException e of
                Just (_ :: ConnectionException) -> throwIO e
                _ -> do
                    w' <- spawn env (wkConstr w)
                    atomically $ modifyTVar' _state $ wkUpdState w'
                    cruising env
        Arrived pkg -> do
            let sm = submitPackage pkg $ _proc s
            atomically $ do
                new_proc <- runTransition env sm
                modifyTVar' _state $ updateProc new_proc
            cruising env
        Shutdown -> do
            atomically $ writeTVar _nextSubmit (raiseException ClosedConnection)
            closing env
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
    let loop = do
            nxt <- lookupTQueue _queue
            case nxt of
                Nothing -> return ()
                Just msg ->
                    case msg of
                        Arrived pkg -> do
                            s <- readTVar _state
                            let sm = submitPackage pkg $ _proc s
                            nxt_proc <- runTransition env sm
                            modifyTVar' _state $ updateProc nxt_proc
                        _ -> loop

    -- If the connection is already closed, it will throw an exception. We just
    -- make sure it doesn't interfere with the cleaning process.
    _ <- try $ connClose _conn :: (IO (Either ConnectionException ()))
    atomically $ do
        loop
        s <- readTVar _state
        _ <- runTransition env $ abort $ _proc s
        return ()

    atomically $ do
        end <- isEmptyTQueue _jobQueue
        unless end retry

    State _ retid rutid  wutid <- readTVarIO _state
    traverse_ killThread retid
    traverse_ killThread rutid
    traverse_ killThread wutid

    atomically $ putTMVar _disposed ()

--------------------------------------------------------------------------------
raiseException :: Exception e => e -> Msg -> IO ()
raiseException e _ = throwIO e

--------------------------------------------------------------------------------
-- | Main Production execution model entry point.
newExecutionModel :: Settings -> HostName -> Int -> IO Production
newExecutionModel setts host port = do
    gen       <- newGenerator
    queue     <- newTQueueIO
    pkg_queue <- newTQueueIO
    job_queue <- newTQueueIO
    conn      <- newConnection setts host port
    var       <- newTVarIO $ emptyState setts gen
    nxt_sub   <- newTVarIO (atomically . writeTQueue queue)
    disposed  <- newEmptyTMVarIO
    let env = Env setts queue pkg_queue job_queue var nxt_sub conn disposed
        handler res =
            case res of
                Left e -> do
                    atomically $ writeTVar (_nextSubmit env) (raiseException e)
                    closing env
                    _settingsLog setts (Error $ UnexpectedException e)
                _ -> return ()
    _ <- forkFinally (bootstrap env) handler
    return $ Prod nxt_sub $ do
        closed <- connIsClosed conn
        unless closed retry
        readTMVar disposed

--------------------------------------------------------------------------------
lookupTQueue :: TQueue a -> STM (Maybe a)
lookupTQueue queue = do
    end <- isEmptyTQueue queue
    if end then return Nothing else fmap Just $ readTQueue queue
