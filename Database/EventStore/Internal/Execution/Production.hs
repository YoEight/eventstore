{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
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
-- | Hols the execution model state.
newtype Production = Prod (TChan Msg)

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
    | AckPersist (IO ()) Running Text [UUID]
    | NakPersist (IO ()) Running Text NakAction (Maybe Text) [UUID]

--------------------------------------------------------------------------------
-- | Asks to shutdown the connection to the server asynchronously.
shutdownExecutionModel :: Production -> IO ()
shutdownExecutionModel (Prod mailbox) = atomically $ writeTChan mailbox Shutdown

--------------------------------------------------------------------------------
-- | Pushes a new 'Operation' asynchronously.
pushOperation :: Production
             -> (Either OperationError a -> IO ())
             -> Operation a
             -> IO ()
pushOperation (Prod mailbox) k op =
    atomically $ writeTChan mailbox (NewOperation k op)

--------------------------------------------------------------------------------
-- | Subscribes to a regular stream.
pushConnectStream :: Production
                  -> (SubConnectEvent -> IO ())
                  -> Text
                  -> Bool
                  -> IO ()
pushConnectStream (Prod mailbox) k n tos =
    atomically $ writeTChan mailbox (ConnectStream k n tos)

--------------------------------------------------------------------------------
-- | Subscribes to a persistent subscription.
pushConnectPersist :: Production
                   -> (SubConnectEvent -> IO ())
                   -> Text
                   -> Text
                   -> Int32
                   -> IO ()
pushConnectPersist (Prod mailbox) k g n buf =
    atomically $ writeTChan mailbox (ConnectPersist k g n buf)

--------------------------------------------------------------------------------
-- | Creates a persistent subscription.
pushCreatePersist :: Production
                  -> (Either PersistActionException ConfirmedAction -> IO ())
                  -> Text
                  -> Text
                  -> PersistentSubscriptionSettings
                  -> IO ()
pushCreatePersist (Prod mailbox) k g n setts =
    atomically $ writeTChan mailbox (CreatePersist k g n setts)

--------------------------------------------------------------------------------
-- | Updates a persistent subscription.
pushUpdatePersist :: Production
                  -> (Either PersistActionException ConfirmedAction -> IO ())
                  -> Text
                  -> Text
                  -> PersistentSubscriptionSettings
                  -> IO ()
pushUpdatePersist (Prod mailbox) k g n setts =
    atomically $ writeTChan mailbox (UpdatePersist k g n setts)

--------------------------------------------------------------------------------
-- | Deletes a persistent subscription.
pushDeletePersist :: Production
                  -> (Either PersistActionException ConfirmedAction -> IO ())
                  -> Text
                  -> Text
                  -> IO ()
pushDeletePersist (Prod mailbox) k g n =
    atomically $ writeTChan mailbox (DeletePersist k g n)

--------------------------------------------------------------------------------
-- | Acknowledges a set of events has been successfully handled.
pushAckPersist :: Production -> IO () -> Running -> Text -> [UUID] -> IO ()
pushAckPersist (Prod mailbox) r run gid evts =
    atomically $ writeTChan mailbox (AckPersist r run gid evts)

--------------------------------------------------------------------------------
-- | Acknowledges a set of events hasn't been handled successfully.
pushNakPersist :: Production
               -> IO ()
               -> Running
               -> Text
               -> NakAction
               -> Maybe Text
               -> [UUID]
               -> IO ()
pushNakPersist (Prod mailbox) r run gid act res evts =
    atomically $ writeTChan mailbox (NakPersist r run gid act res evts)

--------------------------------------------------------------------------------
-- | Unsubscribe from a subscription.
pushUnsubscribe :: Production -> Running -> IO ()
pushUnsubscribe (Prod mailbox) r =
    atomically $ writeTChan mailbox (Unsubscribe r)

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
registerWorker :: Worker -> State -> State
registerWorker (Reader tid) s = s { _reader = Just tid }
registerWorker (Runner tid) s = s { _runner = Just tid }
registerWorker (Writer tid) s = s { _writer = Just tid }

--------------------------------------------------------------------------------
updateProc :: Processor (IO ()) -> State -> State
updateProc p s = s { _proc = p }

--------------------------------------------------------------------------------
-- | Reader thread. Keeps reading 'Package' from the connection.
--------------------------------------------------------------------------------
reader :: Settings -> TChan Msg -> Connection -> IO ()
reader sett mailbox c = forever $ do
    header_bs <- connRecv c 4
    case runGet getLengthPrefix header_bs of
        Left _              -> _settingsLog sett (Error WrongPackageFraming)
        Right length_prefix -> connRecv c length_prefix >>= parsePackage
  where
    parsePackage bs =
        case runGet getPackage bs of
            Left e    -> _settingsLog sett (Error $ PackageParsingError e)
            Right pkg -> do
                atomically $ writeTChan mailbox (Arrived pkg)
                let cmd  = packageCmd pkg
                    uuid = packageCorrelation pkg
                _settingsLog sett $ Info $ PackageReceived cmd uuid

--------------------------------------------------------------------------------
-- | Writer thread, writes incoming 'Package's
--------------------------------------------------------------------------------
writer :: Settings -> TChan Package -> Connection -> IO ()
writer setts chan conn = forever $ do
    pkg <- atomically $ readTChan chan
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
runner :: TChan Job -> IO ()
runner queue = forever $ do
    Job j <- atomically $ readTChan queue
    j

--------------------------------------------------------------------------------
-- | Manager thread. Responsible from serving request coming from both the user
--   and the Reader thread. It also write 'Package' to the connection and
--   dispatches jobs for the runner thread.
--------------------------------------------------------------------------------
-- | Internal intermediary cruise transition.
data Resp r
    = Cruising
    | Restart Worker
    | Closing

--------------------------------------------------------------------------------
-- | Manager state-machine.
manager :: Settings
        -> Connection
        -> TVar State
        -> TChan Msg
        -> TChan Package
        -> TChan Job
        -> IO ()
manager setts conn var mailbox pkg_queue job_queue = bootstrap
  where
    createReader = spawn Reader (reader setts mailbox conn)
    createRunner = spawn Runner (runner job_queue)
    createWriter = spawn Writer (writer setts pkg_queue conn)

    bootstrap = do
        reid <- createReader
        ruid <- createRunner
        wuid <- createWriter
        let _F = registerWorker (Reader reid) .
                 registerWorker (Runner ruid) .
                 registerWorker (Writer wuid)
        atomically $ modifyTVar' var _F
        cruise

    restartWorker (Reader _) = createReader
    restartWorker (Runner _) = createRunner
    restartWorker (Writer _) = createWriter

    loopTransition (Produce j nxt) = do
        let job = Job j
        writeTChan job_queue job
        loopTransition nxt
    loopTransition (Transmit pkg nxt) = do
        writeTChan pkg_queue pkg
        loopTransition nxt
    loopTransition (Await new_proc) =
        return new_proc

    cruise = do
        resp <- atomically $ do
            msg <- readTChan mailbox
            s   <- readTVar var
            case msg of
                Stopped w e ->
                    case fromException e of
                        Just (_ :: ConnectionException) -> return Closing
                        _ -> return $ Restart w
                Arrived pkg -> do
                    let sm = submitPackage pkg $ _proc s
                    new_proc <- loopTransition sm
                    modifyTVar' var $ updateProc new_proc
                    return Cruising
                Shutdown -> return Closing
                NewOperation k op -> do
                    let sm = newOperation k op $ _proc s
                    new_proc <- loopTransition sm
                    modifyTVar' var $ updateProc new_proc
                    return Cruising
                ConnectStream k n tos -> do
                    let sm = connectRegularStream k n tos $ _proc s
                    new_proc <- loopTransition sm
                    modifyTVar' var $ updateProc new_proc
                    return Cruising
                ConnectPersist k g n b -> do
                    let sm = connectPersistent k g n b $ _proc s
                    new_proc <- loopTransition sm
                    modifyTVar' var $ updateProc new_proc
                    return Cruising
                Unsubscribe r -> do
                    let sm = unsubscribe r $ _proc s
                    new_proc <- loopTransition sm
                    modifyTVar' var $ updateProc new_proc
                    return Cruising
                CreatePersist k g n psetts -> do
                    let sm = createPersistent k g n psetts $ _proc s
                    new_proc <- loopTransition sm
                    modifyTVar' var $ updateProc new_proc
                    return Cruising
                UpdatePersist k g n psetts -> do
                    let sm = updatePersistent k g n psetts $ _proc s
                    new_proc <- loopTransition sm
                    modifyTVar' var $ updateProc new_proc
                    return Cruising
                DeletePersist k g n -> do
                    let sm = deletePersistent k g n $ _proc s
                    new_proc <- loopTransition sm
                    modifyTVar' var $ updateProc new_proc
                    return Cruising
                AckPersist r run gid evts -> do
                    let sm = ackPersist r run gid evts $ _proc s
                    new_proc <- loopTransition sm
                    modifyTVar' var $ updateProc new_proc
                    return Cruising
                NakPersist r run gid act res evts -> do
                    let sm = nakPersist r run gid act res evts $ _proc s
                    new_proc <- loopTransition sm
                    modifyTVar' var $ updateProc new_proc
                    return Cruising
        case resp of
            Cruising  -> cruise
            Closing   -> closing
            Restart w -> restartWorker w >> cruise

    closing = do
        let loop = do
                nxt <- lookupTChan mailbox
                case nxt of
                    Nothing -> return ()
                    Just msg ->
                        case msg of
                            Arrived pkg -> do
                                s <- readTVar var
                                let sm = submitPackage pkg $ _proc s
                                nxt_proc <- loopTransition sm
                                modifyTVar' var $ updateProc nxt_proc
                            _ -> loop
        atomically $ do
            loop
            s <- readTVar var
            _ <- loopTransition $ abort $ _proc s
            return ()

        atomically $ do
            end <- isEmptyTChan job_queue
            when (not end) retry
        State _ retid rutid  wutid <- readTVarIO var
        traverse_ killThread retid
        traverse_ killThread rutid
        traverse_ killThread wutid

    spawn mk action =
        mfix $ \tid -> forkFinally action $ \r ->
            case r of
                Left e -> atomically $ writeTChan mailbox (Stopped (mk tid) e)
                _      -> return ()

--------------------------------------------------------------------------------
-- | Main Production execution model entry point.
newExecutionModel :: Settings -> HostName -> PortNumber -> IO Production
newExecutionModel setts host port = do
    gen       <- newGenerator
    mailbox   <- newTChanIO
    pkg_queue <- newTChanIO
    job_queue <- newTChanIO
    conn      <- newConnection setts host port
    var       <- newTVarIO $ emptyState setts gen
    let mkMgr = manager setts conn var mailbox pkg_queue job_queue
        handler res =
            case res of
                Left _ -> do
                    _ <- forkFinally mkMgr handler
                    return ()
                _ -> return ()
    _ <- forkFinally mkMgr handler
    return $ Prod mailbox

--------------------------------------------------------------------------------
lookupTChan :: TChan a -> STM (Maybe a)
lookupTChan chan = do
    end <- isEmptyTChan chan
    if end then return Nothing else fmap Just $ readTChan chan
