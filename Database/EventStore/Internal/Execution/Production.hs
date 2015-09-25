{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
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
-- on 3 threads. The Reader thread that reads 'Package' from the connection, the
-- Runner thread which executes finalizers submitted by the user (typically what
-- to do on operation completion or when a event has arrived for a subscription)
-- , and the Manager thread that handles requests coming both from the user
-- and the Reader thread. If the Reader or Runner threads die, it will be
-- restarted by the Manager thread if the connection hasn't been closed by user
-- in the meantime.
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Execution.Production
    ( Production
    , newExecutionModel
    , pushOperation
    , shutdown
    ) where

--------------------------------------------------------------------------------
import Prelude hiding (take)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Fix
import Data.Foldable
import Text.Printf

--------------------------------------------------------------------------------
import Data.Serialize.Get hiding (Done)
import Data.Serialize.Put
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Connection
import Database.EventStore.Internal.Generator
import Database.EventStore.Internal.Operation hiding
    (Report(..), Input(..), retry)
import Database.EventStore.Internal.Packages
import Database.EventStore.Internal.Processor
import Database.EventStore.Internal.Step
import Database.EventStore.Internal.Types hiding (Stopped)
import Database.EventStore.Logging

--------------------------------------------------------------------------------
data Worker
    = Reader ThreadId
    | Runner ThreadId
    deriving Show

--------------------------------------------------------------------------------
newtype Production = Prod (TChan Msg)

--------------------------------------------------------------------------------
data Msg
    = Stopped Worker
    | Arrived Package
    | Shutdown
    | forall a.
      NewOperation (Either OperationError a -> IO ()) (Operation 'Init a)

--------------------------------------------------------------------------------
-- | Asks to shutdown the connection to the server asynchronously.
shutdown :: Production -> IO ()
shutdown (Prod mailbox) = atomically $ writeTChan mailbox Shutdown

--------------------------------------------------------------------------------
-- | Pushes a new 'Operation' asynchronously.
pushOperation :: Production
             -> (Either OperationError a -> IO ())
             -> Operation 'Init a
             -> IO ()
pushOperation (Prod mailbox) k op =
    atomically $ writeTChan mailbox (NewOperation k op)

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
    }

--------------------------------------------------------------------------------
emptyState :: Settings -> Generator -> State
emptyState setts gen = State (newProcessor setts gen) Nothing Nothing

--------------------------------------------------------------------------------
registerWorker :: Worker -> State -> State
registerWorker (Reader tid) s = s { _reader = Just tid }
registerWorker (Runner tid) s = s { _runner = Just tid }

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
            Right pkg -> atomically $ writeTChan mailbox (Arrived pkg)

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

    let pack = Package
               { packageCmd         = cmd
               , packageCorrelation = col
               , packageData        = dta
               , packageCred        = cred
               }

    return pack

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
data Resp
    = Loop
    | WritePkg Package
    | Restart Worker
    | Closing

--------------------------------------------------------------------------------
-- | Manager state-machine.
manager :: Settings
        -> Connection
        -> TVar State
        -> TChan Msg
        -> TChan Job
        -> IO ()
manager setts conn var mailbox job_queue = bootstrap
  where
    createReader = spawn Reader (reader setts mailbox conn)
    createRunner = spawn Runner (runner job_queue)

    bootstrap = do
        reid <- createReader
        ruid <- createRunner
        let _F = registerWorker (Reader reid) . registerWorker (Runner ruid)
        atomically $ modifyTVar' var _F
        cruise

    restartWorker (Reader _) = createReader
    restartWorker (Runner _) = createRunner

    cruise = do
        resp <- atomically $ do
            msg    <- readTChan mailbox
            s      <- readTVar var
            closed <- connIsClosed conn
            case msg of
                Stopped w | not closed -> return $ Restart w
                          | otherwise  -> return Closing
                Arrived pkg ->
                    case submitPackage pkg $ _proc s of
                        Done j new_proc -> do
                            let job = Job j
                            writeTChan job_queue job
                            if closed
                                then return Closing
                            else do
                                modifyTVar' var $ updateProc new_proc
                                return Loop
                        Send new_pkg new_proc ->
                            if closed
                            then return Closing
                            else do
                                modifyTVar' var $ updateProc new_proc
                                return $ WritePkg new_pkg
                        Cont new_proc -> do
                            modifyTVar' var $ updateProc new_proc
                            if closed
                                then return Closing
                                else return Loop
                Shutdown -> return Closing
                NewOperation k op ->
                    if closed
                    then return Loop
                    else do
                        let (pkg, new_proc) = newOperation k op $ _proc s
                        modifyTVar' var $ updateProc new_proc
                        return $ WritePkg pkg
        case resp of
            Loop         -> cruise
            WritePkg pkg -> writePkg conn pkg >> cruise
            Closing      -> closing
            Restart w    -> restartWorker w >> cruise

    closing = do
        let loop = do
                nxt <- lookupTChan mailbox
                case nxt of
                    Nothing -> return ()
                    Just msg ->
                        case msg of
                            Arrived pkg -> do
                                s <- readTVar var
                                case submitPackage pkg $ _proc s of
                                    Done j nxt_proc -> do
                                        let job = Job j
                                        writeTChan job_queue job
                                        modifyTVar' var $ updateProc nxt_proc
                                        loop
                                    Send _ nxt_proc -> do
                                        modifyTVar' var $ updateProc nxt_proc
                                        loop
                                    Cont nxt_proc -> do
                                        modifyTVar' var $ updateProc nxt_proc
                                        loop
                            _ -> loop
        atomically loop
        atomically $ do
            empty <- isEmptyTChan job_queue
            when (not empty) retry
        State _ retid rutid <- readTVarIO var
        traverse_ killThread retid
        traverse_ killThread rutid

    spawn mk action =
        mfix $ \tid -> forkFinally action $ \_ ->
            atomically $ writeTChan mailbox (Stopped $ mk tid)

--------------------------------------------------------------------------------
-- | Main Production execution model entry point.
newExecutionModel :: Settings -> HostName -> Int -> IO Production
newExecutionModel setts host port = do
    gen       <- newGenerator
    mailbox   <- newTChanIO
    pkg_queue <- newTChanIO
    conn      <- newConnection setts host port
    var       <- newTVarIO $ emptyState setts gen
    let mkMgr = manager setts conn var mailbox pkg_queue
        handler res =
            case res of
                Left _ -> do
                    _ <- forkFinally mkMgr handler
                    return ()
                _ -> return ()
    _ <- forkFinally mkMgr handler
    return $ Prod mailbox

--------------------------------------------------------------------------------
writePkg :: Connection -> Package -> IO ()
writePkg conn pkg = do
    connSend conn (runPut $ putPackage pkg)
    connFlush conn

--------------------------------------------------------------------------------
lookupTChan :: TChan a -> STM (Maybe a)
lookupTChan chan = do
    empty <- isEmptyTChan chan
    if empty then return Nothing else fmap Just $ readTChan chan
