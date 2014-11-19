--------------------------------------------------------------------------------
-- |
-- Module : Database.Eventstore.Internal.Processor
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.Eventstore.Internal.Processor
    ( Application(..)
    , newProcessor
    ) where

--------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import qualified Data.ByteString as B
import           Data.Foldable (for_)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Serialize.Get
import           Data.Serialize.Put
import           Data.Traversable (for)
import           Data.Word
import           System.IO
import           Text.Printf

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Serialize.Put
import Data.Time
import Data.UUID
import Network
import System.Random

--------------------------------------------------------------------------------
import Database.Eventstore.Internal.Packages
import Database.Eventstore.Internal.Reader
import Database.Eventstore.Internal.Types

--------------------------------------------------------------------------------
-- Env
--------------------------------------------------------------------------------
data Env
    = Env
      { _hostname   :: HostName
      , _port       :: Int
      , _settings   :: Settings
      , _chan       :: TChan Msg
      , _finalizer  :: TVar (IO ())
      }

--------------------------------------------------------------------------------
getMsg :: Env -> IO Msg
getMsg env = atomically $ readTChan (_chan env)

--------------------------------------------------------------------------------
sendMsg :: Env -> Msg -> IO ()
sendMsg env msg = atomically $ writeTChan (_chan env) msg

--------------------------------------------------------------------------------
heartbeatInterval :: Env -> NominalDiffTime
heartbeatInterval env = _heartbeatInterval $ _settings env

--------------------------------------------------------------------------------
heartbeatTimeout :: Env -> NominalDiffTime
heartbeatTimeout env = _heartbeatTimeout $ _settings env

--------------------------------------------------------------------------------
newEnv :: Settings -> TChan Msg -> HostName -> Int -> IO Env
newEnv settings chan host port = do
    ref <- newTVarIO (return ())

    return $ Env host port settings chan ref

--------------------------------------------------------------------------------
registerFinalizer :: Env -> IO () -> IO ()
registerFinalizer env action = atomically $ writeTVar var action
  where
    var = _finalizer env

--------------------------------------------------------------------------------
runFinalizer :: Env -> IO ()
runFinalizer env = do
    action <- atomically $ do
        act <- readTVar var
        writeTVar var (return ())
        return act
    action
  where
    var = _finalizer env

--------------------------------------------------------------------------------
-- Connection
--------------------------------------------------------------------------------
data Connection
    = Connection
      { _connId             :: UUID
      , _connHandle         :: Handle
      , _connReaderThreadId :: ThreadId
      }

--------------------------------------------------------------------------------
connectionSend :: Connection -> Put -> IO ()
connectionSend conn put = B.hPut handle (runPut put) >> hFlush handle
  where
    handle = _connHandle conn

--------------------------------------------------------------------------------
connectionClose :: Connection -> IO ()
connectionClose conn = do
    killThread thread_id
    hClose handle
    printf "Disconnected %s\n" conn_id_str
  where
    handle      = _connHandle conn
    thread_id   = _connReaderThreadId conn
    conn_id_str = toString $ _connId conn

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
type Processor = Env -> State -> IO ()

--------------------------------------------------------------------------------
data Application
    = Application
      { appProcess   :: IO ()
      , appFinalizer :: IO ()
      }

--------------------------------------------------------------------------------
-- Manager state
--------------------------------------------------------------------------------
data HeartbeatInfo
    = HeartbeatInfo
      { _lastPackage   :: !Int             -- ^ Last package since last update
      , _intervalStage :: !Bool
      , _elapsedTime   :: !NominalDiffTime -- ^ Elapsed time since last update
      }

--------------------------------------------------------------------------------
-- | Holds every needed piece of information in order to properly communicate
--   with an Eventstore backend
data State
    = State
      { _lastTime      :: !UTCTime
      , _heartbeatInfo :: !HeartbeatInfo
      , _packageNumber :: !Int  -- ^ Number of received packages
      , _operations    :: !(M.Map UUID Operation)
      }

--------------------------------------------------------------------------------
updateHeartbeatInfo :: State
                    -> UTCTime -- ^ Current time
                    -> Bool    -- ^ Is interval stage
                    -> Int     -- ^ Package number
                    -> State
updateHeartbeatInfo cur_state cur_time is_interval_state package_num = new_state
  where
    last_time      = _lastTime cur_state
    elapsed_time   = diffUTCTime cur_time last_time
    new_heart_info = HeartbeatInfo package_num is_interval_state elapsed_time
    new_state      = cur_state { _heartbeatInfo = new_heart_info }

--------------------------------------------------------------------------------
incrPackageNumber :: State -> State
incrPackageNumber cur_state = new_state
  where
    new_package_number = _packageNumber cur_state + 1
    new_state          = cur_state { _packageNumber = new_package_number }

--------------------------------------------------------------------------------
-- | Create an initial @State@
newState :: IO State
newState = do
    cur_time <- getCurrentTime
    let package_num = 0
        info        = HeartbeatInfo
                      { _lastPackage   = package_num
                      , _intervalStage = True
                      , _elapsedTime   = fromIntegral 0
                      }

        state       = State
                      { _lastTime      = cur_time
                      , _heartbeatInfo = info
                      , _packageNumber = package_num
                      , _operations    = M.empty
                      }

    return state

--------------------------------------------------------------------------------
newProcessor :: Settings -> TChan Msg -> HostName -> Int -> IO Application
newProcessor settings chan host port = do
    env   <- newEnv settings chan host port
    state <- newState
    let app = Application
              { appProcess   = connecting env state
              , appFinalizer = runFinalizer env
              }

    return app

--------------------------------------------------------------------------------
connecting :: Processor
connecting env state = do
    handle <- connectTo host (PortNumber $ fromIntegral port)
    hSetBuffering handle NoBuffering

    rid      <- forkFinally (readerThread chan handle) recovering
    conn_id  <- randomIO
    cur_time <- getCurrentTime
    let pack_num  = _packageNumber state
        new_state = updateHeartbeatInfo state cur_time True pack_num
        conn      = Connection
                    { _connId             = conn_id
                    , _connHandle         = handle
                    , _connReaderThreadId = rid
                    }

    printf "Connected %s\n" (toString conn_id)
    registerFinalizer env $
        connectionClose conn

    connected conn env new_state

  where
    port = _port env
    host = _hostname env
    chan = _chan env

    recovering (Left some_ex)=
        case fromException some_ex of
            Just e ->
                case e of
                    ConnectionClosedByServer
                        -> sendMsg env Reconnect
                    Stopped
                        -> return ()
            _ -> sendMsg env Reconnect

    recovering _ = return ()

--------------------------------------------------------------------------------
connected :: Connection -> Processor
connected conn env state = getMsg env >>= go
  where
    go Reconnect = do
        runFinalizer env
        putStrLn "Reconnecting..."
        connecting env state

    go (RecvPackage pack) = do
        new_state <- handlePackage conn env state pack
        connected conn env new_state

    go (RegisterOperation op) =
        registerOperation conn op env state

    go (Notice msg) = do
        print msg
        connected conn env state

    go Tick =
        connected conn env state

--------------------------------------------------------------------------------
registerOperation :: Connection -> Operation -> Processor
registerOperation conn op env state = do
    uuid <- randomIO
    pack <- operationCreatePackage op uuid

    let new_op_map = M.insert uuid op op_map
        new_state  = state { _operations = new_op_map }

    connectionSend conn (putPackage pack)
    connected conn env new_state
  where
    op_map = _operations state

--------------------------------------------------------------------------------
handlePackage :: Connection -> Env -> State -> Package -> IO State
handlePackage conn env state pack = go (packageCmd pack)
  where
    go HeartbeatRequest = do
        handleHeartbeatRequest conn pack
        return new_state

    go HeartbeatResponse =
        return new_state

    go _ =
        case M.lookup corr_id op_map of
            Just op -> handleOperation env new_state pack op
            _       -> fmap (const new_state) $ unhandledPackage pack

    corr_id   = packageCorrelation pack
    op_map    = _operations state
    new_state = incrPackageNumber state

--------------------------------------------------------------------------------
handleHeartbeatRequest :: Connection -> Package -> IO ()
handleHeartbeatRequest conn pack =
    connectionSend conn (putPackage pack_resp)
  where
    corr_id     = packageCorrelation pack
    pack_resp   = heartbeatResponsePackage corr_id
    corr_id_str = toString corr_id

--------------------------------------------------------------------------------
handleBadRequest :: Package -> IO ()
handleBadRequest pack = printf "BadRequest on %s\n" cor_id
  where
    cor_id = toString $ packageCorrelation pack

--------------------------------------------------------------------------------
unhandledPackage :: Package -> IO ()
unhandledPackage pack = printf "Unhandled command: %s\n" cmd_str
  where
    cmd_str = show $ packageCmd pack

--------------------------------------------------------------------------------
handleOperation :: Env -> State -> Package -> Operation -> IO State
handleOperation env state pack op = do
    decision <- operationInspect op pack
    case decision of
        DoNothing ->
            return state
        EndOperation ->
            return new_state
        Retry
            -> do sendMsg env (RegisterOperation op)
                  return new_state
        Reconnection
            -> do sendMsg env Reconnect
                  sendMsg env (RegisterOperation op)
                  return new_state
        _ -> fail "Unexpected decision Processor.hs"
  where
    corr_id    = packageCorrelation pack
    op_map     = _operations state
    new_op_map = M.delete corr_id op_map
    new_state  = state { _operations = new_op_map }

-- --------------------------------------------------------------------------------
-- manageHeartbeats :: ConnectionManager -> IO ()
-- manageHeartbeats mgr@ConnectionManager{..} = do
--     elapsed  <- managerElapsedTime mgr
--     info     <- atomically $ readTVar mgrHeartbeatInfo
--     pack_num <- atomically $ readTVar mgrPackageNumber
--     let timeout = if heartbeatIntervalStage info
--                   then heartbeatInterval
--                   else heartbeatTimeout

--     if pack_num /= heartbeatLastPackage info
--         then managerUpdateHeartbeatInfo mgr pack_num True
--         else when (heartbeatIntervalStage info) $ do
--                  pack <- heartbeatPackage
--                  doSendPackage mgr pack
--                  managerUpdateHeartbeatInfo mgr (heartbeatLastPackage info) False
