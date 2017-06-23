{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Manager.Operation.Registry
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
-- Main operation bookkeeping structure.
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Manager.Operation.Registry
    ( Registry
    , OperationMaxAttemptReached(..)
    , Decision(..)
    , KnownConnection(..)
    , newRegistry
    , register
    , handlePackage
    , abortPendingRequests
    , checkAndRetry
    , startAwaitings
    ) where

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Serialize
import Data.Time
import Data.UUID
import Data.UUID.V4

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Callback
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Connection
import Database.EventStore.Internal.EndPoint
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Operation hiding (retry)
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Stopwatch
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Request =
  Request { _requestCmd     :: !Command
          , _requestPayload :: !ByteString
          }

--------------------------------------------------------------------------------
data Suspend a
  = Required !(Package -> Operation a)
  | Optional !(Maybe Package -> Operation a)
  | Resolved !(Operation a)

--------------------------------------------------------------------------------
type SessionId  = Integer
type SessionMap = HashMap SessionId Session

--------------------------------------------------------------------------------
data Session =
  forall result.
  Session { sessionId       :: !SessionId
          , sessionOp       :: !(Operation result)
          , sessionStack    :: !(IORef (Suspend result))
          , sessionCallback :: !(Callback result)
          }

--------------------------------------------------------------------------------
rejectSession :: Exception e => Session -> e -> IO ()
rejectSession Session{..} = reject sessionCallback

--------------------------------------------------------------------------------
resumeSession :: Registry -> Session -> Package -> IO ()
resumeSession reg session@Session{..} pkg = do
  atomicModifyIORef' sessionStack $ \case
      Required k -> (Resolved $ k pkg, ())
      Optional k -> (Resolved $ k (Just pkg), ())
      same       -> (same, ())

  execute reg session

--------------------------------------------------------------------------------
resumeNoPkgSession :: Registry -> Session -> IO ()
resumeNoPkgSession reg session@Session{..} = do
  atomicModifyIORef' sessionStack $ \case
    Optional k -> (Resolved $ k Nothing, ())
    same       -> (same, ())

  execute reg session

--------------------------------------------------------------------------------
reinitSession :: Session -> IO ()
reinitSession Session{..} = atomicWriteIORef sessionStack (Resolved sessionOp)

--------------------------------------------------------------------------------
restartSession :: Registry -> Session -> IO ()
restartSession reg session = do
  reinitSession session
  execute reg session

--------------------------------------------------------------------------------
data Sessions =
  Sessions { sessionsNextId :: IORef SessionId
           , sessionsMap    :: IORef SessionMap
           }

--------------------------------------------------------------------------------
createSession :: Sessions -> Operation a -> Callback a -> IO Session
createSession Sessions{..} op cb = do
  sid <- atomicModifyIORef' sessionsNextId $ \i -> (succ i, i)
  ref <- newIORef (Resolved op)
  atomicModifyIORef' sessionsMap $ \m ->
    let session =
          Session { sessionId       = sid
                  , sessionOp       = op
                  , sessionStack    = ref
                  , sessionCallback = cb
                  } in
    (insertMap sid session m, session)

--------------------------------------------------------------------------------
destroySession :: Sessions -> Session -> IO ()
destroySession Sessions{..} s =
  atomicModifyIORef' sessionsMap $ \m -> (deleteMap (sessionId s) m, ())

--------------------------------------------------------------------------------
sessionDisposed :: Sessions -> Session -> IO Bool
sessionDisposed Sessions{..} s = notMember (sessionId s) <$> readIORef sessionsMap

--------------------------------------------------------------------------------
newSessions :: IO Sessions
newSessions =
  Sessions <$> newIORef 0 
           <*> newIORef mempty

--------------------------------------------------------------------------------
packageOf :: Settings -> Request -> UUID -> Package
packageOf setts Request{..} uuid =
  Package { packageCmd         = _requestCmd
          , packageCorrelation = uuid
          , packageData        = _requestPayload
          , packageCred        = s_credentials setts
          }

--------------------------------------------------------------------------------
data Pending =
    Pending { _pendingRequest :: !(Maybe Request)
            , _pendingSession :: !Session
            , _pendingRetries :: !Int
            , _pendingLastTry :: !NominalDiffTime
            , _pendingConnId  :: !UUID
            }

--------------------------------------------------------------------------------
destroyPendingSession :: Sessions -> Pending -> IO ()
destroyPendingSession sessions Pending{..} =
  destroySession sessions _pendingSession

--------------------------------------------------------------------------------
type PendingRequests = HashMap UUID Pending

--------------------------------------------------------------------------------
data Awaiting
  = Awaiting !Session
  | AwaitingRequest !Session !Request

--------------------------------------------------------------------------------
type Awaitings = [Awaiting]

--------------------------------------------------------------------------------
rejectPending :: Exception e => Pending -> e -> IO ()
rejectPending Pending{..} = rejectSession _pendingSession

--------------------------------------------------------------------------------
applyResponse :: Registry -> Pending -> Package -> IO ()
applyResponse reg Pending{..} = resumeSession reg _pendingSession

--------------------------------------------------------------------------------
restartPending :: Registry -> Pending -> IO ()
restartPending reg Pending{..} = restartSession reg _pendingSession

--------------------------------------------------------------------------------
newtype KnownConnection =
  KnownConnection { lookupKnownConnection :: IO (Maybe Connection) }

--------------------------------------------------------------------------------
data Registry =
    Registry  { _regSettings  :: Settings
              , _logger       :: Logger
              , _conn         :: KnownConnection
              , _regPendings  :: IORef PendingRequests
              , _regAwaitings :: IORef Awaitings
              , _stopwatch    :: Stopwatch
              , _sessions     :: Sessions
              }

--------------------------------------------------------------------------------
newRegistry :: Settings -> Logger -> KnownConnection -> IO Registry
newRegistry setts logger conn =
   Registry setts logger conn <$> newIORef mempty
                              <*> newIORef []
                              <*> newStopwatch
                              <*> newSessions

--------------------------------------------------------------------------------
scheduleAwait :: Registry -> Awaiting -> IO ()
scheduleAwait Registry{..} aw =
  atomicModifyIORef' _regAwaitings $ \stack ->
    (aw : stack, ())

--------------------------------------------------------------------------------
execute :: Registry -> Session -> IO ()
execute self session = do
  runExecution operation >>= \case
    Succeeded decision ->
      case decision of
        Completed -> destroySession (_sessions self) session
        Suspended -> return ()
    Retry -> do
      reinitSession session
      execute self session
    Failed e -> do
      destroySession (_sessions self) session
      rejectSession session e
  where
    operation = compute self session

--------------------------------------------------------------------------------
data Computed
  = Completed
  | Suspended

--------------------------------------------------------------------------------
compute :: Registry -> Session -> Execution Computed
compute self@Registry{..} session@Session{..} =
  liftIO (readIORef sessionStack) >>= \case
    Resolved action -> loop action
    _               -> return Suspended
  where
    loop (MachineT m) = m >>= \case
      Stop  -> return Completed
      Yield a next -> do
        liftIO $ fulfill sessionCallback a
        loop next
      Await k tpe _ ->
        case tpe of
          NeedRemote cmd payload -> do
            let req = Request { _requestCmd     = cmd
                              , _requestPayload = payload
                              }
            liftIO $ do
              atomicWriteIORef sessionStack (Required k)
              lookupKnownConnection _conn >>= \case
                Nothing   -> scheduleAwait self (AwaitingRequest session req)
                Just conn -> issueRequest self session conn req

            return Suspended
          WaitRemote uuid -> liftIO $ do
            atomicWriteIORef sessionStack (Optional k)
            let mkNewPending conn = do
                  let connId = connectionId conn
                  pending <- createPending session _stopwatch connId Nothing
                  insertPending self uuid pending

            traverse_ mkNewPending =<< lookupKnownConnection _conn
            return Suspended

--------------------------------------------------------------------------------
issueRequest :: Registry -> Session -> Connection -> Request -> IO ()
issueRequest reg@Registry{..} session conn req = do
  uuid    <- nextRandom
  pending <- createPending session _stopwatch (connectionId conn) (Just req)

  insertPending reg uuid pending
  enqueuePackage conn (packageOf _regSettings req uuid)

--------------------------------------------------------------------------------
createPending :: Session
              -> Stopwatch
              -> UUID
              -> Maybe Request
              -> IO Pending
createPending session stopwatch connId mReq = do
  elapsed <- stopwatchElapsed stopwatch
  let pending =
        Pending { _pendingRequest = mReq
                , _pendingSession = session
                , _pendingRetries = 1
                , _pendingLastTry = elapsed
                , _pendingConnId  = connId
                }

  return pending

--------------------------------------------------------------------------------
insertPending :: Registry -> UUID -> Pending -> IO ()
insertPending Registry{..} key pending =
  atomicModifyIORef' _regPendings $ \pendings ->
    (insertMap key pending pendings, ())

--------------------------------------------------------------------------------
register :: Registry -> Operation a -> Callback a -> IO ()
register reg op cb = do
  session <- createSession (_sessions reg) op cb
  execute reg session

--------------------------------------------------------------------------------
abortPendingRequests :: Registry -> IO ()
abortPendingRequests Registry{..} = do
  m <- atomicModifyIORef' _regPendings $ \pendings -> (mempty, pendings)

  for_ m $ \p -> rejectPending p Aborted

--------------------------------------------------------------------------------
data Decision
  = Handled
  | Reconnect NodeEndPoints

--------------------------------------------------------------------------------
handlePackage :: Registry -> Package -> IO (Maybe Decision)
handlePackage reg@Registry{..} pkg@Package{..} = do
  outcome <- atomicModifyIORef' _regPendings $ \pendings ->
    let uuid = packageCorrelation in
    (deleteMap uuid pendings, lookup uuid pendings)

  case outcome of
    Nothing      -> return Nothing
    Just pending -> Just <$> executePending reg pkg pending

--------------------------------------------------------------------------------
executePending :: Registry -> Package -> Pending -> IO Decision
executePending reg@Registry{..} pkg@Package{..} p@Pending{..} =
  case packageCmd of
    cmd | cmd == badRequestCmd -> do
            let reason = packageDataAsText pkg

            rejectPending p (ServerError reason)
            return Handled
        | cmd == notAuthenticatedCmd -> do
            rejectPending p NotAuthenticatedOp
            return Handled
        | cmd == notHandledCmd -> do
            logFormat _logger Debug "NotHandled response received: {}" (Only $ Shown pkg)
            let Just msg = maybeDecodeMessage packageData
                reason   = getField $ notHandledReason msg
            case reason of
              N_NotMaster -> do
                let Just details = getField $ notHandledAdditionalInfo msg
                    info         = masterInfo details
                    node         = masterInfoNodeEndPoints info

                restartPending reg p
                return $ Reconnect node
              -- In this case with just retry the operation.
              _ -> Handled <$ restartPending reg p
        | otherwise -> do
            applyResponse reg p pkg
            return Handled

--------------------------------------------------------------------------------
-- | Occurs when an operation has been retried more than 's_operationRetry'.
data OperationMaxAttemptReached =
  OperationMaxAttemptReached UUID Command
  deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception OperationMaxAttemptReached

--------------------------------------------------------------------------------
checkAndRetry :: Registry -> Connection -> IO ()
checkAndRetry self@Registry{..} conn = do
  pendings    <- readIORef _regPendings
  elapsed     <- stopwatchElapsed _stopwatch
  newPendings <- foldM (checking elapsed) pendings (mapToList pendings)
  atomicWriteIORef _regPendings newPendings
  where
    checking elapsed reg (key, p) =
      case _pendingRequest p of
        Nothing
          | connectionId conn == _pendingConnId p -> return reg
          | otherwise -> do
            resumeNoPkgSession self (_pendingSession p)
            disposed <- sessionDisposed _sessions (_pendingSession p)
            let newReg = if disposed then deleteMap key reg else reg
            return newReg
        Just req -> do
          let lastTry    = _pendingLastTry p
              hasTimeout = elapsed - lastTry >= s_operationTimeout _regSettings
          if hasTimeout || _pendingConnId p /= connectionId conn
            then do
              let retry = do
                    uuid <- nextRandom
                    let pkg     = packageOf _regSettings req uuid
                        pending =
                          p { _pendingLastTry = elapsed
                            , _pendingRetries = _pendingRetries p + 1
                            , _pendingConnId  = connectionId conn
                            }
                        nextReg = deleteMap key $ insertMap uuid pending reg

                    enqueuePackage conn pkg
                    return nextReg

              case s_operationRetry _regSettings of
                AtMost maxAttempts
                  | _pendingRetries p <= maxAttempts
                    -> retry
                  | otherwise -> do
                    let cmd = _requestCmd req
                    destroyPendingSession _sessions p
                    rejectPending p (OperationMaxAttemptReached key cmd)
                    return $ deleteMap key reg
                KeepRetrying -> retry
            else return reg

--------------------------------------------------------------------------------
startAwaitings :: Registry -> Connection -> IO ()
startAwaitings reg@Registry{..} conn = do
  awaitings <- atomicModifyIORef' _regAwaitings $ \stack ->
    ([], reverse stack)

  traverse_ starting awaitings
  where
    starting (Awaiting session)            = execute reg session
    starting (AwaitingRequest session req) = issueRequest reg session conn req

--------------------------------------------------------------------------------
maybeDecodeMessage :: Decode a => ByteString -> Maybe a
maybeDecodeMessage bytes =
    case runGet decodeMessage bytes of
        Right a -> Just a
        _       -> Nothing
