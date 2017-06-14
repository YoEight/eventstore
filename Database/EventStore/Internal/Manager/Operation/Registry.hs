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
    , newRegistry
    , register
    , schedule
    , handlePackage
    , abortPendingRequests
    , checkAndRetry
    , startAwaitings
    ) where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Bifoldable (bitraverse_)
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
import Database.EventStore.Internal.Operation hiding (retry)
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
type SessionMap = Map SessionId Session

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

  execute reg session Nothing

--------------------------------------------------------------------------------
resumeNoPkgSession :: Registry -> Session -> IO ()
resumeNoPkgSession reg session@Session{..} = do
  atomicModifyIORef' sessionStack $ \case
    Optional k -> (Resolved $ k Nothing, ())
    same       -> (same, ())

  execute reg session Nothing

--------------------------------------------------------------------------------
reinitSession :: Session -> IO ()
reinitSession Session{..} = atomicWriteIORef sessionStack (Resolved sessionOp)

--------------------------------------------------------------------------------
restartSession :: Registry -> Session -> IO ()
restartSession reg session = do
  reinitSession session
  scheduleSession reg session

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
                  , sessionStack = ref
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
data Registry =
    Registry  { _regSettings  :: Settings
              , _regPendings  :: IORef PendingRequests
              , _regAwaitings :: IORef Awaitings
              , _stopwatch    :: Stopwatch
              , _sessions     :: Sessions
              }

--------------------------------------------------------------------------------
newRegistry :: Settings -> IO Registry
newRegistry setts = Registry setts <$> newIORef mempty
                                   <*> newIORef []
                                   <*> newStopwatch
                                   <*> newSessions

--------------------------------------------------------------------------------
schedule :: Registry -> Operation a -> Callback a -> IO ()
schedule reg op cb = scheduleSession reg =<< createSession (_sessions reg) op cb

--------------------------------------------------------------------------------
scheduleSession :: Registry -> Session -> IO ()
scheduleSession reg session = scheduleAwait reg (Awaiting session)

--------------------------------------------------------------------------------
scheduleAwait :: Registry -> Awaiting -> IO ()
scheduleAwait Registry{..} aw =
  atomicModifyIORef' _regAwaitings $ \stack ->
    (aw : stack, ())

--------------------------------------------------------------------------------
execute :: Registry -> Session -> Maybe Connection -> IO ()
execute self session mConn = do
  uuid <- nextRandom
  runExecution operation uuid >>= \case
    Succeeded decision ->
      case decision of
        Completed -> destroySession (_sessions self) session
        Suspended -> return ()
    Retry -> do
      reinitSession session
      execute self session mConn
    Failed e -> do
      destroySession (_sessions self) session
      rejectSession session e
  where
    operation = compute self session mConn

--------------------------------------------------------------------------------
data Computed
  = Completed
  | Suspended

--------------------------------------------------------------------------------
compute :: Registry -> Session -> Maybe Connection -> Execution Computed
compute self@Registry{..} session@Session{..} mConn =
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
              case mConn of
                Nothing   -> scheduleAwait self (AwaitingRequest session req)
                Just conn -> issuePending self session conn (Just req)

            return Suspended
          WaitRemote uuid -> liftIO $ do
            atomicWriteIORef sessionStack (Optional k)
            case mConn of
              Nothing   -> scheduleAwait self (Awaiting session)
              Just conn -> issuePending self session conn Nothing

            return Suspended

--------------------------------------------------------------------------------
issuePending :: Registry -> Session -> Connection -> Maybe Request -> IO ()
issuePending reg@Registry{..} session conn mReq = do
  uuid    <- nextRandom
  elapsed <- stopwatchElapsed _stopwatch
  let
      pending = Pending { _pendingRequest = mReq
                        , _pendingSession = session
                        , _pendingRetries = 1
                        , _pendingLastTry = elapsed
                        , _pendingConnId  = connectionId conn
                        }

  insertPending reg uuid pending
  traverse_ (handleReq uuid) mReq
  where
    handleReq uuid req =
      enqueuePackage conn (packageOf _regSettings req uuid)

--------------------------------------------------------------------------------
insertPending :: Registry -> UUID -> Pending -> IO ()
insertPending Registry{..} key pending =
  atomicModifyIORef' _regPendings $ \pendings ->
    (insertMap key pending pendings, ())

--------------------------------------------------------------------------------
register :: Registry -> Connection -> Operation a -> Callback a -> IO ()
register reg conn op cb = do
  session <- createSession (_sessions reg) op cb
  execute reg session (Just conn)

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
    starting (Awaiting session) =
      execute reg session (Just conn)
    starting (AwaitingRequest session req) =
      issuePending reg session conn (Just req)

--------------------------------------------------------------------------------
maybeDecodeMessage :: Decode a => ByteString -> Maybe a
maybeDecodeMessage bytes =
    case runGet decodeMessage bytes of
        Right a -> Just a
        _       -> Nothing
