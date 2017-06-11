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
import           ClassyPrelude
import           Data.ProtocolBuffers
import           Data.Serialize
import           Data.Time
import           Data.UUID
import           Data.UUID.V4
import qualified Pipes         as Pipes
import qualified Pipes.Prelude as Pipes

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Callback
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Connection
import Database.EventStore.Internal.EndPoint
import Database.EventStore.Internal.Operation hiding (retry)
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Request =
    forall result.
    Request { _requestOp       :: !(Operation result)
            , _requestId       :: !UUID
            , _requestCmd      :: !Command
            , _requestRespCmd  :: !Command
            , _requestPayload  :: !ByteString
            , _requestResume   :: ByteString -> Code () result ()
            , _requestCallback :: !(Callback result)
            }

--------------------------------------------------------------------------------
packageOf :: Settings -> Request -> Package
packageOf setts Request{..} =
  Package { packageCmd         = _requestCmd
          , packageCorrelation = _requestId
          , packageData        = _requestPayload
          , packageCred        = s_credentials setts
          }

--------------------------------------------------------------------------------
data PendingRequest =
    PendingRequest { _pendingRequest :: !Request
                   , _pendingRetries :: !Int
                   , _pendingLastTry :: !UTCTime
                   , _pendingConnId  :: !UUID
                   }

--------------------------------------------------------------------------------
type PendingRequests = HashMap UUID PendingRequest

--------------------------------------------------------------------------------
data Awaiting
  = forall result.
    Awaiting { _awaitingOp       :: !(Operation result)
             , _awaitingCallback :: !(Callback result)
             }

  | AwaitingRequest !Request

--------------------------------------------------------------------------------
type Awaitings = [Awaiting]

--------------------------------------------------------------------------------
rejectPending :: Exception e => PendingRequest -> e -> IO ()
rejectPending PendingRequest{..} e = go _pendingRequest
  where
    go Request{..} = reject _requestCallback e

--------------------------------------------------------------------------------
applyResponse :: Registry -> PendingRequest -> ByteString -> IO ()
applyResponse reg p@PendingRequest{..} = go _pendingRequest
  where
    go Request{..} bytes =
      case runGet decodeMessage bytes of
        Left e ->
          reject _requestCallback (ProtobufDecodingError e)
        Right resp ->
          execute reg Nothing _requestOp _requestCallback (_requestResume resp)

--------------------------------------------------------------------------------
data Registry =
    Registry  { _regSettings  :: Settings
              , _regPendings  :: IORef PendingRequests
              , _regAwaitings :: IORef Awaitings
              }

--------------------------------------------------------------------------------
newRegistry :: Settings -> IO Registry
newRegistry setts = Registry setts <$> newIORef mempty
                                   <*> newIORef []

--------------------------------------------------------------------------------
restartRequest :: Registry -> Request -> IO ()
restartRequest reg Request{..} = schedule reg _requestOp _requestCallback

--------------------------------------------------------------------------------
schedule :: Registry -> Operation a -> Callback a -> IO ()
schedule reg op cb = scheduleAwait reg (Awaiting op cb)

--------------------------------------------------------------------------------
scheduleAwait :: Registry -> Awaiting -> IO ()
scheduleAwait Registry{..} aw =
  atomicModifyIORef' _regAwaitings $ \stack ->
    (aw : stack, ())

--------------------------------------------------------------------------------
execute :: Registry
        -> Maybe Connection
        -> Operation a
        -> Callback a
        -> Code () a ()
        -> IO ()
execute self mConn op cb code = do
  uuid <- nextRandom
  runExecution (runEffect program) uuid  >>= \case
    Succeeded _ -> return ()
    Retry       -> execute self mConn op cb op
    Failed e    -> reject cb e
    Stop        -> return ()
  where
    operation = compute self op cb mConn >>~ (const code)
    program   = Pipes.for operation (liftIO . fulfill cb)

--------------------------------------------------------------------------------
type Compute a = Server Ask Serve (Execution IO) a

--------------------------------------------------------------------------------
stop :: Compute a
stop = lift (Execution $ \_ -> return Stop)

--------------------------------------------------------------------------------
compute :: Registry
        -> Operation a
        -> Callback a
        -> Maybe Connection
        -> Compute r
compute self@Registry{..} op cb mConn = respond Ignore >>= loop
  where
    loop FreshId = do
      uuid <- liftIO nextRandom
      respond (NewId uuid) >>= loop
    loop (SendPkg pid cmdReq cmdResp bytes) = do
      let request = Request { _requestOp       = op
                            , _requestId       = pid
                            , _requestCmd      = cmdReq
                            , _requestRespCmd  = cmdResp
                            , _requestPayload  = bytes
                            , _requestResume   = respond . RespPkg
                            , _requestCallback = cb
                            }

      liftIO $
        case mConn of
          Nothing   -> scheduleAwait self (AwaitingRequest request)
          Just conn -> issueRequest self conn request

      stop

--------------------------------------------------------------------------------
issueRequest :: Registry -> Connection -> Request -> IO ()
issueRequest reg@Registry{..} conn request = do
  uuid <- nextRandom
  now  <- getCurrentTime
  let pkg     = packageOf _regSettings request uuid
      pending = PendingRequest { _pendingRequest = request
                               , _pendingRetries = 1
                               , _pendingLastTry = now
                               , _pendingConnId  = connectionId conn
                               }

  insertPending reg uuid pending
  enqueuePackage conn pkg

--------------------------------------------------------------------------------
insertPending :: Registry -> UUID -> PendingRequest -> IO ()
insertPending Registry{..} key pending =
  atomicModifyIORef' _regPendings $ \pendings ->
    (insertMap key pending pendings, ())

--------------------------------------------------------------------------------
register :: Registry -> Connection -> Operation a -> Callback a -> IO ()
register reg conn op cb = execute reg (Just conn) op cb op

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
executePending :: Registry -> Package -> PendingRequest -> IO Decision
executePending reg@Registry{..} pkg@Package{..} p@PendingRequest{..} =
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

                restartRequest reg _pendingRequest
                return $ Reconnect node
              -- In this case with just retry the operation.
              _ -> do restartRequest reg _pendingRequest
                      return Handled
        | cmd /= _requestRespCmd _pendingRequest -> do
            let reqCmd = _requestCmd _pendingRequest
                resp   = InvalidServerResponse reqCmd cmd
            rejectPending p resp
            return Handled
        | otherwise -> do
            applyResponse reg p packageData
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
checkAndRetry Registry{..} conn = do
  pendings    <- readIORef _regPendings
  now         <- getCurrentTime
  newPendings <- foldM (checking now) pendings (mapToList pendings)
  atomicWriteIORef _regPendings newPendings
  where
    checking now reg (key, p)
      | diffUTCTime now (_pendingLastTry p) >= s_operationTimeout _regSettings =
        let retry = do
              uuid <- nextRandom
              let pkg        = packageOf _regSettings (_pendingRequest p) uuid
                  newPending = p { _pendingLastTry = now
                                 , _pendingRetries = _pendingRetries p + 1
                                 , _pendingConnId  = connectionId conn
                                 }

                  newReg = deleteMap key $ insertMap uuid newPending reg

              enqueuePackage conn pkg
              return reg in
        case s_operationRetry _regSettings of
          AtMost maxAttempts
            | _pendingRetries p <= maxAttempts
              -> retry
            | otherwise
              -> do let cmd    = _requestCmd $ _pendingRequest p
                        action = rejectPending p
                                   (OperationMaxAttemptReached key cmd)

                    return $ deleteMap key reg
          KeepRetrying -> retry
      | otherwise = return reg

--------------------------------------------------------------------------------
startAwaitings :: Registry -> Connection -> IO ()
startAwaitings reg@Registry{..} conn = do
    awaitings <- atomicModifyIORef' _regAwaitings $ \stack ->
      ([], stack)

    for_ awaitings $ \aw ->
      case aw of
        Awaiting{..}        -> register reg conn _awaitingOp _awaitingCallback
        AwaitingRequest req -> issueRequest reg conn req


--------------------------------------------------------------------------------
maybeDecodeMessage :: Decode a => ByteString -> Maybe a
maybeDecodeMessage bytes =
    case runGet decodeMessage bytes of
        Right a -> Just a
        _       -> Nothing
