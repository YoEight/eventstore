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
  forall result.
  Request { _requestOp        :: !(Operation result)
          , _requestCmd       :: !Command
          , _requestPayload   :: !ByteString
          , _requestResume    :: !(Package -> Operation result)
          , _requestCallback  :: !(Callback result)
          }

--------------------------------------------------------------------------------
data Expect =
  forall result.
  Expect { _expectOp       :: !(Operation result)
         , _expectResume   :: !(Package -> Operation result)
         , _expectCallback :: !(Callback result)
         }

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
    Pending { _pendingType    :: !(Either Request Expect)
            , _pendingRetries :: !Int
            , _pendingLastTry :: !NominalDiffTime
            , _pendingConnId  :: !UUID
            }

--------------------------------------------------------------------------------
type PendingRequests = HashMap UUID Pending

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
rejectPending :: Exception e => Pending -> e -> IO ()
rejectPending Pending{..} e = bitraverse_ onRequest onExpect _pendingType
  where
    onRequest Request{..} = reject _requestCallback e
    onExpect  Expect{..}  = reject _expectCallback e

--------------------------------------------------------------------------------
applyResponse :: Registry -> Pending -> Package -> IO ()
applyResponse reg Pending{..} pkg = bitraverse_ onRequest onExpect _pendingType
  where
    onRequest Request{..} =
      execute reg Nothing _requestOp _requestCallback (_requestResume pkg)
    onExpect Expect{..} =
      execute reg Nothing _expectOp _expectCallback (_expectResume pkg)

--------------------------------------------------------------------------------
data Registry =
    Registry  { _regSettings  :: Settings
              , _regPendings  :: IORef PendingRequests
              , _regAwaitings :: IORef Awaitings
              , _stopwatch    :: Stopwatch
              }

--------------------------------------------------------------------------------
newRegistry :: Settings -> IO Registry
newRegistry setts = Registry setts <$> newIORef mempty
                                   <*> newIORef []
                                   <*> newStopwatch

--------------------------------------------------------------------------------
restart :: Registry -> Either Request Expect -> IO ()
restart reg (Left Request{..}) = schedule reg _requestOp _requestCallback
restart reg (Right Expect{..}) = schedule reg _expectOp _expectCallback

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
        -> Operation a
        -> IO ()
execute self mConn op cb code = do
  uuid <- nextRandom
  runExecution operation uuid >>= \case
    Succeeded _ -> return ()
    Retry       -> execute self mConn op cb op
    Failed e    -> reject cb e
  where
    operation = compute self op cb mConn code

--------------------------------------------------------------------------------
compute :: Registry
        -> Operation a
        -> Callback a
        -> Maybe Connection
        -> Operation a
        -> Execution ()
compute self@Registry{..} op cb mConn = loop
  where
    loop (MachineT m) = m >>= \case
      Stop  -> return ()
      Yield a next -> do
        liftIO $ fulfill cb a
        loop next
      Await k tpe _ ->
        case tpe of
          NeedRemote cmd payload -> do
            let request = Request { _requestOp       = op
                                  , _requestCmd      = cmd
                                  , _requestPayload  = payload
                                  , _requestResume   = k
                                  , _requestCallback = cb
                                  }
            liftIO $
              case mConn of
                Nothing   -> scheduleAwait self (AwaitingRequest request)
                Just conn -> issueRequest self conn request
            return ()

--------------------------------------------------------------------------------
issueRequest :: Registry -> Connection -> Request -> IO ()
issueRequest reg@Registry{..} conn request = do
  uuid    <- nextRandom
  elapsed <- stopwatchElapsed _stopwatch
  let pkg     = packageOf _regSettings request uuid
      pending = Pending { _pendingType    = Left request
                        , _pendingRetries = 1
                        , _pendingLastTry = elapsed
                        , _pendingConnId  = connectionId conn
                        }

  insertPending reg uuid pending
  enqueuePackage conn pkg

--------------------------------------------------------------------------------
insertPending :: Registry -> UUID -> Pending -> IO ()
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

                restart reg _pendingType
                return $ Reconnect node
              -- In this case with just retry the operation.
              _ -> do restart reg _pendingType
                      return Handled
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
checkAndRetry Registry{..} conn = do
  pendings    <- readIORef _regPendings
  elapsed     <- stopwatchElapsed _stopwatch
  newPendings <- foldM (checking elapsed) pendings (mapToList pendings)
  atomicWriteIORef _regPendings newPendings
  where
    checking elapsed reg (key, p) =
      case _pendingType p of
        Right _  ->
          -- Not Implemented yet
          return reg
        Left req -> do
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
                    rejectPending p (OperationMaxAttemptReached key cmd)
                    return $ deleteMap key reg
                KeepRetrying -> retry
            else return reg

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
