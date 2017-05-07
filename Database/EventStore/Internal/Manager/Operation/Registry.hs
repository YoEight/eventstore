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
    , newRegistry
    , register
    , handlePackage
    , abortPendingRequests
    , checkAndRetry
    ) where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.ProtocolBuffers
import Data.Serialize
import Data.Time
import Data.UUID
import Data.UUID.V4

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Callback
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.Operation hiding (retry)
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Pending =
    forall result response. Decode response =>
    Pending { _pendingOp       :: !(Operation result)
            , _pendingRespCmd  :: !Command
            , _pendingRetries  :: !Int
            , _pendingLastTry  :: !UTCTime
            , _pendingLastPkg  :: !Package
            , _pendingResume   :: response -> SM result ()
            , _pendingCallback :: !(Callback result)
            }

--------------------------------------------------------------------------------
rejectPending :: Exception e => Pending -> e -> IO ()
rejectPending Pending{..} e = reject _pendingCallback e

--------------------------------------------------------------------------------
data Registry =
    Registry  { _regSettings :: Settings
              , _regBus      :: Publish
              , _regPendings :: IORef (HashMap UUID Pending)
              }

--------------------------------------------------------------------------------
newRegistry :: Settings -> Publish -> IO Registry
newRegistry setts bus = Registry setts bus <$> newIORef mempty

--------------------------------------------------------------------------------
register :: Registry -> Operation a -> Callback a -> IO ()
register reg op cb = evaluate reg op cb op

--------------------------------------------------------------------------------
abortPendingRequests :: Registry -> IO ()
abortPendingRequests Registry{..} = do
  m <- atomicModifyIORef' _regPendings $ \pendings -> (mempty, pendings)

  for_ m $ \Pending{..} -> reject _pendingCallback Aborted

--------------------------------------------------------------------------------
handlePackage :: Registry -> Package -> IO ()
handlePackage reg@Registry{..} pkg@Package{..} = do
  m <- atomicModifyIORef' _regPendings $ \pendings ->
    (deleteMap packageCorrelation pendings, pendings)

  for_ (lookup packageCorrelation m) $ \Pending{..} ->
    case packageCmd of
      cmd
        | cmd == badRequestCmd -> do
          let reason = packageDataAsText pkg

          reject _pendingCallback (ServerError reason)
        | cmd == notAuthenticatedCmd ->
          reject _pendingCallback NotAuthenticatedOp
        | cmd == notHandledCmd -> do
          let Just msg = maybeDecodeMessage packageData
              reason   = getField $ notHandledReason msg
          case reason of
            N_NotMaster -> do
              let Just details = getField $ notHandledAdditionalInfo msg
                  info         = masterInfo details
                  node         = masterInfoNodeEndPoints info

              publish _regBus (ForceReconnect node)
              evaluate reg _pendingOp _pendingCallback _pendingOp
            -- In this case with just retry the operation.
            _ -> evaluate reg _pendingOp _pendingCallback _pendingOp
        | cmd /= _pendingRespCmd ->
          reject _pendingCallback (InvalidServerResponse _pendingRespCmd cmd)
        | otherwise ->
          case runGet decodeMessage packageData of
            Left e ->
              reject _pendingCallback (ProtobufDecodingError e)
            Right resp ->
              evaluate reg _pendingOp _pendingCallback (_pendingResume resp)

--------------------------------------------------------------------------------
evaluate :: Registry
         -> Operation a
         -> Callback a
         -> SM a ()
         -> IO ()
evaluate Registry{..} op cb = go
  where
    go (Return _)  = return ()
    go (Yield a next) = do
      fulfill cb a
      go next
    go (FreshId k) = do
      uuid <- nextRandom
      go (k uuid)
    go (SendPkg cmdReq cmdResp req next) = do
      uuid <- nextRandom
      now  <- getCurrentTime
      let pkg = Package { packageCmd         = cmdReq
                        , packageCorrelation = uuid
                        , packageData        = runPut $ encodeMessage req
                        , packageCred        = s_credentials _regSettings
                        }

          pending = Pending { _pendingOp       = op
                            , _pendingRespCmd  = cmdResp
                            , _pendingRetries  = 1
                            , _pendingLastTry  = now
                            , _pendingLastPkg  = pkg
                            , _pendingResume   = next
                            , _pendingCallback = cb
                            }
      atomicModifyIORef' _regPendings $ \m ->
        (insertMap uuid pending m, ())

      publish _regBus (TcpSend pkg)
    go (Failure outcome) =
      case outcome of
        Just e  -> reject cb e
        Nothing -> go op

--------------------------------------------------------------------------------
-- | Occurs when an operation has been retried more than 's_operationRetry'.
data OperationMaxAttemptReached =
  OperationMaxAttemptReached UUID Command
  deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception OperationMaxAttemptReached

--------------------------------------------------------------------------------
checkAndRetry :: Registry -> IO ()
checkAndRetry Registry{..} = do
  reg    <- readIORef _regPendings
  now    <- getCurrentTime
  newReg <- foldM (checking now) reg (mapToList reg)

  atomicWriteIORef _regPendings newReg
  where
    checking time reg (key, p)
      | diffUTCTime time (_pendingLastTry p) > s_operationTimeout _regSettings =
        let retry = do
              corrId <- nextRandom
              let oldPkg     = _pendingLastPkg p
                  newPkg     = oldPkg { packageCorrelation = corrId }
                  newPending = p { _pendingLastPkg = newPkg
                                 , _pendingLastTry = time
                                 , _pendingRetries = _pendingRetries p + 1
                                 }
                  nextReg    = deleteMap key $ insertMap corrId newPending reg

              publish _regBus (TcpSend newPkg)
              return nextReg in
        case s_operationRetry _regSettings of
          AtMost maxAttempts
            | _pendingRetries p <= maxAttempts
              -> retry
            | otherwise
              -> do let initialCmd = packageCmd (_pendingLastPkg p)
                    rejectPending p
                      (OperationMaxAttemptReached key initialCmd)

                    return $ deleteMap key reg
          KeepRetrying -> retry
      | otherwise = return reg

--------------------------------------------------------------------------------
maybeDecodeMessage :: Decode a => ByteString -> Maybe a
maybeDecodeMessage bytes =
    case runGet decodeMessage bytes of
        Right a -> Just a
        _       -> Nothing
