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
import System.Random

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
type Pendings = HashMap UUID Pending

--------------------------------------------------------------------------------
rejectPending :: Exception e => Pending -> e -> IO ()
rejectPending Pending{..} e = reject _pendingCallback e

--------------------------------------------------------------------------------
data Registry =
    Registry  { _regSettings :: Settings
              , _regBus      :: Publish
              , _regPendings :: TVar Pendings
              }

--------------------------------------------------------------------------------
newRegistry :: Settings -> Publish -> IO Registry
newRegistry setts bus = Registry setts bus <$> newTVarIO mempty

--------------------------------------------------------------------------------
register :: Registry -> Operation a -> Callback a -> IO ()
register reg op cb = do
  gen <- newStdGen
  -- Beware as there is no proof that this time value will be used right away.
  -- This STM transaction could take more time to be validated. However we
  -- don't need to be very precise in this case. This is just an implementation
  -- note.
  now <- getCurrentTime
  action <- atomically $ evaluate reg (randoms gen) now op cb op
  action

--------------------------------------------------------------------------------
abortPendingRequests :: Registry -> IO ()
abortPendingRequests Registry{..} = do
  m <- atomically $ do
    pendings <- readTVar _regPendings
    writeTVar _regPendings mempty
    return pendings

  for_ m $ \Pending{..} -> reject _pendingCallback Aborted

--------------------------------------------------------------------------------
handlePackage :: Registry -> Package -> IO ()
handlePackage reg@Registry{..} pkg@Package{..} = do
  gen <- newStdGen
  -- Beware as there is no proof that this time value will be used right away.
  -- This STM transaction could take more time to be validated. However we
  -- don't need to be very precise in this case. This is just an implementation
  -- note.
  now <- getCurrentTime
  action <- atomically $ do
    pendings <- readTVar _regPendings
    writeTVar _regPendings (deleteMap packageCorrelation pendings)
    let mPending = lookup packageCorrelation pendings
    outcome <- traverse (handlePackageSTM reg (randoms gen) now pkg) mPending
    return $ fold outcome

  action

--------------------------------------------------------------------------------
handlePackageSTM :: Registry
                 -> [UUID]
                 -> UTCTime
                 -> Package
                 -> Pending
                 -> STM (IO ())
handlePackageSTM reg@Registry{..} uuids now pkg@Package{..} Pending{..} =
  case packageCmd of
    cmd | cmd == badRequestCmd -> do
            let reason = packageDataAsText pkg

            return $ reject _pendingCallback (ServerError reason)
        | cmd == notAuthenticatedCmd ->
            return $ reject _pendingCallback NotAuthenticatedOp
        | cmd == notHandledCmd -> do
            let Just msg = maybeDecodeMessage packageData
                reason   = getField $ notHandledReason msg
            case reason of
              N_NotMaster -> do
                let Just details = getField $ notHandledAdditionalInfo msg
                    info         = masterInfo details
                    node         = masterInfoNodeEndPoints info
                    action       = publish _regBus (ForceReconnect node)

                end <- evaluate reg uuids now _pendingOp _pendingCallback _pendingOp
                return (action >> end)
              -- In this case with just retry the operation.
              _ -> evaluate reg uuids now _pendingOp _pendingCallback _pendingOp
        | cmd /= _pendingRespCmd ->
            let resp = InvalidServerResponse _pendingRespCmd cmd in
            return $ reject _pendingCallback resp
        | otherwise ->
            case runGet decodeMessage packageData of
              Left e ->
                return $ reject _pendingCallback (ProtobufDecodingError e)
              Right resp ->
                evaluate reg uuids now _pendingOp _pendingCallback
                  (_pendingResume resp)

--------------------------------------------------------------------------------
evaluate :: Registry
         -> [UUID]
         -> UTCTime
         -> Operation a
         -> Callback a
         -> SM a ()
         -> STM (IO ())
evaluate Registry{..} uuids now op cb = go uuids (return ())
  where
    go _ acc (Return _) = return acc
    go ids acc (Yield a next) =
      go ids (acc >> fulfill cb a) next
    go (uuid:ids) acc (FreshId k) =
      go ids acc (k uuid)
    go (uuid:_) acc (SendPkg cmdReq cmdResp req next) = do
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

      pendings <- readTVar _regPendings
      writeTVar _regPendings (insertMap uuid pending pendings)

      return (acc >> publish _regBus (TcpSend pkg))
    go ids acc (Failure outcome) =
      case outcome of
        Just e  -> return $ reject cb e
        Nothing -> go ids acc op

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
  gen <- newStdGen
  -- Beware as there is no proof that this time value will be used right away.
  -- This STM transaction could take more time to be validated. However we
  -- don't need to be very precise in this case. This is just an implementation
  -- note.
  now <- getCurrentTime
  action <- atomically $ do
    pendings <- readTVar _regPendings
    checking (return ()) (randoms gen) now (mapToList pendings)

  action
  where
    checking acc _ _ [] = return acc
    checking acc ids@(corrId:otherIds) now ((key, p):rest)
      | diffUTCTime now (_pendingLastTry p) >= s_operationTimeout _regSettings =
        let retry = do
              pendings <- readTVar _regPendings
              let oldPkg     = _pendingLastPkg p
                  newPkg     = oldPkg { packageCorrelation = corrId }
                  newPending = p { _pendingLastPkg = newPkg
                                 , _pendingLastTry = now
                                 , _pendingRetries = _pendingRetries p + 1
                                 }

                  newPendings =
                    deleteMap key $ insertMap corrId newPending pendings

                  action = publish _regBus (TcpSend newPkg)

              writeTVar _regPendings newPendings
              checking (acc >> action) otherIds now rest in
        case s_operationRetry _regSettings of
          AtMost maxAttempts
            | _pendingRetries p <= maxAttempts
              -> retry
            | otherwise
              -> do let initialCmd = packageCmd (_pendingLastPkg p)
                        action = rejectPending p
                                   (OperationMaxAttemptReached key initialCmd)

                    pendings <- readTVar _regPendings
                    writeTVar _regPendings (deleteMap key pendings)
                    checking (acc >> action) ids now rest
          KeepRetrying -> retry
      | otherwise = checking acc ids now rest

--------------------------------------------------------------------------------
maybeDecodeMessage :: Decode a => ByteString -> Maybe a
maybeDecodeMessage bytes =
    case runGet decodeMessage bytes of
        Right a -> Just a
        _       -> Nothing
