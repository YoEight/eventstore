{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
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
    , newRegistry
    , register
    , handlePackage
    , abortPendingRequests
    ) where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.ProtocolBuffers
import Data.Serialize
import Data.UUID
import Data.UUID.V4

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Callback
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Pending =
    forall result response. Decode response =>
    Pending { _pendingOp       :: !(Operation result)
            , _pendingRespCmd  :: !Command
            , _pendingRetries  :: !Int
            , _pendingResume   :: response -> SM result ()
            , _pendingCallback :: !(Callback result)
            }

--------------------------------------------------------------------------------
data Registry =
    Registry { _regSettings :: Settings
              , _regBus      :: Bus
              , _regPendings :: IORef (HashMap UUID Pending)
              }

--------------------------------------------------------------------------------
newRegistry :: Settings -> Bus -> IO Registry
newRegistry setts bus = Registry setts bus <$> newIORef mempty

--------------------------------------------------------------------------------
register :: Registry -> Operation a -> Callback a -> IO ()
register reg op cb = evaluate reg op cb op

--------------------------------------------------------------------------------
abortPendingRequests :: Registry -> IO ()
abortPendingRequests Registry{..} = do
  m <- readIORef _regPendings
  atomicWriteIORef _regPendings mempty

  for_ m $ \Pending{..} -> reject _pendingCallback Aborted

--------------------------------------------------------------------------------
handlePackage :: Registry -> Package -> IO ()
handlePackage reg@Registry{..} pkg@Package{..} = do
  m <- readIORef _regPendings

  atomicModifyIORef' _regPendings $ \m ->
    (deleteMap packageCorrelation m, ())

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
      let pkg = Package { packageCmd         = cmdReq
                        , packageCorrelation = uuid
                        , packageData        = runPut $ encodeMessage req
                        , packageCred        = s_credentials _regSettings
                        }

          pending = Pending { _pendingOp       = op
                            , _pendingRespCmd  = cmdResp
                            , _pendingRetries  = 1
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
maybeDecodeMessage :: Decode a => ByteString -> Maybe a
maybeDecodeMessage bytes =
    case runGet decodeMessage bytes of
        Right a -> Just a
        _       -> Nothing
