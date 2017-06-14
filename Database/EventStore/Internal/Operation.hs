{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation
  ( OpResult(..)
  , OperationError(..)
  , Operation
  , Need(..)
  , Code
  , Execution(..)
  , Outcome(..)
  , freshId
  , failure
  , retry
  , send
  , request
  , requestEither
  , waitFor
  , waitForEither
  , wrongVersion
  , streamDeleted
  , invalidTransaction
  , accessDenied
  , protobufDecodingError
  , serverError
  , invalidServerResponse
  , module Data.Machine
  ) where

--------------------------------------------------------------------------------
import ClassyPrelude
import Control.Monad.Reader (local)
import Data.Machine
import Data.ProtocolBuffers
import Data.Serialize
import Data.UUID
import Data.UUID.V4

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Operation result sent by the server.
data OpResult
    = OP_SUCCESS
    | OP_PREPARE_TIMEOUT
    | OP_COMMIT_TIMEOUT
    | OP_FORWARD_TIMEOUT
    | OP_WRONG_EXPECTED_VERSION
    | OP_STREAM_DELETED
    | OP_INVALID_TRANSACTION
    | OP_ACCESS_DENIED
    deriving (Eq, Enum, Show)

--------------------------------------------------------------------------------
-- | Operation exception that can occurs on an operation response.
data OperationError
    = WrongExpectedVersion Text ExpectedVersion -- ^ Stream and Expected Version
    | StreamDeleted Text                        -- ^ Stream
    | InvalidTransaction
    | AccessDenied StreamName                   -- ^ Stream
    | InvalidServerResponse Command Command     -- ^ Expected, Found
    | ProtobufDecodingError String
    | ServerError (Maybe Text)                  -- ^ Reason
    | InvalidOperation Text
    | NotAuthenticatedOp
      -- ^ Invalid operation state. If happens, it's a driver bug.
    | Aborted
      -- ^ Occurs when the user asked to close the connection or if the
      --   connection can't reconnect anymore.
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception OperationError

--------------------------------------------------------------------------------
data Outcome a
  = Succeeded a
  | Retry
  | Failed !OperationError

--------------------------------------------------------------------------------
newtype Execution a = Execution { runExecution :: UUID -> IO (Outcome a) }

--------------------------------------------------------------------------------
instance Functor Execution where
  fmap f (Execution k) = Execution (fmap go . k)
    where
      go (Succeeded a) = Succeeded (f a)
      go Retry         = Retry
      go (Failed e)    = Failed e

--------------------------------------------------------------------------------
instance Applicative Execution where
  pure  = return
  (<*>) = ap

--------------------------------------------------------------------------------
instance Monad Execution where
  return a = Execution $ \_ -> return (Succeeded a)

  Execution k >>= f = Execution $ \uuid ->
    k uuid >>= \case
      Retry       -> return Retry
      Failed e    -> return (Failed e)
      Succeeded a -> runExecution (f a) uuid

--------------------------------------------------------------------------------
instance MonadReader UUID Execution where
  ask = Execution (return . Succeeded)

  local f m = Execution $ \uuid -> runExecution m (f uuid)

--------------------------------------------------------------------------------
instance MonadIO Execution where
  liftIO m = Execution $ \_ -> Succeeded <$> liftIO m

--------------------------------------------------------------------------------
type Operation output = MachineT Execution Need output

--------------------------------------------------------------------------------
data Need a where
  NeedRemote :: Command -> ByteString -> Need Package
  WaitRemote :: UUID -> Need (Maybe Package)

--------------------------------------------------------------------------------
-- | Instruction that composed an 'Operation'.
type Code o a = PlanT Need o Execution a

--------------------------------------------------------------------------------
-- | Asks for a unused 'UUID'.
freshId :: Code o UUID
freshId = liftIO nextRandom

--------------------------------------------------------------------------------
-- | Raises an 'OperationError'.
failure :: OperationError -> Code o a
failure e = lift $ Execution $ \_ -> return (Failed e)

--------------------------------------------------------------------------------
-- | Asks to resume the interpretation from the beginning.
retry :: Code o a
retry = lift $ Execution $ \_ -> return Retry

--------------------------------------------------------------------------------
-- | Like 'request' except it discards the correlation id of the network
--   exchange.
send :: (Encode req, Decode resp)
     => Command
     -> Command
     -> req
     -> Code o resp
send reqCmd expCmd req = snd <$> request reqCmd expCmd req

--------------------------------------------------------------------------------
-- | Sends a message to remote server. It returns the expected deserialized
--   message along with the correlation id of the network exchange.
request :: (Encode req, Decode resp)
        => Command
        -> Command
        -> req
        -> Code o (UUID, resp)
request reqCmd expCmd rq = do
  let payload = runPut $ encodeMessage rq
  pkg <- awaits $ NeedRemote reqCmd payload
  let gotCmd = packageCmd pkg

  when (gotCmd /= expCmd)
    (invalidServerResponse expCmd gotCmd)

  case runGet decodeMessage (packageData pkg) of
    Left e     -> protobufDecodingError e
    Right resp -> return (packageCorrelation pkg, resp)

--------------------------------------------------------------------------------
-- | Sends a message to remote server. It returns one of expected deserialized
--   messages along with the correlation id of the network exchange.
requestEither :: (Encode req, Decode resp1, Decode resp2)
              => Command
              -> req
              -> Code o (UUID, Either resp1 resp2)
requestEither reqCmd rq = do
  let payload = runPut $ encodeMessage rq
  pkg <- awaits $ NeedRemote reqCmd payload

  case runGet decodeMessage (packageData pkg) of
    Left e ->
      case runGet decodeMessage (packageData pkg) of
        Left e      -> protobufDecodingError e
        Right resp2 -> return (packageCorrelation pkg, Right resp2)
    Right resp1 -> return (packageCorrelation pkg, Left resp1)

--------------------------------------------------------------------------------
-- | Waits for a message from the server at the given correlation id. If the
--   the connection has been reset in the meantime, it will emit a 'stop'.
waitFor :: Decode resp => UUID -> Code o resp
waitFor pid =
  awaits (WaitRemote pid) >>= \case
    Nothing  -> stop
    Just pkg ->
      case runGet decodeMessage (packageData pkg) of
        Left e     -> protobufDecodingError e
        Right resp -> return resp

--------------------------------------------------------------------------------
-- | Waits for a message from the server at the given correlation id for either
--   'Decode' value type. If the connection has been reset in the meantime,
--   it will emit a 'stop'.
waitForEither :: (Decode resp1, Decode resp2)
              => UUID
              -> Code o (Either resp1 resp2)
waitForEither pid =
  awaits (WaitRemote pid) >>= \case
    Nothing  -> stop
    Just pkg ->
      case runGet decodeMessage (packageData pkg) of
        Left _ ->
          case runGet decodeMessage (packageData pkg) of
            Left e     -> protobufDecodingError e
            Right resp -> return (Right resp)
        Right resp1 -> return (Left resp1)

--------------------------------------------------------------------------------
-- | Raises 'WrongExpectedVersion' exception.
wrongVersion :: Text -> ExpectedVersion -> Code o a
wrongVersion stream ver = failure (WrongExpectedVersion stream ver)

--------------------------------------------------------------------------------
-- | Raises 'StreamDeleted' exception.
streamDeleted :: Text -> Code o a
streamDeleted stream = failure (StreamDeleted stream)

--------------------------------------------------------------------------------
-- | Raises 'InvalidTransaction' exception.
invalidTransaction :: Code o a
invalidTransaction = failure InvalidTransaction

--------------------------------------------------------------------------------
-- | Raises 'AccessDenied' exception.
accessDenied :: StreamName -> Code oconcat a
accessDenied = failure . AccessDenied

--------------------------------------------------------------------------------
-- | Raises 'ProtobufDecodingError' exception.
protobufDecodingError :: String -> Code o a
protobufDecodingError = failure . ProtobufDecodingError

--------------------------------------------------------------------------------
-- | Raises 'ServerError' exception.
serverError :: Maybe Text -> Code o a
serverError = failure . ServerError

--------------------------------------------------------------------------------
-- | Raises 'InvalidServerResponse' exception.
invalidServerResponse :: Command -> Command -> Code o a
invalidServerResponse expe got = failure $ InvalidServerResponse expe got
