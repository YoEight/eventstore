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
  , Ask(..)
  , Serve(..)
  , Operation
  , Code
  , Execution(..)
  , Outcome(..)
  , freshId
  , failure
  , retry
  , send
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
import Pipes
import Pipes.Core

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
data Ask
  = FreshId
  | SendPkg !UUID !Command !Command !ByteString

--------------------------------------------------------------------------------
data Serve
  = Ignore
  | NewId !UUID
  | RespPkg !ByteString

--------------------------------------------------------------------------------
data Outcome a
  = Succeeded a
  | Retry
  | Failed !OperationError

--------------------------------------------------------------------------------
newtype Execution m a = Execution { runExecution :: UUID -> m (Outcome a) }

--------------------------------------------------------------------------------
instance Functor m => Functor (Execution m) where
  fmap f (Execution k) = Execution (fmap go . k)
    where
      go (Succeeded a) = Succeeded (f a)
      go Retry         = Retry
      go (Failed e)    = Failed e

--------------------------------------------------------------------------------
instance Monad m => Applicative (Execution m) where
  pure  = return
  (<*>) = ap

--------------------------------------------------------------------------------
instance Monad m => Monad (Execution m) where
  return a = Execution $ \_ -> return (Succeeded a)

  Execution k >>= f = Execution $ \uuid ->
    k uuid >>= \case
      Retry       -> return Retry
      Failed e    -> return (Failed e)
      Succeeded a -> runExecution (f a) uuid

--------------------------------------------------------------------------------
instance Monad m => MonadReader UUID (Execution m) where
  ask = Execution (return . Succeeded)

  local f m = Execution $ \uuid -> runExecution m (f uuid)

--------------------------------------------------------------------------------
instance MonadIO m => MonadIO (Execution m) where
  liftIO m = Execution $ \_ -> Succeeded <$> liftIO m

--------------------------------------------------------------------------------
type Operation output = forall m. Monad m => MachineT (Execution m) Need output

--------------------------------------------------------------------------------
data Need a where
  NeedId     :: Need UUID
  NeedRemote :: UUID -> Command -> Command -> ByteString -> Need ByteString

--------------------------------------------------------------------------------
-- | Instruction that composed an 'Operation'.
type Code' m o a = PlanT Need o (Execution m) a

--------------------------------------------------------------------------------
type Code o a  = forall m. Monad m => Code' m o a

--------------------------------------------------------------------------------
-- | Asks for a unused 'UUID'.
freshId :: Code o UUID
freshId = awaits NeedId

--------------------------------------------------------------------------------
-- | Raises an 'OperationError'.
failure :: OperationError -> Code o a
failure e = lift $ Execution $ \_ -> return (Failed e)

--------------------------------------------------------------------------------
-- | Asks to resume the interpretation from the beginning.
retry :: Code o a
retry = lift $ Execution $ \_ -> return Retry

--------------------------------------------------------------------------------
-- | Sends a Ask to the server given a command Ask and response. It
--   returns the expected deserialized message.
send :: (Encode req, Decode resp)
     => Command
     -> Command
     -> req
     -> Code o resp
send ci co rq = do
  selfId <- ask
  let reqPayload = runPut $ encodeMessage rq
  respPayload <- awaits $ NeedRemote selfId ci co reqPayload
  case runGet decodeMessage respPayload of
    Left e     -> failure (ProtobufDecodingError e)
    Right resp -> return resp

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
