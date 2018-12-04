{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
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
  , Expect(..)
  , freshId
  , failure
  , retry
  , send
  , request
  , waitFor
  , waitForOr
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
import Prelude (String)

--------------------------------------------------------------------------------
import Data.Machine
import Data.ProtocolBuffers
import Data.Serialize
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Settings
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
    | StreamDeleted StreamName                        -- ^ Stream
    | InvalidTransaction
    | forall t. AccessDenied (StreamId t)                   -- ^ Stream
    | InvalidServerResponse Command Command     -- ^ Expected, Found
    | ProtobufDecodingError String
    | ServerError (Maybe Text)                  -- ^ Reason
    | InvalidOperation Text
    | StreamNotFound StreamName
    | NotAuthenticatedOp
      -- ^ Invalid operation state. If happens, it's a driver bug.
    | Aborted
      -- ^ Occurs when the user asked to close the connection or if the
      --   connection can't reconnect anymore.
    deriving Typeable

--------------------------------------------------------------------------------
deriving instance Show OperationError

--------------------------------------------------------------------------------
instance Exception OperationError

--------------------------------------------------------------------------------
data Execution a
  = Proceed a
  | Retry
  | Failed !OperationError

--------------------------------------------------------------------------------
instance Functor Execution where
  fmap f (Proceed a) = Proceed (f a)
  fmap _ Retry       = Retry
  fmap _ (Failed e)  = Failed e

--------------------------------------------------------------------------------
instance Applicative Execution where
  pure = return
  (<*>) = ap

--------------------------------------------------------------------------------
instance Monad Execution where
  return = Proceed

  Proceed a >>= f = f a
  Retry     >>= _ = Retry
  Failed e  >>= _ = Failed e

--------------------------------------------------------------------------------
type Operation output = MachineT Execution Need output

--------------------------------------------------------------------------------
data Need a where
  NeedUUID   :: Need UUID
  NeedRemote :: Command -> ByteString -> Maybe Credentials -> Need Package
  WaitRemote :: UUID -> Need (Maybe Package)

--------------------------------------------------------------------------------
-- | Instruction that composed an 'Operation'.
type Code o a = PlanT Need o Execution a

--------------------------------------------------------------------------------
-- | Asks for a unused 'UUID'.
freshId :: Code o UUID
freshId = awaits NeedUUID

--------------------------------------------------------------------------------
-- | Raises an 'OperationError'.
failure :: OperationError -> Code o a
failure = lift . Failed

--------------------------------------------------------------------------------
-- | Asks to resume the interpretation from the beginning.
retry :: Code o a
retry = lift Retry

--------------------------------------------------------------------------------
-- | Like 'request' except it discards the correlation id of the network
--   exchange.
send :: (Encode req, Decode resp)
     => Command
     -> Command
     -> Maybe Credentials
     -> req
     -> Code o resp
send reqCmd expCmd cred req = do
  let payload = runPut $ encodeMessage req
  pkg <- awaits $ NeedRemote reqCmd payload cred
  let gotCmd = packageCmd pkg

  when (gotCmd /= expCmd)
    (invalidServerResponse expCmd gotCmd)

  case runGet decodeMessage (packageData pkg) of
    Left e     -> protobufDecodingError e
    Right resp -> return resp

--------------------------------------------------------------------------------
data Expect o where
  Expect :: Decode resp => Command -> (UUID -> resp -> Code o ()) -> Expect o

--------------------------------------------------------------------------------
-- | Runs the first expection that matches.
runFirstMatch :: Package -> [Expect o] -> Code o ()
runFirstMatch _ [] = invalidOperation "No expectation was fulfilled"
runFirstMatch pkg (Expect cmd k:rest)
  | packageCmd pkg /= cmd = runFirstMatch pkg rest
  | otherwise =
    case runGet decodeMessage (packageData pkg) of
      Left e     -> protobufDecodingError e
      Right resp -> k (packageCorrelation pkg) resp

--------------------------------------------------------------------------------
-- | Sends a message to remote server. It returns the expected deserialized
--   message along with the correlation id of the network exchange.
request :: Encode req
        => Command
        -> Maybe Credentials
        -> req
        -> [Expect o]
        -> Code o ()
request reqCmd cred rq exps = do
  let payload = runPut $ encodeMessage rq
  pkg <- awaits $ NeedRemote reqCmd payload cred
  runFirstMatch pkg exps

--------------------------------------------------------------------------------
-- | Like 'waitForOr' but will 'stop' if the connection reset.
waitFor :: UUID -> [Expect o] -> Code o ()
waitFor pid exps = waitForOr pid stop exps

--------------------------------------------------------------------------------
-- | @waitForElse uuid alternative expects@ Waits for a message from the server
--   at the given /uuid/. If the connection has been reset in the meantime, it
--   will use /alternative/.
waitForOr :: UUID -> (Code o ()) -> [Expect o] -> Code o ()
waitForOr pid alt exps =
  awaits (WaitRemote pid) >>= \case
    Nothing  -> alt
    Just pkg ->
      runFirstMatch pkg exps

--------------------------------------------------------------------------------
-- | Raises 'WrongExpectedVersion' exception.
wrongVersion :: Text -> ExpectedVersion -> Code o a
wrongVersion stream ver = failure (WrongExpectedVersion stream ver)

--------------------------------------------------------------------------------
-- | Raises 'StreamDeleted' exception.
streamDeleted :: StreamName -> Code o a
streamDeleted stream = failure (StreamDeleted stream)

--------------------------------------------------------------------------------
-- | Raises 'InvalidTransaction' exception.
invalidTransaction :: Code o a
invalidTransaction = failure InvalidTransaction

--------------------------------------------------------------------------------
-- | Raises 'AccessDenied' exception.
accessDenied :: StreamId t -> Code o a
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

--------------------------------------------------------------------------------
invalidOperation :: Text -> Code o a
invalidOperation = failure . InvalidOperation
