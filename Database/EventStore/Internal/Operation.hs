{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types     #-}
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
module Database.EventStore.Internal.Operation where

--------------------------------------------------------------------------------
import Data.ByteString
import Data.ProtocolBuffers
import Data.Serialize
import Data.Text
import Data.UUID
import Data.Word

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Mode
    = Init
    | Pending
    | Completed

--------------------------------------------------------------------------------
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
data OperationError
    = WrongExpectedVersion Text ExpectedVersion -- ^ Stream and Expected Version
    | StreamDeleted Text                        -- ^ Stream
    | InvalidTransaction
    | AccessDenied StreamName                         -- ^ Stream
    | InvalidServerResponse Word8 Word8         -- ^ Expected, Found
    | ProtobufDecodingError String
    | ServerError (Maybe Text)                  -- ^ Reason
    | InvalidOperation Text
    deriving Show

--------------------------------------------------------------------------------
data Report r
    = Retry (Operation 'Init r)
    | Error OperationError
    | Success r

--------------------------------------------------------------------------------
type PendingOrCompleted r =
    Either (Operation 'Pending r) (Operation 'Completed r)

--------------------------------------------------------------------------------
data Input :: Mode -> * -> * -> * where
    Create  :: UUID -> Input 'Init r (Package, Operation 'Pending r)
    Arrived :: Package -> Input 'Pending r (PendingOrCompleted r)
    Report  :: Input 'Completed r (Report r)

--------------------------------------------------------------------------------
newtype Operation m r = Operation (forall a. Input m r a -> a)

--------------------------------------------------------------------------------
createPackage :: UUID -> Operation 'Init r -> (Package, Operation 'Pending r)
createPackage u (Operation k) = k (Create u)

--------------------------------------------------------------------------------
packageArrived :: Package
               -> Operation 'Pending r
               -> Either (Operation 'Pending r) (Operation 'Completed r)
packageArrived pkg (Operation k) = k (Arrived pkg)

--------------------------------------------------------------------------------
getReport :: Operation 'Completed r -> Report r
getReport (Operation k) = k Report

--------------------------------------------------------------------------------
errored :: OperationError -> Operation 'Completed r
errored e = Operation $ \Report -> Error e

--------------------------------------------------------------------------------
success :: r -> Operation 'Completed r
success r = Operation $ \Report -> Success r

--------------------------------------------------------------------------------
decodeResp :: Decode m
           => ByteString
           -> (m -> Operation 'Completed r)
           -> Operation 'Completed r
decodeResp bytes k =
    case runGet decodeMessage bytes of
        Left e  -> protobufDecodingError e
        Right m -> k m

--------------------------------------------------------------------------------
retry :: (forall a. Input 'Init r a -> a) -> Operation 'Completed r
retry k = Operation $ \Report -> Retry $ Operation k

--------------------------------------------------------------------------------
wrongVersion :: Text -> ExpectedVersion -> Operation 'Completed r
wrongVersion stream ver = errored (WrongExpectedVersion stream ver)

--------------------------------------------------------------------------------
streamDeleted :: Text -> Operation 'Completed r
streamDeleted stream = errored (StreamDeleted stream)

--------------------------------------------------------------------------------
invalidTransaction :: Operation 'Completed r
invalidTransaction = errored InvalidTransaction

--------------------------------------------------------------------------------
accessDenied :: StreamName -> Operation 'Completed r
accessDenied stream = errored . AccessDenied

--------------------------------------------------------------------------------
protobufDecodingError :: String -> Operation 'Completed r
protobufDecodingError = errored . ProtobufDecodingError

--------------------------------------------------------------------------------
serverError :: Maybe Text -> Operation 'Completed r
serverError = errored . ServerError
