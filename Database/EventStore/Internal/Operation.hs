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
import Control.Exception
import Control.Monad

--------------------------------------------------------------------------------
import Data.ByteString
import Data.ProtocolBuffers
import Data.Serialize
import Data.Text
import Data.UUID
import Data.Word

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

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
    | AccessDenied StreamName                   -- ^ Stream
    | InvalidServerResponse Word8 Word8         -- ^ Expected, Found
    | ProtobufDecodingError String
    | ServerError (Maybe Text)                  -- ^ Reason
    | InvalidOperation Text
      -- ^ Invalid operation state. If happens, it's a driver bug.
    deriving Show

--------------------------------------------------------------------------------
instance Exception OperationError

--------------------------------------------------------------------------------
data SM o a
    = Return a
    | Yield o (SM o a)
    | FreshId (UUID -> SM o a)
    | SendPkg Package (Package -> SM o a)
    | Failure (Maybe OperationError)
    | forall b. NewOp (forall r. Either OperationError b -> r) (UUID -> SM b ())

--------------------------------------------------------------------------------
instance Functor (SM o) where
    fmap f (Return a)    = Return (f a)
    fmap f (Yield o n)   = Yield o (fmap f n)
    fmap f (FreshId k)   = FreshId (fmap f . k)
    fmap f (SendPkg p k) = SendPkg p (fmap f . k)
    fmap _ (Failure e)   = Failure e
    fmap _ (NewOp cb op) = NewOp cb op

--------------------------------------------------------------------------------
instance Applicative (SM o) where
    pure = return
    (<*>) = ap

--------------------------------------------------------------------------------
instance Monad (SM o) where
    return = Return

    Return a    >>= f = f a
    Yield o n   >>= f = Yield o (n >>= f)
    FreshId k   >>= f = FreshId ((f =<<) . k)
    SendPkg p k >>= f = SendPkg p ((f =<<) . k)
    Failure e   >>= _ = Failure e
    NewOp cb op >>= _ = NewOp cb op

--------------------------------------------------------------------------------
freshId :: SM o UUID
freshId = FreshId Return

--------------------------------------------------------------------------------
failure :: OperationError -> SM o a
failure e = Failure $ Just e

--------------------------------------------------------------------------------
retry :: SM o a
retry = Failure Nothing

--------------------------------------------------------------------------------
send :: Package -> SM o Package
send pkg = SendPkg pkg Return

--------------------------------------------------------------------------------
yield :: o -> SM o ()
yield o = Yield o (Return ())

--------------------------------------------------------------------------------
newOp :: (forall r. Either OperationError b -> r) -> (UUID -> SM b ()) -> SM o a
newOp cb k = NewOp cb k

--------------------------------------------------------------------------------
foreach :: SM a x -> (a -> SM b x) -> SM b x
foreach start k = go start
  where
    go (Return x)     = Return x
    go (Yield a n)    = k a >> go n
    go (FreshId ki)   = FreshId (go . ki)
    go (SendPkg p kp) = SendPkg p (go . kp)
    go (Failure e)    = Failure e
    go (NewOp cb op)  = NewOp cb op

--------------------------------------------------------------------------------
newtype Operation a = Operation { applyOp :: UUID -> SM a () }

--------------------------------------------------------------------------------
operation :: (UUID -> SM a ()) -> Operation a
operation = Operation

--------------------------------------------------------------------------------
decodeResp :: Decode a => ByteString -> SM o a
decodeResp bytes =
    case runGet decodeMessage bytes of
        Left e  -> failure $ ProtobufDecodingError e
        Right m -> return m

--------------------------------------------------------------------------------
wrongVersion :: Text -> ExpectedVersion -> SM o a
wrongVersion stream ver = failure (WrongExpectedVersion stream ver)

--------------------------------------------------------------------------------
streamDeleted :: Text -> SM o a
streamDeleted stream = failure (StreamDeleted stream)

--------------------------------------------------------------------------------
invalidTransaction :: SM o a
invalidTransaction = failure InvalidTransaction

--------------------------------------------------------------------------------
accessDenied :: StreamName -> SM o a
accessDenied = failure . AccessDenied

--------------------------------------------------------------------------------
protobufDecodingError :: String -> SM o a
protobufDecodingError = failure . ProtobufDecodingError

--------------------------------------------------------------------------------
serverError :: Maybe Text -> SM o a
serverError = failure . ServerError

--------------------------------------------------------------------------------
invalidServerResponse :: Word8 -> Word8 -> SM o a
invalidServerResponse expe got = failure $ InvalidServerResponse expe got
