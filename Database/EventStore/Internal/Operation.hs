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
    | forall rq rp. (Encode rq, Decode rp) =>
      SendPkg Word8 Word8 rq (rp -> SM o a)
    | Failure (Maybe OperationError)

--------------------------------------------------------------------------------
instance Functor (SM o) where
    fmap f (Return a)          = Return (f a)
    fmap f (Yield o n)         = Yield o (fmap f n)
    fmap f (FreshId k)         = FreshId (fmap f . k)
    fmap f (SendPkg ci co p k) = SendPkg ci co p (fmap f . k)
    fmap _ (Failure e)         = Failure e

--------------------------------------------------------------------------------
instance Applicative (SM o) where
    pure = return
    (<*>) = ap

--------------------------------------------------------------------------------
instance Monad (SM o) where
    return = Return

    Return a          >>= f = f a
    Yield o n         >>= f = Yield o (n >>= f)
    FreshId k         >>= f = FreshId ((f =<<) . k)
    SendPkg ci co p k >>= f = SendPkg ci co p ((f =<<) . k)
    Failure e         >>= _ = Failure e

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
send :: (Encode rq, Decode rp) => Word8 -> Word8 -> rq -> SM o rp
send ci co rq = SendPkg ci co rq Return

--------------------------------------------------------------------------------
yield :: o -> SM o ()
yield o = Yield o (Return ())

--------------------------------------------------------------------------------
foreach :: SM a x -> (a -> SM b x) -> SM b x
foreach start k = go start
  where
    go (Return x)           = Return x
    go (Yield a n)          = k a >> go n
    go (FreshId ki)         = FreshId (go . ki)
    go (SendPkg ci co p kp) = SendPkg ci co p (go . kp)
    go (Failure e)          = Failure e

--------------------------------------------------------------------------------
type Operation a = SM a ()

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
