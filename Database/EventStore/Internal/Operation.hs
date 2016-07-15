{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE Rank2Types         #-}
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
import ClassyPrelude
import Data.ProtocolBuffers
import Data.UUID

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
      -- ^ Invalid operation state. If happens, it's a driver bug.
    | Aborted
      -- ^ Occurs when the user asked to close the connection or if the
      --   connection can't reconnect anymore.
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception OperationError

--------------------------------------------------------------------------------
-- | Main operation state machine instruction.
data SM o a
    = Return a
      -- ^ Lifts a pure value into the intruction tree. Also marks the end of
      --   an instruction tree.
    | Yield o (SM o a)
      -- ^ Emits an operation return value.
    | FreshId (UUID -> SM o a)
      -- ^ Asks for an unused 'UUID'.
    | forall rq rp. (Encode rq, Decode rp) =>
      SendPkg Command Command rq (rp -> SM o a)
      -- ^ Send a request message given a command and an expected command.
      --   response. It also carries a callback to call when response comes in.
    | Failure (Maybe OperationError)
      -- ^ Ends the instruction interpretation. If holds Nothing, the
      --   interpretation should resume from the beginning. Otherwise it ends
      --   by indicating what went wrong.

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
-- | Asks for a unused 'UUID'.
freshId :: SM o UUID
freshId = FreshId Return

--------------------------------------------------------------------------------
-- | Raises an 'OperationError'.
failure :: OperationError -> SM o a
failure e = Failure $ Just e

--------------------------------------------------------------------------------
-- | Asks to resume the interpretation from the beginning.
retry :: SM o a
retry = Failure Nothing

--------------------------------------------------------------------------------
-- | Sends a request to the server given a command request and response. It
--   returns the expected deserialized message.
send :: (Encode rq, Decode rp) => Command -> Command -> rq -> SM o rp
send ci co rq = SendPkg ci co rq Return

--------------------------------------------------------------------------------
-- | Emits operation return value.
yield :: o -> SM o ()
yield o = Yield o (Return ())

--------------------------------------------------------------------------------
-- | Replaces every emitted value, via 'yield' function by calling the given
--   callback.
foreach :: SM a x -> (a -> SM b x) -> SM b x
foreach start k = go start
  where
    go (Return x)           = Return x
    go (Yield a n)          = k a >> go n
    go (FreshId ki)         = FreshId (go . ki)
    go (SendPkg ci co p kp) = SendPkg ci co p (go . kp)
    go (Failure e)          = Failure e

--------------------------------------------------------------------------------
-- | Maps every emitted value, via 'yield', using given function.
mapOp :: (a -> b) -> SM a () -> SM b ()
mapOp k sm = foreach sm (yield . k)

--------------------------------------------------------------------------------
-- | An operation is just a 'SM' tree.
type Operation a = SM a ()

--------------------------------------------------------------------------------
-- | Raises 'WrongExpectedVersion' exception.
wrongVersion :: Text -> ExpectedVersion -> SM o a
wrongVersion stream ver = failure (WrongExpectedVersion stream ver)

--------------------------------------------------------------------------------
-- | Raises 'StreamDeleted' exception.
streamDeleted :: Text -> SM o a
streamDeleted stream = failure (StreamDeleted stream)

--------------------------------------------------------------------------------
-- | Raises 'InvalidTransaction' exception.
invalidTransaction :: SM o a
invalidTransaction = failure InvalidTransaction

--------------------------------------------------------------------------------
-- | Raises 'AccessDenied' exception.
accessDenied :: StreamName -> SM o a
accessDenied = failure . AccessDenied

--------------------------------------------------------------------------------
-- | Raises 'ProtobufDecodingError' exception.
protobufDecodingError :: String -> SM o a
protobufDecodingError = failure . ProtobufDecodingError

--------------------------------------------------------------------------------
-- | Raises 'ServerError' exception.
serverError :: Maybe Text -> SM o a
serverError = failure . ServerError

--------------------------------------------------------------------------------
-- | Raises 'InvalidServerResponse' exception.
invalidServerResponse :: Command -> Command -> SM o a
invalidServerResponse expe got = failure $ InvalidServerResponse expe got
