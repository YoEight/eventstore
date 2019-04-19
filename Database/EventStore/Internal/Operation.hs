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
  , Code
  , Need(..)
  , Execution(..)
  , Expect(..)
  , Coroutine(..)
  , Payload(..)
  , freshId
  , failure
  , retry
  , send
  , request
  , waitForOr
  , wrongVersion
  , streamDeleted
  , invalidTransaction
  , accessDenied
  , protobufDecodingError
  , serverError
  , invalidServerResponse
  , construct
  , yield
  , traversing
  , stop
  , (<~)
  , unfolding
  , append
  , Stream(..)
  ) where

--------------------------------------------------------------------------------
import Prelude (String)
import Control.Category

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Serialize
import Data.UUID
import Data.Void (Void, absurd)
import Streaming.Internal

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Prelude hiding ((.), id)
import Database.EventStore.Internal.Settings
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
infixr 9 <~

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
data Payload =
  Payload
  { payloadCmd :: !Command
  , payloadData :: !ByteString
  , payloadCreds :: !(Maybe Credentials)
  }

--------------------------------------------------------------------------------
data Coroutine k o a where
  Yield :: o -> a -> Coroutine k o a
  Await :: (i -> a) -> k i -> a -> Coroutine k o a
  Stop  :: Coroutine k o a

--------------------------------------------------------------------------------
instance Functor (Coroutine k o) where
  fmap f (Yield o a)   = Yield o (f a)
  fmap f (Await k n a) = Await (f . k) n (f a)
  fmap _ Stop          = Stop

--------------------------------------------------------------------------------
data Is a b where
  Same :: Is a a

--------------------------------------------------------------------------------
instance Category Is where
  id = Same
  Same . Same = Same

--------------------------------------------------------------------------------
data Need a where
  NeedUUID   :: Need UUID
  NeedRemote :: Payload -> Need Package
  WaitRemote :: UUID -> Need (Maybe Package)

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
type Machine k o m r  = Stream (Coroutine k o) m r
type Code output a    = Machine Need output Execution a
type Operation output = Code output Void
type Process m a b    = Stream (Coroutine (Is a) b) m Void

--------------------------------------------------------------------------------
awaits :: Monad m => k i -> Machine k o m i
awaits instr = Step (Await pure instr (Step Stop))

--------------------------------------------------------------------------------
await :: (Monad m, Category k) => Machine (k i) o m i
await = awaits id

--------------------------------------------------------------------------------
stop :: Machine k o m a
stop = Step Stop

--------------------------------------------------------------------------------
yield :: Monad m => o -> Machine k o m ()
yield o = Step (Yield o (pure ()))

--------------------------------------------------------------------------------
traversing :: Monad m => (a -> m b) -> Process m a b
traversing k = repeatedly $ do
  a <- await
  b <- lift (k a)
  yield b

--------------------------------------------------------------------------------
append :: Operation o -> Operation o -> Operation o
append start right = go start
  where
    go cur =
      case cur of
        Return x  -> absurd x
        Effect m  -> Effect (fmap go m)
        Step step ->
          case step of
            Yield o next ->
              Step $ Yield o (append next right)
            Await k instr failed ->
              Step $ Await (\i -> append (k i) right) instr (append failed right)
            Stop ->
              right

--------------------------------------------------------------------------------
stepping :: Operation a -> Code o (a, Operation a)
stepping = go
  where
    go cur =
      case cur of
        Return x  -> absurd x
        Effect m  -> Effect (fmap go m)
        Step step ->
          case step of
            Yield a next         -> pure (a, next)
            Await k instr failed -> Step $ Await (go . k) instr (go failed)
            Stop                 -> stop

--------------------------------------------------------------------------------
unfolding :: (Maybe a -> Code o (Operation a)) -> Operation o
unfolding k = k Nothing >>= go
  where
    go cur = do
      (a, next) <- stepping cur
      newState  <- k (Just a)
      go (append next newState)

--------------------------------------------------------------------------------
repeatedly :: Functor m => Machine k o m x -> Machine k o m r
repeatedly start = go start
  where
    go (Return _)  = go start
    go (Effect m)  = Effect (fmap go m)
    go (Step step) =
      case step of
        Yield o next         -> Step $ Yield o (go next)
        Await k instr failed -> Step $ Await (go . k) instr (go failed)
        Stop                 -> stop

--------------------------------------------------------------------------------
(<~) :: Monad m => Process m a b -> Machine k a m r -> Machine k b m r
mp <~ mb =
  case mp of
    Return x  -> absurd x
    Effect m  -> Effect (fmap (<~ mb) m)
    Step consumer ->
      case consumer of
        Yield c next        -> Step $ Yield c (next <~ mb)
        Stop                -> stop
        Await k Same failed ->
          case mb of
            Return _      -> failed <~ stop
            Effect m      ->  Effect (fmap (Step consumer <~) m)
            Step producer ->
              case producer of
                Yield b next -> k b <~ next
                Await kb instr kfailed ->
                  Step (Await ((mp <~) . kb) instr (mp <~ kfailed))
                Stop -> failed <~ stop

--------------------------------------------------------------------------------
-- | Asks for a unused 'UUID'.
freshId :: Code o UUID
freshId = Step $ Await pure NeedUUID (Step Stop)

--------------------------------------------------------------------------------
-- | Raises an 'OperationError'.
failure :: OperationError -> Code o a
failure = lift . Failed

--------------------------------------------------------------------------------
-- | Asks to resume the interpretation from the beginning.
retry :: Code o a
retry = lift Retry

--------------------------------------------------------------------------------
-- | Sends a package to the server and wait for a response.
sendRemote :: Payload -> Code o Package
sendRemote p = Step $ Await pure (NeedRemote p) (Step Stop)

--------------------------------------------------------------------------------
-- | Waits package from the server.
waitRemote :: UUID -> Code o (Maybe Package)
waitRemote c = Step $ Await pure (WaitRemote c) (Step Stop)

--------------------------------------------------------------------------------
construct :: Code o a -> Operation o
construct m = m >> stop

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
  let dat     = runPut $ encodeMessage req
      payload = Payload reqCmd dat cred
  pkg <- sendRemote payload
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
  let dat = runPut $ encodeMessage rq
      payload = Payload reqCmd dat cred
  pkg <- sendRemote payload
  runFirstMatch pkg exps

--------------------------------------------------------------------------------
-- | @waitForElse uuid alternative expects@ Waits for a message from the server
--   at the given /uuid/. If the connection has been reset in the meantime, it
--   will use /alternative/.
waitForOr :: UUID -> Code o () -> [Expect o] -> Code o ()
waitForOr pid alt exps =
  waitRemote pid >>= \case
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
