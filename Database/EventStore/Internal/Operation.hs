{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module    :  Database.EventStore.Internal.Operation
-- Copyright :  (C) 2020 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation where

--------------------------------------------------------------------------------
import Prelude (String)
import Data.ProtocolBuffers
import Data.Serialize (runPut, runGet)

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Control
import Database.EventStore.Internal.Prelude hiding ((.), id)
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
newtype Mailbox = Mailbox (Chan (Either OperationError Package))

--------------------------------------------------------------------------------
mailboxSendPkg :: MonadBase IO m => Mailbox -> Package -> m ()
mailboxSendPkg (Mailbox chan) pkg = writeChan chan (Right pkg)

--------------------------------------------------------------------------------
mailboxFail :: MonadBase IO m => Mailbox -> OperationError -> m ()
mailboxFail (Mailbox chan) e = writeChan chan (Left e)

--------------------------------------------------------------------------------
mailboxRead :: MonadBase IO m => Mailbox -> m (Either OperationError Package)
mailboxRead (Mailbox chan) = readChan chan

--------------------------------------------------------------------------------
mailboxReadDecoded
  :: (MonadBase IO m, Decode resp)
  => Mailbox
  -> m (Either OperationError resp)
mailboxReadDecoded (Mailbox chan)
  = fmap (decodePkg =<<) $ readChan chan

--------------------------------------------------------------------------------
mailboxNew :: MonadBase IO m => m Mailbox
mailboxNew = Mailbox <$> newChan

--------------------------------------------------------------------------------
createPkg
  :: (Encode msg, MonadIO m)
  => Command
  -> Maybe Credentials
  -> msg
  -> m Package
createPkg cmd creds msg
  = do pkgId <- freshUUID
       let dat = runPut $ encodeMessage msg
           pkg
             = Package
               { packageCmd = cmd
               , packageCorrelation = pkgId
               , packageData = dat
               , packageCred = creds
               }

       pure pkg

--------------------------------------------------------------------------------
-- FIXME We could use Bifunctor but can't I am not sure it covers all the GHC
-- we support at that time.
decodePkg :: Decode msg => Package -> Either OperationError msg
decodePkg pkg
  = case runGet decodeMessage (packageData pkg) of
      Left e -> Left $ ProtobufDecodingError e
      Right resp -> Right resp

--------------------------------------------------------------------------------
-- | Operation exception that can occurs on an operation response.
data OperationError
  = WrongExpectedVersion !Text !ExpectedVersion -- ^ Stream and Expected Version
  | StreamDeleted !StreamName                        -- ^ Stream
  | InvalidTransaction
  | forall t. AccessDenied !(StreamId t)                   -- ^ Stream
  | InvalidServerResponse !Command !Command     -- ^ Expected, Found
  | ProtobufDecodingError !String
  | ServerError !(Maybe Text)                  -- ^ Reason
  | InvalidOperation !Text
  | StreamNotFound !StreamName
  | NotAuthenticatedOp
    -- ^ Invalid operation state. If happens, it's a driver bug.
  | Aborted
    -- ^ Occurs when the user asked to close the connection or if the
    --   connection can't reconnect anymore.
  | ConnectionHasDropped
  deriving Typeable

--------------------------------------------------------------------------------
deriving instance Show OperationError

--------------------------------------------------------------------------------
instance Exception OperationError

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
data Lifetime
  = OneTime
  | KeepAlive !Command

--------------------------------------------------------------------------------
data Loop a
  = Loop
  | Break !a

--------------------------------------------------------------------------------
data LoopS s a
  = LoopS !s
  | BreakS !a

--------------------------------------------------------------------------------
keepLooping :: Monad m => m (Loop a) -> m a
keepLooping action
  = go
  where
    go = do result <- action
            case result of
              Loop -> go
              Break a -> pure a

--------------------------------------------------------------------------------
keepLoopingS :: Monad m => s -> (s -> m (LoopS s a)) -> m a
keepLoopingS seed action
  = go seed
  where
    go cur
      = do result <- action cur
           case result of
             LoopS next
               -> go next
             BreakS a
               -> pure a
