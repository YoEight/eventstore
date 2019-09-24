{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module    :  Database.EventStore.Internal.Driver
-- Copyright :  (C) 2019 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Driver where

--------------------------------------------------------------------------------
import Control.Exception (Exception)
import Control.Monad (forever, when, foldM, filterM)
import Control.Monad.Loops (iterateM_)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.ProtocolBuffers (Decode, encodeMessage, decodeMessage, getField)
import Data.Serialize (runPut, runGet)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Data.UUID (UUID)
import Prelude
import Data.String.Interpolate.IsString (i)

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.EndPoint
import Database.EventStore.Internal.Operation (Operation, OperationError(..))
import qualified Database.EventStore.Internal.Operation.Identify as Identify
import Database.EventStore.Internal.Settings
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
newtype PackageId = PackageId UUID deriving (Show, Ord, Eq, Hashable)

--------------------------------------------------------------------------------
newtype ConnectionId = ConnectionId UUID deriving (Show, Ord, Eq, Hashable)

--------------------------------------------------------------------------------
data Connection m =
  Connection
  { connectionId :: ConnectionId
  , enqueuePackage :: Package -> m ()
  , closeConnection :: ConnectionError -> m ()
  }

--------------------------------------------------------------------------------
instance Eq (Connection m) where
  a == b = connectionId a == connectionId b

--------------------------------------------------------------------------------
instance Ord (Connection m) where
  compare a b = compare (connectionId a) (connectionId b)

--------------------------------------------------------------------------------
instance Show (Connection m) where
  show a = show (connectionId a)

--------------------------------------------------------------------------------
data ConnectingState m
  = Reconnecting
  | EndpointDiscovery
  | ConnectionEstablishing (Connection m)
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
data StopReason
  = OfflineError OfflineError
  | OnlineError ConnectionId OnlineError

--------------------------------------------------------------------------------
data OfflineError
  = ConnectionMaxAttemptReached

--------------------------------------------------------------------------------
data OnlineError

--------------------------------------------------------------------------------
data ConnectionError
  = IdentificationFailure
  | forall e. Exception e => FatalConnectionError e

--------------------------------------------------------------------------------
data Driver m =
  Driver
  { connect :: EndPoint -> m (Connection m)
  , forceReconnect :: UUID -> NodeEndPoints -> m (Connection m)
  , generateId :: m UUID
  , discover :: m ()
  , getElapsedTime :: m NominalDiffTime
  , stop :: StopReason -> m ()
  , getSettings :: m Settings
  , output :: Transmission -> m ()
  }

--------------------------------------------------------------------------------
data TransmissionError
  = ConnectionDropped
  | ConnectionClosed
  | MaxRetriesReached
  | BadRequest (Maybe Text)
  | AuthenticationNeeded
  deriving (Eq, Show)

--------------------------------------------------------------------------------
data BadNews =
  BadNews
  { badNewsId :: UUID
  , badNewsError :: TransmissionError
  } deriving (Show)

--------------------------------------------------------------------------------
makeBadNews :: TransmissionError -> Package -> BadNews
makeBadNews e pkg =
  BadNews
  { badNewsId = packageCorrelation pkg
  , badNewsError = e
  }

--------------------------------------------------------------------------------
reportBadNews :: Driver m
              -> TransmissionError
              -> Exchange
              -> m ()
reportBadNews self e =
  output self . Recv . Left . makeBadNews e . exchangeRequest

--------------------------------------------------------------------------------
reportBadNews_ :: Driver m
               -> TransmissionError
               -> Package
               -> m ()
reportBadNews_ self e =
  output self . Recv . Left . makeBadNews e

--------------------------------------------------------------------------------
reportResponse :: Driver m -> Package -> m ()
reportResponse self = output self . Recv . Right

--------------------------------------------------------------------------------
sendPkg :: Connection m -> Package -> m ()
sendPkg self = enqueuePackage self

--------------------------------------------------------------------------------
ignorePkg :: Driver m -> Package -> m ()
ignorePkg self = output self . Ignored

--------------------------------------------------------------------------------
data Exchange =
  Exchange
  { exchangeRetry :: Int
  , exchangeRequest :: Package
  , exchangeStarted :: NominalDiffTime
  }

--------------------------------------------------------------------------------
exchangePkgId :: Exchange -> UUID
exchangePkgId = packageCorrelation . exchangeRequest

--------------------------------------------------------------------------------
makeExchange :: Functor m => Driver m -> Package -> m Exchange
makeExchange self pkg = Exchange 0 pkg <$> getElapsedTime self

--------------------------------------------------------------------------------
data Await =
  Await
  { awaitRetry :: Int
  , awaitPackage :: Package
  }

--------------------------------------------------------------------------------
data ConnectionAttempt =
  ConnectionAttempt
  { connectionAttemptRetry :: Int
  , connectionAttemptStarted :: NominalDiffTime
  }

--------------------------------------------------------------------------------
createConnectionAttempt :: Monad m => Driver m -> m ConnectionAttempt
createConnectionAttempt self = do
  started <- getElapsedTime self
  let att =
        ConnectionAttempt
        { connectionAttemptRetry = 0
        , connectionAttemptStarted = started
        }

  pure att

--------------------------------------------------------------------------------
exchangeToAwait :: Exchange -> Await
exchangeToAwait e =
  Await
  { awaitRetry = exchangeRetry e
  , awaitPackage = exchangeRequest e
  }

--------------------------------------------------------------------------------
pkgToAwait :: Package -> Await
pkgToAwait pkg =
  Await
  { awaitRetry = 0
  , awaitPackage = pkg
  }

--------------------------------------------------------------------------------
type Reg = HashMap UUID Exchange

--------------------------------------------------------------------------------
data ConfirmationState
  = Authentication
  | Identification

--------------------------------------------------------------------------------
data Registry =
  Registry
  { registryReg :: Reg
  , registryLastCheck :: NominalDiffTime
  }

--------------------------------------------------------------------------------
newRegistry :: Functor m => Driver m -> m Registry
newRegistry self = Registry HashMap.empty <$> getElapsedTime self

--------------------------------------------------------------------------------
retryMaxReached :: Retry -> Int -> Bool
retryMaxReached (AtMost n) i = i > n
retryMaxReached KeepRetrying _ = False

--------------------------------------------------------------------------------
registerExchange :: Exchange -> Registry -> Registry
registerExchange exc reg =
  let newMap = HashMap.insert (exchangePkgId exc) exc $ registryReg reg in
  reg { registryReg = newMap }

--------------------------------------------------------------------------------
checkAndRetry :: Monad m
              => Driver m
              -> NominalDiffTime
              -> Registry
              -> m Registry
checkAndRetry self elapsed reg = do
  setts <- getSettings self
  make <$> foldM (go setts) pendings (HashMap.toList pendings)
  where
    pendings = registryReg reg

    make newReg =
      Registry
      { registryReg = newReg
      , registryLastCheck = elapsed
      }

    go setts cur (key, exc)
      | elapsed - exchangeStarted exc >= s_operationTimeout setts =
        if retryMaxReached (s_operationRetry setts) (exchangeRetry exc)
          then
            let pkgId = packageCorrelation $ exchangeRequest exc
                next = HashMap.delete pkgId cur in

            next <$ reportBadNews self MaxRetriesReached exc
          else pure cur
      | otherwise =
        pure cur

--------------------------------------------------------------------------------
cleanup :: Monad m
        => Driver m
        -> Registry
        -> m ()
cleanup self = traverse_ (reportBadNews self ConnectionDropped) . registryReg

--------------------------------------------------------------------------------
data ConnectedStage
  = Confirming [Await] NominalDiffTime UUID ConfirmationState
  | Active Registry

--------------------------------------------------------------------------------
data DriverState m
  = Init
  | Awaiting [Await] ConnectionAttempt (ConnectingState m)
  | Connected (Connection m) ConnectedStage
  | Closed

--------------------------------------------------------------------------------
data Transmission
  = Ignored Package
  | Recv (Either BadNews Package)
  deriving (Show)

--------------------------------------------------------------------------------
type ClientVersion = Int32
type ConnectionName = Text

--------------------------------------------------------------------------------
data Msg
  = SystemInit
  | EstablishConnection EndPoint
  | ConnectionEstablished ConnectionId
  | PackageArrived ConnectionId Package
  | SendPackage Package
  | forall e. Exception e => CloseConnection e
  | Tick

--------------------------------------------------------------------------------
react :: Monad m => Driver m -> DriverState m -> Msg -> m (DriverState m)
react self s SystemInit = discovery self s
react self s (EstablishConnection ept) = establish self s ept
react self s (ConnectionEstablished cid) = established self s cid
react self s (PackageArrived connId pkg) = packageArrived self s connId pkg
react self s (SendPackage pkg) = sendPackage self s pkg
react self s (CloseConnection e) = onCloseConnection self s e
react self s Tick = tick self s

--------------------------------------------------------------------------------
discovery :: Monad m => Driver m -> DriverState m -> m (DriverState m)
discovery self = \case
  Init -> do
    att <- createConnectionAttempt self
    discovery self (Awaiting [] att Reconnecting)

  Awaiting pkgs started Reconnecting ->
    Awaiting pkgs started EndpointDiscovery <$ discover self

  s -> pure s

--------------------------------------------------------------------------------
establish :: Monad m => Driver m -> DriverState m -> EndPoint -> m (DriverState m)
establish self (Awaiting pkgs started EndpointDiscovery) ept =
  Awaiting pkgs started . ConnectionEstablishing <$> connect self ept
establish _ s _ = pure s

--------------------------------------------------------------------------------
established :: Monad m
            => Driver m
            -> DriverState m
            -> ConnectionId
            -> m (DriverState m)
established self s@(Awaiting pkgs _ (ConnectionEstablishing known)) cid
  | cid == connectionId known = do
    setts <- getSettings self
    elapsed <- getElapsedTime self

    case s_defaultUserCredentials setts of
      Just cred -> do
        pkg <- createAuthenticatePkg self cred

        let uuid = packageCorrelation pkg

        sendPkg known pkg
        pure $ Connected known (Confirming pkgs elapsed uuid Authentication)

      Nothing -> do
        pkg <- identifyClient self

        let uuid = packageCorrelation pkg

        sendPkg known pkg
        pure $ Connected known (Confirming pkgs elapsed uuid Identification)

  | otherwise = pure s
established _ s _ = pure s

--------------------------------------------------------------------------------
identifyClient :: Monad m => Driver m -> m Package
identifyClient self = do
  uuid <- generateId self
  setts <- getSettings self
  let defName = [i|ES-#{uuid}|]
      connName = fromMaybe defName (s_defaultConnectionName setts)

  createIdentifyPkg self clientVersion connName

  where
    clientVersion = 1

--------------------------------------------------------------------------------
createAuthenticatePkg :: Monad m => Driver m -> Credentials -> m Package
createAuthenticatePkg self cred = do
  uuid <- generateId self
  let pkg = Package { packageCmd         = authenticateCmd
                    , packageCorrelation = uuid
                    , packageData        = ""
                    , packageCred        = Just cred
                    }
  pure pkg

--------------------------------------------------------------------------------
createIdentifyPkg :: Monad m
                  => Driver m
                  -> ClientVersion
                  -> ConnectionName
                  -> m Package
createIdentifyPkg self version name = do
  uuid <- generateId self
  let msg = Identify.newRequest version name
      pkg = Package { packageCmd         = identifyClientCmd
                    , packageCorrelation = uuid
                    , packageData        = runPut $ encodeMessage msg
                    , packageCred        = Nothing
                    }

  pure pkg

--------------------------------------------------------------------------------
-- | I'm bad at naming thing however, we are going to use that datastructure
--  so we could lookup and delete in one single pass.
data Blob a b = Blob a b

--------------------------------------------------------------------------------
instance Functor (Blob a) where
  fmap f (Blob a b) = Blob a (f b)

--------------------------------------------------------------------------------
removeExchange :: UUID -> Registry -> (Maybe Exchange, Registry)
removeExchange key reg =
  let Blob result newMap =
        HashMap.alterF go key (registryReg reg) in
        (result, reg { registryReg = newMap })
  where
    go Nothing  = Blob Nothing Nothing
    go (Just e) = Blob (Just e) Nothing

--------------------------------------------------------------------------------
packageArrived :: Monad m
               => Driver m
               -> DriverState m
               -> ConnectionId
               -> Package
               -> m (DriverState m)
packageArrived self s@(Connected known stage) connId pkg
  | connectionId known /= connId = s <$ ignorePkg self pkg
  | otherwise =
    case () of
      _ | cmd == heartbeatResponseCmd -> pure s
        | cmd == heartbeatRequestCmd -> s <$ sendPkg known heartbeatResponse
        | otherwise ->
          case stage of
            Confirming pkgs started pkgId state
              | correlation /= pkgId -> pure s
              | otherwise ->
                case state of
                  Authentication
                    | cmd == authenticatedCmd || cmd == notAuthenticatedCmd
                      -> switchToIdentification self known pkgs
                    | otherwise -> pure s

                  Identification
                    | cmd == clientIdentifiedCmd -> do
                      reg <- sendAwaitingPkgs self known pkgs
                      pure (Connected known (Active reg))
                    | otherwise -> pure s
            Active reg -> do
              let (excMaybe, newReg) = removeExchange correlation reg

              case excMaybe of
                Nothing -> s <$ ignorePkg self pkg
                Just exc -> do
                  case () of
                    _ | cmd == badRequestCmd -> do
                        let reason = packageDataAsText pkg

                        reportBadNews self (BadRequest reason) exc
                        pure (Connected known (Active newReg))

                      | cmd == notAuthenticatedCmd -> do
                        reportBadNews self AuthenticationNeeded exc
                        pure (Connected known (Active newReg))

                      | cmd == notHandledCmd -> do
                        let Just msg = maybeDecodeMessage (packageData pkg)
                            reason   = getField $ notHandledReason msg

                        case reason of
                          N_NotMaster -> do
                            let Just details = getField $ notHandledAdditionalInfo msg
                                info         = masterInfo details
                                node         = masterInfoNodeEndPoints info

                            newCid <- forceReconnect self correlation node
                            aws <- makeAwaitings self reg
                            att <- createConnectionAttempt self

                            let newState =
                                  Awaiting (exchangeToAwait exc : aws) att
                                    (ConnectionEstablishing newCid)

                            pure newState

                          -- In this case with just retry the operation.
                          _ -> s <$ sendPkg known (exchangeRequest exc)

                      | otherwise ->
                        Connected known (Active newReg) <$ reportResponse self pkg

  where
    cmd = packageCmd pkg
    correlation = packageCorrelation pkg
    heartbeatResponse =
      heartbeatResponsePackage $ packageCorrelation pkg

packageArrived self s _ pkg = s <$ ignorePkg self pkg

--------------------------------------------------------------------------------
sendPackage :: Monad m => Driver m -> DriverState m -> Package -> m (DriverState m)
sendPackage self cur pkg =
  case cur of
    Init -> do
      next <- react self Init SystemInit
      sendPackage self next pkg

    Awaiting aws att state ->
      let aw = pkgToAwait pkg in
      pure (Awaiting (aw:aws) att state)

    Connected conn stage ->
      case stage of
        Confirming aws started pkgId state ->
          let aw = pkgToAwait pkg in
          pure (Connected conn $ Confirming (aw:aws) started pkgId state)

        Active reg -> do
          exc <- makeExchange self pkg
          Connected conn (Active (registerExchange exc reg))
            <$ sendPkg conn pkg

    Closed ->
      cur <$ reportBadNews_ self ConnectionClosed pkg

--------------------------------------------------------------------------------
onCloseConnection :: (Monad m, Exception e)
                  => Driver m
                  -> DriverState m
                  -> e
                  -> m (DriverState m)
onCloseConnection self state e =
  case state of
    Init -> pure Closed

    Closed -> pure state

    Awaiting aws _ connState -> do
      traverse_ (reportBadNews_ self ConnectionClosed . awaitPackage) aws

      case connState of
        ConnectionEstablishing conn ->
          closeConnection conn (FatalConnectionError e)
        _ -> pure ()

      pure Closed

    Connected conn connState -> do
      closeConnection conn (FatalConnectionError e)

      case connState of
        Confirming aws _ _ _ ->
          traverse_ (reportBadNews_ self ConnectionClosed . awaitPackage) aws

        Active reg ->
          cleanup self reg

      pure Closed

--------------------------------------------------------------------------------
tick :: Monad m => Driver m -> DriverState m -> m (DriverState m)
tick self Init = pure Init
tick self Closed = pure Closed
tick self cur@(Awaiting aws att _) = do
  setts <- getSettings self
  elapsed <- getElapsedTime self

  if elapsed - connectionAttemptStarted att >= s_reconnect_delay setts
    then
      if maxRetryReached (s_retry setts) (connectionAttemptRetry att)
        then maxReconnectReached self
        else reconnect self att aws
    else pure cur
tick self cur@(Connected cid state) = do
  setts <- getSettings self

  case state of
    Confirming aws started _ state -> do
      elapsed <- getElapsedTime self

      if elapsed - started >= s_operationTimeout setts
        then
          case state of
            Authentication ->
              switchToIdentification self cid aws
            Identification ->
              close self cid (PendingConfirmation aws) IdentificationFailure
        else
          pure cur
    Active reg -> do
      elapsed <- getElapsedTime self
      if elapsed - registryLastCheck reg >= s_operationTimeout setts
        then Connected cid . Active <$> checkAndRetry self elapsed reg
        else pure cur

--------------------------------------------------------------------------------
reconnect :: Monad m => Driver m -> ConnectionAttempt -> [Await] -> m (DriverState m)
reconnect self att aws = do
  started <- getElapsedTime self

  let newAtt =
        ConnectionAttempt
        { connectionAttemptRetry = connectionAttemptRetry att + 1
        , connectionAttemptStarted = started
        }

  discovery self (Awaiting aws newAtt Reconnecting)

--------------------------------------------------------------------------------
maxReconnectReached :: Monad m => Driver m -> m (DriverState m)
maxReconnectReached self = Closed <$ stop self (OfflineError ConnectionMaxAttemptReached)

--------------------------------------------------------------------------------
switchToIdentification :: Monad m
                       => Driver m
                       -> Connection m
                       -> [Await]
                       -> m (DriverState m)
switchToIdentification self conn aws = do
  pkg <- identifyClient self
  elapsed <- getElapsedTime self

  let pkgId = packageCorrelation pkg

  Connected conn (Confirming aws elapsed pkgId Identification)
    <$ sendPkg conn pkg

--------------------------------------------------------------------------------
data ClosingContext
  = PendingConfirmation [Await]
  | ConnectionWasActive Registry

--------------------------------------------------------------------------------
close :: Monad m
      => Driver m
      -> Connection m
      -> ClosingContext
      -> ConnectionError
      -> m (DriverState m)
close self conn ctx e = do
  closeConnection conn e
  case ctx of
    PendingConfirmation aws -> do
      att <- createConnectionAttempt self
      reconnect self att aws
    ConnectionWasActive reg -> do
      cleanup self reg
      att <- createConnectionAttempt self
      reconnect self att []

--------------------------------------------------------------------------------
sendAwaitingPkgs :: Monad m => Driver m -> Connection m -> [Await] -> m Registry
sendAwaitingPkgs self conn aws =
  foldM go HashMap.empty aws >>= \reg ->
    Registry reg <$> getElapsedTime self
  where
    go reg a = do
      elapsed <- getElapsedTime self

      let pkg = awaitPackage a
          exc =
            Exchange
            { exchangeRetry = awaitRetry a
            , exchangeStarted = elapsed
            , exchangeRequest = pkg
            }

      HashMap.insert (packageCorrelation pkg) exc reg
        <$ enqueuePackage conn pkg

--------------------------------------------------------------------------------
makeAwaitings :: Monad m => Driver m -> Registry -> m [Await]
makeAwaitings self reg = do
  setts <- getSettings self
  fmap exchangeToAwait
    <$> filterM (go setts) (HashMap.elems $ registryReg reg)
  where
    go setts exc
      | maxRetryReached (s_operationRetry setts) (exchangeRetry exc) =
        False <$ reportBadNews self MaxRetriesReached exc
      | otherwise = pure True

--------------------------------------------------------------------------------
maxRetryReached :: Retry -> Int -> Bool
maxRetryReached (AtMost n) i = i + 1 >= n
maxRetryReached KeepRetrying _ = False

--------------------------------------------------------------------------------
maybeDecodeMessage :: Decode a => ByteString -> Maybe a
maybeDecodeMessage bytes =
    case runGet decodeMessage bytes of
        Right a -> Just a
        _       -> Nothing
