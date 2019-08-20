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
import Control.Monad (forever, when, foldM, filterM)
import Data.ByteString (ByteString)
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
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Reader
import Polysemy.State
import Data.String.Interpolate.IsString (i)

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.EndPoint
import Database.EventStore.Internal.Effect.Driver
import Database.EventStore.Internal.Operation (Operation, OperationError(..))
import qualified Database.EventStore.Internal.Operation.Identify as Identify
import Database.EventStore.Internal.Settings
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data TransmissionError
  = ConnectionDropped
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
data Exchange =
  Exchange
  { exchangeRetry :: Int
  , exchangeStarted :: NominalDiffTime
  , exchangeRequest :: Package
  }

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
createConnectionAttempt :: Member Driver r => Sem r ConnectionAttempt
createConnectionAttempt = do
  started <- getElapsedTime
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
newRegistry :: Member Driver r => Sem r Registry
newRegistry = Registry HashMap.empty <$> getElapsedTime

--------------------------------------------------------------------------------
retryMaxReached :: Retry -> Int -> Bool
retryMaxReached (AtMost n) i = i > n
retryMaxReached KeepRetrying _ = False

--------------------------------------------------------------------------------
checkAndRetry :: forall r. Member (Output Transmission) r
              => Settings
              -> NominalDiffTime
              -> Registry
              -> Sem r Registry
checkAndRetry setts elapsed reg =
  make <$> foldM go pendings (HashMap.toList pendings)
  where
    pendings = registryReg reg

    make newReg =
      Registry
      { registryReg = newReg
      , registryLastCheck = elapsed
      }

    go :: Reg -> (UUID, Exchange) -> Sem r Reg
    go cur (key, exc)
      | elapsed - exchangeStarted exc >= s_operationTimeout setts =
        if retryMaxReached (s_operationRetry setts) (exchangeRetry exc)
          then
            let pkgId = packageCorrelation $ exchangeRequest exc
                cmd = packageCmd $ exchangeRequest exc
                next = HashMap.delete pkgId cur
                badNews =
                  BadNews
                  { badNewsId = pkgId
                  , badNewsError = MaxRetriesReached
                  } in

            next <$ output (Recv $ Left badNews)
          else pure cur
      | otherwise =
        pure cur

--------------------------------------------------------------------------------
data ConnectedStage
  = Confirming [Await] NominalDiffTime UUID ConfirmationState
  | Active Registry

--------------------------------------------------------------------------------
data DriverState
  = Init
  | Awaiting [Await] ConnectionAttempt ConnectingState
  | Connected ConnectionId ConnectedStage
  | Closed

--------------------------------------------------------------------------------
data Transmission
  = Send Package
  | Ignored Package
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
  | Tick

----------------------------------------------------------------------------------
process :: forall r. Members [Reader Settings, Input Msg, Output Transmission, Driver] r
        => Sem r ()
process = go Init
  where
    go :: Members [Reader Settings, Input Msg, Output Transmission, Driver] r
       => DriverState
       -> Sem r ()
    go cur = do msg <- input
                go =<< react cur msg

--------------------------------------------------------------------------------
react :: Members '[Reader Settings, Output Transmission, Driver] r
      => DriverState
      -> Msg
      -> Sem r DriverState
react s SystemInit = discovery s
react s (EstablishConnection ept) = establish s ept
react s (ConnectionEstablished cid) = established s cid
react s (PackageArrived connId pkg) = packageArrived s connId pkg
react s Tick = tick s

--------------------------------------------------------------------------------
discovery :: Members '[Driver] r
          => DriverState
          -> Sem r DriverState
discovery = \case
  Init -> do
    att <- createConnectionAttempt
    discovery (Awaiting [] att Reconnecting)

  Awaiting pkgs started Reconnecting ->
    Awaiting pkgs started EndpointDiscovery <$ discover

  s -> pure s

--------------------------------------------------------------------------------
establish :: Members '[Driver] r
          => DriverState
          -> EndPoint
          -> Sem r DriverState
establish (Awaiting pkgs started EndpointDiscovery) ept =
  Awaiting pkgs started . ConnectionEstablishing <$> connect ept
establish s _ = pure s

--------------------------------------------------------------------------------
established :: Members '[Reader Settings, Driver, Output Transmission] r
            => DriverState
            -> ConnectionId
            -> Sem r DriverState
established s@(Awaiting pkgs _ (ConnectionEstablishing known)) cid
  | cid == known = do
    setts <- ask
    elapsed <- getElapsedTime

    case s_defaultUserCredentials setts of
      Just cred -> do
        pkg <- createAuthenticatePkg cred

        let uuid = packageCorrelation pkg

        output (Send pkg)
        pure $ Connected cid (Confirming pkgs elapsed uuid Authentication)

      Nothing -> do
        pkg <- identifyClient setts

        let uuid = packageCorrelation pkg

        output (Send pkg)
        pure $ Connected cid (Confirming pkgs elapsed uuid Identification)

  | otherwise = pure s
established s _ = pure s

--------------------------------------------------------------------------------
identifyClient :: Members '[Output Transmission, Driver] r
               => Settings
               -> Sem r Package
identifyClient setts = do
  uuid <- generateId
  let defName = [i|ES-#{uuid}|]
      connName = fromMaybe defName (s_defaultConnectionName setts)

  createIdentifyPkg clientVersion connName

  where
    clientVersion = 1

--------------------------------------------------------------------------------
createAuthenticatePkg :: Member Driver r => Credentials -> Sem r Package
createAuthenticatePkg cred = do
  uuid <- generateId
  let pkg = Package { packageCmd         = authenticateCmd
                    , packageCorrelation = uuid
                    , packageData        = ""
                    , packageCred        = Just cred
                    }
  pure pkg

--------------------------------------------------------------------------------
createIdentifyPkg :: Member Driver r
                  => ClientVersion
                  -> ConnectionName
                  -> Sem r Package
createIdentifyPkg version name = do
  uuid <- generateId
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
packageArrived :: Members '[Reader Settings, Output Transmission, Driver] r
               => DriverState
               -> ConnectionId
               -> Package
               -> Sem r DriverState
packageArrived s@(Connected known stage) connId pkg
  | known /= connId = ignored s
  | otherwise =
    case () of
      _ | cmd == heartbeatResponseCmd -> pure s
        | cmd == heartbeatRequestCmd -> s <$ output (Send heartbeatResponse)
        | otherwise ->
          case stage of
            Confirming pkgs started pkgId state
              | correlation /= pkgId -> pure s
              | otherwise ->
                case state of
                  Authentication
                    | cmd == authenticatedCmd || cmd == notAuthenticatedCmd
                      -> switchToIdentification known pkgs
                    | otherwise -> pure s

                  Identification
                    | cmd == clientIdentifiedCmd -> do
                      reg <- sendAwaitingPkgs pkgs
                      pure (Connected known (Active reg))
                    | otherwise -> pure s
            Active reg -> do
              let (excMaybe, newReg) = removeExchange correlation reg

              case excMaybe of
                Nothing -> ignored s
                Just exc -> do
                  case () of
                    _ | cmd == badRequestCmd -> do
                        let reason = packageDataAsText pkg
                            badNews =
                              BadNews
                              { badNewsId = correlation
                              , badNewsError = BadRequest reason
                              }

                        output $ Recv (Left badNews)
                        pure (Connected known (Active newReg))

                      | cmd == notAuthenticatedCmd -> do
                        let badNews =
                              BadNews
                              { badNewsId = correlation
                              , badNewsError = AuthenticationNeeded
                              }

                        output $ Recv (Left badNews)
                        pure (Connected known (Active newReg))

                      | cmd == notHandledCmd -> do
                        let Just msg = maybeDecodeMessage (packageData pkg)
                            reason   = getField $ notHandledReason msg

                        case reason of
                          N_NotMaster -> do
                            let Just details = getField $ notHandledAdditionalInfo msg
                                info         = masterInfo details
                                node         = masterInfoNodeEndPoints info

                            newCid <- forceReconnect correlation node
                            setts <- ask
                            aws <- makeAwaitings setts reg
                            att <- createConnectionAttempt

                            let newState =
                                  Awaiting (exchangeToAwait exc : aws) att
                                    (ConnectionEstablishing newCid)

                            pure newState

                          -- In this case with just retry the operation.
                          _ -> do
                            output (Send $ exchangeRequest exc)
                            pure s

                      | otherwise -> do
                        output (Recv $ Right pkg)
                        pure $ Connected known (Active newReg)

  where
    cmd = packageCmd pkg
    correlation = packageCorrelation pkg
    heartbeatResponse =
      heartbeatResponsePackage $ packageCorrelation pkg

packageArrived s _ _ = ignored s

--------------------------------------------------------------------------------
tick :: forall r.  Members '[Reader Settings, Driver, Output Transmission] r
     => DriverState
     -> Sem r DriverState
tick Init = pure Init
tick Closed = pure Closed
tick cur@(Awaiting aws att _) = do
  setts <- ask
  elapsed <- getElapsedTime

  if elapsed - connectionAttemptStarted att >= s_reconnect_delay setts
    then
      case s_retry setts of
        AtMost n
          | connectionAttemptRetry att <= n -> reconnect att aws
          | otherwise -> maxReconnectReached n
        KeepRetrying -> reconnect att aws
    else pure cur
tick cur@(Connected cid state) = do
  setts <- ask

  case state of
    Confirming aws started _ state -> do
      elapsed <- getElapsedTime

      if elapsed - started >= s_operationTimeout setts
        then
          case state of
            Authentication ->
              switchToIdentification cid aws
            Identification ->
              close cid (PendingConfirmation aws) IdentificationFailure
        else
          pure cur
    Active reg -> do
      setts <- ask
      elapsed <- getElapsedTime
      if elapsed - registryLastCheck reg >= s_operationTimeout setts
        then Connected cid . Active <$> checkAndRetry setts elapsed reg
        else pure cur

--------------------------------------------------------------------------------
reconnect :: Members '[Driver] r
          => ConnectionAttempt
          -> [Await]
          -> Sem r DriverState
reconnect att aws = do
  started <- getElapsedTime

  let newAtt =
        ConnectionAttempt
        { connectionAttemptRetry = connectionAttemptRetry att + 1
        , connectionAttemptStarted = started
        }

  discovery (Awaiting aws newAtt Reconnecting)

--------------------------------------------------------------------------------
maxReconnectReached :: Member Driver r => Int -> Sem r DriverState
maxReconnectReached count = do
  let reason =
        ConnectionMaxAttemptReached count

  stop (OfflineError reason)
  pure Closed

--------------------------------------------------------------------------------
switchToIdentification :: Members '[Reader Settings, Driver, Output Transmission] r
                       => ConnectionId
                       -> [Await]
                       -> Sem r DriverState
switchToIdentification connId aws = do
  setts <- ask
  pkg <- identifyClient setts
  elapsed <- getElapsedTime

  let pkgId = packageCorrelation pkg

  output (Send pkg)
  pure $ Connected connId (Confirming aws elapsed pkgId Identification)

--------------------------------------------------------------------------------
data ClosingContext
  = PendingConfirmation [Await]
  | ConnectionWasActive Reg

--------------------------------------------------------------------------------
close :: Members '[Reader Settings, Driver] r
      => ConnectionId
      -> ClosingContext
      -> ConnectionError
      -> Sem r DriverState
close cid ctx e = do
  closeConnection cid e
  case ctx of
    PendingConfirmation aws -> do
      att <- createConnectionAttempt
      reconnect att aws
    ConnectionWasActive reg -> do
      -- TODO - Perform registry cleanup!
      att <- createConnectionAttempt
      reconnect att []

--------------------------------------------------------------------------------
ignored :: s -> Sem r s
ignored = pure

--------------------------------------------------------------------------------
sendAwaitingPkgs :: forall r. Members [Output Transmission, Driver] r
                 => [Await]
                 -> Sem r Registry
sendAwaitingPkgs aws =
  foldM go HashMap.empty aws >>= \reg ->
    Registry reg <$> getElapsedTime
  where
    go :: Reg -> Await -> Sem r Reg
    go reg a = do
      elapsed <- getElapsedTime

      let pkg = awaitPackage a
          exc =
            Exchange
            { exchangeRetry = awaitRetry a
            , exchangeStarted = elapsed
            , exchangeRequest = pkg
            }

      HashMap.insert (packageCorrelation pkg) exc reg
        <$ output (Send pkg)

--------------------------------------------------------------------------------
makeAwaitings :: Member (Output Transmission) r
              => Settings
              -> Registry
              -> Sem r [Await]
makeAwaitings setts reg =
  fmap exchangeToAwait
    <$> filterM go (HashMap.elems $ registryReg reg)
  where
    retry = s_operationRetry setts

    go exc
      | maxRetryReached retry (exchangeRetry exc)
        = let badNews =
                BadNews
                { badNewsId = packageCorrelation (exchangeRequest exc)
                , badNewsError = MaxRetriesReached
                } in
          False <$ output (Recv $ Left badNews)

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
