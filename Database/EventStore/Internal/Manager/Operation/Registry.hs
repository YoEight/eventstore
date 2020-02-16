--------------------------------------------------------------------------------
-- |
-- Module    :  Database.EventStore.Internal.Manager.Registry
-- Copyright :  (C) 2020 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Manager.Operation.Registry where

--------------------------------------------------------------------------------
import qualified Data.HashMap.Strict as HashMap

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Serialize

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Control
import Database.EventStore.Internal.EndPoint
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Stopwatch
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Request =
  Request
  { requestOriginal :: !Package
  , requestConnId :: !UUID
  , requestRetries :: !Int
  , requestStarted :: !NominalDiffTime
  , requestMailbox :: !Mailbox
  , requestLifetime :: !Lifetime
  }

--------------------------------------------------------------------------------
requestIsKeepAlive :: Request -> Bool
requestIsKeepAlive req
  = case requestLifetime req of
      OneTime -> False
      KeepAlive _ -> True

--------------------------------------------------------------------------------
requestToWaiting :: Request -> Waiting
requestToWaiting req =
  Waiting
  { waitingLifetime = requestLifetime req
  , waitingPkg = requestOriginal req
  , waitingMailbox = requestMailbox req
  }

--------------------------------------------------------------------------------
waitingToRequest
  :: UUID -- Connection id.
  -> NominalDiffTime
  -> Waiting
  -> Request
waitingToRequest connId started w
  = Request
    { requestOriginal = waitingPkg w
    , requestConnId = connId
    , requestRetries = 1
    , requestStarted = started
    , requestMailbox = waitingMailbox w
    , requestLifetime = waitingLifetime w
    }

--------------------------------------------------------------------------------
data Waiting =
  Waiting
  { waitingLifetime :: !Lifetime
  , waitingPkg :: !Package
  , waitingMailbox :: !Mailbox
  }

--------------------------------------------------------------------------------
type Requests = HashMap UUID Request

--------------------------------------------------------------------------------
data Registry' =
  Registry'
  { registryRequests :: !Requests
  , registryWaitings :: ![Waiting]
  , registryTimeout :: !NominalDiffTime
  , registryMaxRetry :: !Retry
  }

--------------------------------------------------------------------------------
registryClear :: Registry' -> Registry'
registryClear reg =
  reg
  { registryRequests = mempty
  , registryWaitings = []
  }

--------------------------------------------------------------------------------
data Registry =
  Registry
  { registryState :: IORef Registry'
  , registryStopwatch :: Stopwatch
  }

--------------------------------------------------------------------------------
-- | I'm bad at naming thing however, we are going to use that datastructure
--  so we could lookup and delete in one single pass.
data Blob a b = Blob a b

--------------------------------------------------------------------------------
instance Functor (Blob a) where
  fmap f (Blob a b) = Blob a (f b)

--------------------------------------------------------------------------------
registryRemoveRequest
  :: UUID
  -> Registry'
  -> (Maybe Request, Registry')
registryRemoveRequest key reg
  = let Blob result newMap
          = HashMap.alterF go key (registryRequests reg)
    in (result, reg { registryRequests = newMap })
  where
    go Nothing = Blob Nothing Nothing
    go (Just e) = Blob (Just e) Nothing

--------------------------------------------------------------------------------
registryNew :: NominalDiffTime -> Retry -> IO Registry
registryNew timeout maxRetry
  = Registry
    <$> newIORef state
    <*> newStopwatch
  where
    state =
      Registry'
      { registryRequests = mempty
      , registryWaitings = []
      , registryTimeout = timeout
      , registryMaxRetry = maxRetry
      }

--------------------------------------------------------------------------------
registryRegister
  :: Registry
  -> UUID -- Connection Id.
  -> Lifetime
  -> Package
  -> Mailbox
  -> EventStore ()
registryRegister reg connId lifetime pkg mailbox
  = do started <- stopwatchElapsed (registryStopwatch reg)
       modifyIORef' (registryState reg) (go started)
  where
    go started state =
      let req = Request
                { requestOriginal = pkg
                , requestConnId = connId
                , requestRetries = 1
                , requestStarted = started
                , requestMailbox = mailbox
                , requestLifetime = lifetime
                }

          correlation = packageCorrelation pkg
          nextReqs = insertMap correlation req (registryRequests state)
      in state { registryRequests = nextReqs }

--------------------------------------------------------------------------------
registryPostpone
  :: Registry
  -> Mailbox
  -> Lifetime
  -> Package
  -> EventStore ()
registryPostpone reg mailbox lifetime pkg
  = modifyIORef' (registryState reg) go
  where
    go state
      = let waiting
              = Waiting
                { waitingLifetime = lifetime
                , waitingPkg = pkg
                , waitingMailbox = mailbox
                }

            nextWs = waiting : registryWaitings state
        in state { registryWaitings = nextWs }

--------------------------------------------------------------------------------
registryHandle
  :: Registry
  -> Package
  -> EventStore (Maybe NodeEndPoints)
registryHandle reg pkg
  = do state <- readIORef (registryState reg)
       case registryRemoveRequest (packageCorrelation pkg) state of
         (Nothing, _)
           -> do $logWarn [i|No operation associated to package: #{pkg}|]
                 pure Nothing
         (Just req, stateWithoutReq)
           -> case packageCmd pkg of
                cmd | cmd == badRequestCmd
                  -> do let reason = packageDataAsText pkg
                        mailboxFail (requestMailbox req) (ServerError reason)
                        pure Nothing

                    | cmd == notAuthenticatedCmd
                  -> do mailboxFail (requestMailbox req) NotAuthenticatedOp
                        pure Nothing

                    | cmd == notHandledCmd -- In all cases, we decide to postpone that command.
                  -> do $(logWarn) [i|Not handled response received: #{pkg}.|]
                        let Just msg = maybeDecodeMessage $ packageData pkg
                            reason = getField $ notHandledReason msg
                            waiting = requestToWaiting req
                            nextWs = waiting : registryWaitings stateWithoutReq
                            finalState = stateWithoutReq { registryWaitings = nextWs }
                            origCmd = packageCmd (requestOriginal req)
                            pkgId = packageCorrelation pkg

                        writeIORef (registryState reg) finalState

                        case reason of
                          N_NotMaster
                            -> do let Just details = getField $ notHandledAdditionalInfo msg
                                      info = masterInfo details
                                      node = masterInfoNodeEndPoints info

                                  $(logWarn) [i|Received a non master error on command #{origCmd} [#{pkgId}] on #{node}|]
                                  pure (Just node)

                          _ -> do $(logWarn) [i|The server has either not started or is too busy. Retrying command #{origCmd} #{pkgId}|]
                                  pure Nothing

                    | otherwise
                  -> do let respCmd = packageCmd pkg

                        mailboxSendPkg (requestMailbox req) pkg

                        case requestLifetime req of
                          OneTime
                            -> do writeIORef (registryState reg) stateWithoutReq
                                  pure Nothing

                          KeepAlive endCmd
                            | endCmd == respCmd
                            -> do writeIORef (registryState reg) stateWithoutReq
                                  pure Nothing
                            | otherwise -- Means we keep the previous state (Subscription).
                            -> pure Nothing

--------------------------------------------------------------------------------
data CRState =
  CRState
  { crsState :: !Registry'
  , crsPkgs :: ![Package]
  }

--------------------------------------------------------------------------------
crsStateNew :: Registry' -> CRState
crsStateNew reg =
  CRState
  { crsState = reg
  , crsPkgs = []
  }

--------------------------------------------------------------------------------
crsStateDeleteReq :: Request -> CRState -> CRState
crsStateDeleteReq req reg
  = let state
          = crsState reg
        nextReqs
          = deleteMap
              (packageCorrelation . requestOriginal $ req)
              (registryRequests state)
        nextState
          = state { registryRequests = nextReqs } in
    reg { crsState = nextState }

--------------------------------------------------------------------------------
crsStateRegisterReq :: Request -> CRState -> CRState
crsStateRegisterReq req reg
  = let state
          = crsState reg
        nextReqs
          = insertMap
              (packageCorrelation . requestOriginal $ req)
              req
              (registryRequests state)
        nextState
          = state { registryRequests = nextReqs } in
    reg { crsState = nextState }

--------------------------------------------------------------------------------
crsStateAddPkg :: Package -> CRState -> CRState
crsStateAddPkg pkg reg
  = let nextPkgs = pkg : crsPkgs reg in
    reg { crsPkgs = nextPkgs }

--------------------------------------------------------------------------------
registryCheckAndRetry
  :: Registry
  -> UUID -- Connection id.
  -> EventStore [Package]
registryCheckAndRetry reg connId
  = do state <- readIORef (registryState reg)
       elapsed <- stopwatchElapsed (registryStopwatch reg)
       let reqs = mapToList $ registryRequests state

       newState <- foldM (checking elapsed) (crsStateNew state) reqs
       let newStateTemp = crsState newState
           awaitings = registryWaitings newStateTemp
           tempState = newStateTemp { registryWaitings = [] }
           newCRState = newState { crsState = tempState }
           finalState = foldl' (sending elapsed) newCRState awaitings

       writeIORef (registryState reg) (crsState finalState)
       pure (crsPkgs finalState)
  where
    checking elapsed state (_, req)
      = do let maxTimeout = registryTimeout . crsState $ state
               hasTimeout = elapsed - (requestStarted req) >= maxTimeout
               maxRetry = registryMaxRetry . crsState $ state
           if requestConnId req /= connId
             then
               do mailboxFail (requestMailbox req) ConnectionHasDropped
                  pure (crsStateDeleteReq req state)
           else if not (requestIsKeepAlive req) && hasTimeout
             then case maxRetry of
                    AtMost maxAtt
                      | requestRetries req + 1 > maxAtt
                      -> do let pkg = requestOriginal req
                                pkgId = packageCorrelation pkg
                                cmd = packageCmd pkg

                            $(logError) [i|Command #{cmd} [#{pkgId}] maximum retries threshold reached (#{maxAtt}), aborted!|]
                            mailboxFail (requestMailbox req) Aborted
                            pure (crsStateDeleteReq req state)
                      | otherwise
                      -> retryReq
                    KeepRetrying
                      -> retryReq
           else
             pure state
      where
        retryReq
          = do let nextRetries
                     = requestRetries req + 1
                   nextReq
                     = req
                       { requestRetries = nextRetries
                       , requestStarted = elapsed
                       }

                   maxAtt
                     = case registryMaxRetry . crsState $ state of
                         AtMost n -> n
                         KeepRetrying -> maxBound

                   pkg = requestOriginal req
                   cmd = packageCmd pkg
                   pkgId = packageCorrelation pkg

               $(logWarn) [i|Command #{cmd} [#{pkgId} has timeout. Retrying (attempt #{nextRetries}/#{maxAtt})|]

               pure . crsStateRegisterReq nextReq . crsStateAddPkg pkg $ state

    sending elapsed state w
      = let req = waitingToRequest connId elapsed w
            pkg = requestOriginal req in
        crsStateRegisterReq req . crsStateAddPkg pkg $ state

--------------------------------------------------------------------------------
registryAbort :: Registry -> EventStore ()
registryAbort reg
  = do state <- readIORef (registryState reg)
       writeIORef (registryState reg) (registryClear state)

       for_ (registryRequests state) $ \req
         -> mailboxFail (requestMailbox req) Aborted

       for_ (registryWaitings state) $ \w
         -> mailboxFail (waitingMailbox w) Aborted

--------------------------------------------------------------------------------
maybeDecodeMessage :: Decode a => ByteString -> Maybe a
maybeDecodeMessage bytes =
    case runGet decodeMessage bytes of
        Right a -> Just a
        _       -> Nothing
