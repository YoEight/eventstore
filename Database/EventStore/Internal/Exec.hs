{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Exec
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Exec
  ( Exec
  , newExec
  ) where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Discovery
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.ConnectionManager
import Database.EventStore.Internal.OperationManager
import Database.EventStore.Internal.SubscriptionManager
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
type ServicePendingInit = HashMap Service ()

--------------------------------------------------------------------------------
data Stage
  = Init
  | Available Publish
  | Errored String

--------------------------------------------------------------------------------
newtype Terminated = Terminated String deriving Show

--------------------------------------------------------------------------------
instance Exception Terminated

--------------------------------------------------------------------------------
data Exec =
  Exec { _execPub    :: STM Publish
       , _finishLock :: TMVar ()
       }

--------------------------------------------------------------------------------
stageSTM :: TVar Stage -> STM Publish
stageSTM var = do
  stage <- readTVar var
  case stage of
    Init          -> retrySTM
    Available pub -> return pub
    Errored msg   -> throwSTM $ Terminated msg

--------------------------------------------------------------------------------
errored :: TVar Stage -> String -> STM ()
errored var err = do
  stage <- readTVar var
  case stage of
    Errored _ -> return ()
    _         -> writeTVar var (Errored err)

--------------------------------------------------------------------------------
initServicePending :: ServicePendingInit
initServicePending = foldMap (\svc -> singletonMap svc ()) [minBound..]

--------------------------------------------------------------------------------
newExec :: Settings -> Discovery -> IO Exec
newExec setts disc = do
  mainBus <- newBus "main-bus"
  var     <- newTVarIO Init
  exe     <- Exec (stageSTM var) <$> newEmptyTMVarIO
  initRef <- newIORef initServicePending
  logMgr  <- newLogManager (s_loggerSettings setts)

  let logger = getLogger "Exec" logMgr

  connectionManager (getLogger "ConnectionManager" logMgr) setts disc mainBus
  operationManager (getLogger "OperationManager" logMgr) setts mainBus
  subscriptionManager (getLogger "SubscriptionManager" logMgr) setts mainBus

  subscribe mainBus (onInit logger initRef var mainBus)
  subscribe mainBus (onInitFailed logger mainBus var)
  subscribe mainBus (onShutdown logger mainBus)
  subscribe mainBus (onFatal logger)

  publish mainBus SystemInit

  return exe

--------------------------------------------------------------------------------
onInit :: Logger
       -> IORef ServicePendingInit
       -> TVar Stage
       -> Bus
       -> Initialized
       -> IO ()
onInit logger ref var bus (Initialized svc) = do
  logFormat logger Info "Service {} initialized" (Only $ Shown svc)
  initialized <- atomicModifyIORef' ref $ \m ->
    let m' = deleteMap svc m in
    (m', null m')

  when initialized $ do
    logMsg logger Info "Entire system initialized properly"
    atomically $ writeTVar var (Available $ asPub bus)

--------------------------------------------------------------------------------
onInitFailed :: Logger -> Bus -> TVar Stage -> InitFailed -> IO ()
onInitFailed logger bus var (InitFailed svc) = do
  atomically $ errored var "Driver failed to initialized"
  logFormat logger Error "Service {} failed to initialize" (Only $ Shown svc)
  busStop bus

--------------------------------------------------------------------------------
onShutdown :: Logger -> Bus -> SystemShutdown -> IO ()
onShutdown logger bus _ = do
  logMsg logger Info "Driver shutdown by the user"
  busStop bus

--------------------------------------------------------------------------------
onFatal :: Logger -> FatalException -> IO ()
onFatal logger (FatalException e) = do
  logFormat logger Fatal "Fatal exception: {}" (Only $ Shown e)