{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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
  , execWaitTillClosed
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
data Internal =
  Internal { _logger    :: Logger
           , _initRef   :: IORef ServicePendingInit
           , _finishRef :: IORef ServicePendingInit
           , _stageVar  :: TVar Stage
           , _mainBus   :: Bus
           , _finishVar :: TMVar ()
           }

--------------------------------------------------------------------------------
instance Pub Exec where
  publish e a = do
    pub <- atomically $ _execPub e
    publish pub a

--------------------------------------------------------------------------------
execWaitTillClosed :: Exec -> IO ()
execWaitTillClosed e = atomically $ readTMVar $ _finishLock e

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
  logMgr <- newLogManager (s_loggerSettings setts)
  let logger = getLogger "Exec" logMgr

  internal <- Internal logger <$> newIORef initServicePending
                              <*> newIORef initServicePending
                              <*> newTVarIO Init
                              <*> newBus "main-bus"
                              <*> newEmptyTMVarIO

  let stagePub = stageSTM $ _stageVar internal
      exe      = Exec stagePub (_finishVar internal)
      mainBus  = _mainBus internal

  connectionManager logMgr setts disc mainBus
  operationManager (getLogger "OperationManager" logMgr) setts mainBus
  subscriptionManager (getLogger "SubscriptionManager" logMgr) setts mainBus

  subscribe mainBus (onInit internal)
  subscribe mainBus (onInitFailed internal)
  subscribe mainBus (onShutdown internal)
  subscribe mainBus (onFatal internal)
  subscribe mainBus (onTerminated internal)

  publish mainBus SystemInit

  return exe

--------------------------------------------------------------------------------
onInit :: Internal -> Initialized -> IO ()
onInit Internal{..} (Initialized svc) = do
  logFormat _logger Info "Service {} initialized" (Only $ Shown svc)
  initialized <- atomicModifyIORef' _initRef $ \m ->
    let m' = deleteMap svc m in
    (m', null m')

  when initialized $ do
    logMsg _logger Info "Entire system initialized properly"
    atomically $ writeTVar _stageVar (Available $ asPub _mainBus)

--------------------------------------------------------------------------------
onInitFailed :: Internal -> InitFailed -> IO ()
onInitFailed Internal{..} (InitFailed svc) = do
  atomically $ errored _stageVar "Driver failed to initialized"
  logFormat _logger Error "Service {} failed to initialize" (Only $ Shown svc)
  busStop _mainBus

--------------------------------------------------------------------------------
onShutdown :: Internal -> SystemShutdown -> IO ()
onShutdown Internal{..} _ = do
  logMsg _logger Info "Driver shutdown by the user"
  atomically $ writeTVar _stageVar (Errored "Connection closed")

--------------------------------------------------------------------------------
onFatal :: Internal -> FatalException -> IO ()
onFatal Internal{..} situation = do
  case situation of
    FatalException e ->
      logFormat _logger Fatal "Fatal exception: {}" (Only $ Shown e)
    FatalCondition ->
      logMsg _logger Fatal "Driver is in unrecoverable state."

  publish _mainBus SystemShutdown

--------------------------------------------------------------------------------
onTerminated :: Internal -> ServiceTerminated -> IO ()
onTerminated Internal{..} (ServiceTerminated svc) = do
  logFormat _logger Info "Service {} terminated." (Only $ Shown svc)
  shutdown <- atomicModifyIORef' _finishRef $ \m ->
    let m' = deleteMap svc m in
    (m', null m')

  when shutdown $ do
    logMsg _logger Info "Entire system shutdown properly"
    -- FIXME - It locks the driver when enabled.
    -- busStop _mainBus
    atomically $ putTMVar _finishVar ()