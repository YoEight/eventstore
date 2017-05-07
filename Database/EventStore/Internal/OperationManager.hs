{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.OperationManager
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.OperationManager
  ( operationManager ) where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Manager.Operation.Registry
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Internal =
  Internal { _logger  :: Logger
           , _mainBus :: Bus
           , _reg     :: Registry
           }

--------------------------------------------------------------------------------
operationManager :: Logger -> Settings -> Bus -> IO ()
operationManager logger setts mainBus = do
  internal <- Internal logger mainBus <$> newRegistry setts mainBus

  subscribe mainBus (onInit internal)
  subscribe mainBus (onNew internal)
  subscribe mainBus (onRecv internal)
  subscribe mainBus (onShutdown internal)
  subscribe mainBus (onCheck internal)

--------------------------------------------------------------------------------
onInit :: Internal -> SystemInit -> IO ()
onInit Internal{..} _ = publish _mainBus (Initialized OperationManager)

--------------------------------------------------------------------------------
onNew :: Internal -> SubmitOperation -> IO ()
onNew Internal{..} (SubmitOperation cb op) = register _reg op cb

--------------------------------------------------------------------------------
onRecv :: Internal -> PackageReceived -> IO ()
onRecv Internal{..} (PackageReceived pkg) = handlePackage _reg pkg

--------------------------------------------------------------------------------
onShutdown :: Internal -> SystemShutdown -> IO ()
onShutdown Internal{..} _ = do
  logMsg _logger Info "Shutting down..."
  abortPendingRequests _reg
  publish _mainBus (ServiceTerminated OperationManager)

--------------------------------------------------------------------------------
onCheck :: Internal -> Check -> IO ()
onCheck Internal{..} _ = checkAndRetry _reg
