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
import Database.EventStore.Internal.Generator
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Manager.Operation.Model
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.Callback
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
type OpModel      = Model (IO ())
type OpTransition = Transition (IO ())

--------------------------------------------------------------------------------
data Internal =
  Internal { _logger  :: Logger
           , _mainBus :: Bus
           , _ref     :: IORef OpModel
           }

--------------------------------------------------------------------------------
operationManager :: Logger -> Settings -> Bus -> IO ()
operationManager logger setts mainBus = do
  gen      <- newGenerator
  internal <- Internal logger mainBus <$> newIORef (newModel setts gen)

  subscribe mainBus (onInit internal)
  subscribe mainBus (onNew internal)
  subscribe mainBus (onRecv internal)
  subscribe mainBus (onAbort internal)
  subscribe mainBus (onShutdown internal)

--------------------------------------------------------------------------------
onInit :: Internal -> SystemInit -> IO ()
onInit Internal{..} _ =
  publish _mainBus (Initialized OperationManager)

--------------------------------------------------------------------------------
onNew :: Internal -> SubmitOperation -> IO ()
onNew i@Internal{..} (SubmitOperation p op) = do
  model <- readIORef _ref
  let callback outcome =
        case outcome of
          Left e -> do
            logFormat _logger Error "Operation error: {}" (Only $ Shown e)
            reject p e
          Right a -> fulfill p a

  nextModel <- interpret i (pushOperation callback op model)
  atomicWriteIORef _ref nextModel

--------------------------------------------------------------------------------
onRecv :: Internal -> PackageReceived -> IO ()
onRecv i@Internal{..} (PackageReceived pkg) = do
  model <- readIORef _ref
  for_ (submitPackage pkg model) $ \outcome -> do
    nextModel <- interpret i outcome
    atomicWriteIORef _ref nextModel

--------------------------------------------------------------------------------
onAbort :: Internal -> Abort -> IO ()
onAbort i@Internal{..} _ = do
  model     <- readIORef _ref
  nextModel <- interpretAbort i (abort model)
  atomicWriteIORef _ref nextModel

--------------------------------------------------------------------------------
onShutdown :: Internal -> SystemShutdown -> IO ()
onShutdown Internal{..} _ = do
  logMsg _logger Info "Shutting down..."
  publish _mainBus (ServiceTerminated OperationManager)

--------------------------------------------------------------------------------
interpret :: Internal -> OpTransition -> IO OpModel
interpret Internal{..} = go
  where
    go (Produce action nxt)  = action >> go nxt
    go (Transmit pkg nxt)    = publish _mainBus (TcpSend pkg) >> go nxt
    go (Await m)             = return m
    go (NotHandled info nxt) = do
      let node = masterInfoNodeEndPoints info
      publish _mainBus (ForceReconnect node)
      go nxt

--------------------------------------------------------------------------------
interpretAbort :: Internal -> OpTransition -> IO OpModel
interpretAbort Internal{..} = go
  where
    go (Produce action nxt) = action >> go nxt
    go (Transmit _ nxt)     = go nxt
    go (Await m)            = return m
    go (NotHandled _ nxt)   = go nxt