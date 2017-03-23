{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.SubscriptionManager
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.SubscriptionManager
  ( subscriptionManager ) where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Generator
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Manager.Subscription.Driver
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.Callback
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
type SubDriver = Driver (IO ())

--------------------------------------------------------------------------------
data Internal =
  Internal { _setts   :: Settings
           , _logger  :: Logger
           , _mainBus :: Bus
           , _ref     :: IORef SubDriver
           }

--------------------------------------------------------------------------------
subscriptionManager :: Logger -> Settings -> Bus -> IO ()
subscriptionManager logger setts mainBus = do
  gen      <- newGenerator
  internal <- Internal setts logger mainBus <$> newIORef (newDriver setts gen)

  subscribe mainBus (onInit internal)
  subscribe mainBus (onSub internal)
  subscribe mainBus (onRecv internal)
  subscribe mainBus (onAbort internal)
  subscribe mainBus (onShutdown internal)

--------------------------------------------------------------------------------
onInit :: Internal -> SystemInit -> IO ()
onInit Internal{..} _ =
  publish _mainBus (Initialized SubscriptionManager)

--------------------------------------------------------------------------------
onSub :: Internal -> SubmitSubscription -> IO ()
onSub Internal{..} cmd = do
  pkg <- atomicModifyIORef' _ref $ \driver ->
    let (pkg, nextDriver) =
          case cmd of
            ConnectStream p s tos ->
              connectToStream (fulfill p) s tos driver
            ConnectPersist p g s b ->
              connectToPersist (fulfill p) g s b driver
            CreatePersist p g s ss ->
              createPersist (fromEither p) g s ss driver
            UpdatePersist p g s ss ->
              updatePersist (fromEither p) g s ss driver
            DeletePersist p g s ->
              deletePersist (fromEither p) g s driver
            AckPersist p r uids ->
              ackPersist (fulfill p ()) r uids driver
            NakPersist p r a t uids ->
              nakPersist (fulfill p ()) r a t uids driver
            Unsubscribe r ->
              unsubscribe r driver in
    (nextDriver, pkg)

  publish _mainBus (TcpSend pkg)

--------------------------------------------------------------------------------
onRecv :: Internal -> PackageReceived -> IO ()
onRecv Internal{..} (PackageReceived pkg) = do
  outcome <- atomicModifyIORef' _ref $ \driver ->
    case submitPackage pkg driver of
      Just (action, nextDriver) -> (nextDriver, Just action)
      Nothing                   -> (driver, Nothing)

  fold outcome

--------------------------------------------------------------------------------
onAbort :: Internal -> Abort -> IO ()
onAbort Internal{..} _ = do
  driver <- readIORef _ref

  fold (abort driver)

--------------------------------------------------------------------------------
onShutdown :: Internal -> SystemShutdown -> IO ()
onShutdown Internal{..} _ = do
  logMsg _logger Info "Shutting down..."
  publish _mainBus (ServiceTerminated SubscriptionManager)