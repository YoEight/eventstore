{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.ConnectionManager
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.ConnectionManager where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Connection
import Database.EventStore.Internal.Discovery
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Connect = Connect

--------------------------------------------------------------------------------
data ConnectionFailure = ConnectionFailure ConnectionException

--------------------------------------------------------------------------------
connectionManager :: Settings -> Discovery -> Logger -> Bus -> IO ()
connectionManager setts disc logger mainBus = do
  pkgQueue <- newTQueueIO

  subscribe mainBus (onInit mainBus)
  subscribe mainBus (onSend pkgQueue)
  subscribe mainBus (onConnect setts disc mainBus pkgQueue)
  subscribe mainBus (onConnectFailure logger mainBus)

--------------------------------------------------------------------------------
onInit :: Bus -> SystemInit -> IO ()
onInit bus _ = do
  publish bus Connect
  publish bus (Initialized ConnectionManager)

--------------------------------------------------------------------------------
onConnect :: Settings
          -> Discovery
          -> Bus
          -> TQueue Package
          -> Connect
          -> IO ()
onConnect setts disc bus queue _ = do
  conn <- newConnection setts disc
  _    <- fork (sender conn queue bus)
  _    <- fork (reader conn bus)
  return ()

--------------------------------------------------------------------------------
onConnectFailure :: Logger -> Bus -> ConnectionFailure -> IO ()
onConnectFailure logger bus (ConnectionFailure e) = do
  logFormat logger Error "Connection error: {}" (Only $ Shown e)

  case e of
    PackageParsingError s -> do
      logFormat logger Error "Malformed package probably a driver issue {}"
        (Only s)
      publish bus Connect
    _ -> publish bus (FatalException e)

--------------------------------------------------------------------------------
onSend :: TQueue Package -> TcpSend -> IO ()
onSend queue (TcpSend pkg) = atomically $ writeTQueue queue pkg

--------------------------------------------------------------------------------
sender :: InternalConnection -> TQueue Package -> Bus -> IO ()
sender conn queue bus = loop
  where
    loop = do
      pkg     <- atomically $ readTQueue queue
      outcome <- try $ connSend conn pkg

      case outcome of
        Left e  -> publish bus (ConnectionFailure e)
        Right _ -> loop

--------------------------------------------------------------------------------
reader :: InternalConnection -> Bus -> IO ()
reader conn bus = loop
  where
    loop = do
      outcome <- try $ connRecv conn
      case outcome of
        Left e    -> publish bus (ConnectionFailure e)
        Right pkg -> publish bus (PackageReceived pkg) >> loop