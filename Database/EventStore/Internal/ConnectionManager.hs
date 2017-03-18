{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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
module Database.EventStore.Internal.ConnectionManager
  ( connectionManager ) where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Connection
import Database.EventStore.Internal.Discovery
import Database.EventStore.Internal.EndPoint
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Connect = Connect (Maybe NodeEndPoints)

--------------------------------------------------------------------------------
data ConnectionFailure = ConnectionFailure ConnectionException

--------------------------------------------------------------------------------
type LastPids = Maybe (ThreadId, ThreadId)

--------------------------------------------------------------------------------
data Internal =
  Internal { _setts    :: Settings
           , _disc     :: Discovery
           , _logger   :: Logger
           , _mainBus  :: Bus
           , _queue    :: TQueue Package
           , _lastPids :: IORef LastPids
           }

--------------------------------------------------------------------------------
connectionManager :: Logger -> Settings -> Discovery -> Bus -> IO ()
connectionManager logger setts disc mainBus = do
  internal <- Internal setts disc logger mainBus <$> newTQueueIO
                                                 <*> newIORef Nothing

  subscribe mainBus (onInit internal)
  subscribe mainBus (onSend internal)
  subscribe mainBus (onConnect internal)
  subscribe mainBus (onConnectFailure internal)
  subscribe mainBus (onForceReconnect internal)

--------------------------------------------------------------------------------
onInit :: Internal -> SystemInit -> IO ()
onInit Internal{..} _ = do
  publish _mainBus (Connect Nothing)
  publish _mainBus (Initialized ConnectionManager)

--------------------------------------------------------------------------------
onConnect :: Internal -> Connect -> IO ()
onConnect i@Internal{..} (Connect nodeMay) = do
  conn <- newConnection _setts _disc
  for_ nodeMay $ \node ->
    connForceReconnect conn node

  spid <- fork (sender i conn)
  wpid <- fork (reader i conn)

  atomicWriteIORef _lastPids (Just (spid, wpid))

--------------------------------------------------------------------------------
onConnectFailure :: Internal -> ConnectionFailure -> IO ()
onConnectFailure i@Internal{..} (ConnectionFailure e) = do
  logFormat _logger Error "Connection error: {}" (Only $ Shown e)

  killExchange i

  case e of
    PackageParsingError s -> do
      logFormat _logger Error "Malformed package probably a driver issue {}"
        (Only s)
      publish _mainBus (Connect Nothing)
    _ -> publish _mainBus (FatalException e)

--------------------------------------------------------------------------------
onSend :: Internal -> TcpSend -> IO ()
onSend Internal{..} (TcpSend pkg) = atomically $ writeTQueue _queue pkg

--------------------------------------------------------------------------------
onForceReconnect :: Internal -> ForceReconnect -> IO ()
onForceReconnect i@Internal{..} (ForceReconnect node) = do
  killExchange i
  publish _mainBus (Connect $ Just node)

--------------------------------------------------------------------------------
sender :: Internal -> InternalConnection -> IO ()
sender Internal{..} conn = loop
  where
    loop = do
      pkg     <- atomically $ readTQueue _queue
      outcome <- try $ connSend conn pkg

      case outcome of
        Left e  -> publish _mainBus (ConnectionFailure e)
        Right _ -> loop

--------------------------------------------------------------------------------
reader :: Internal -> InternalConnection -> IO ()
reader Internal{..} conn = loop
  where
    loop = do
      outcome <- try $ connRecv conn
      case outcome of
        Left e    -> publish _mainBus (ConnectionFailure e)
        Right pkg -> do
          if packageCmd pkg == 0x01
            then let corrId = packageCorrelation pkg
                     resp   = heartbeatResponsePackage corrId in
                  atomically $ writeTQueue _queue resp
            else publish _mainBus (PackageReceived pkg)
          loop

--------------------------------------------------------------------------------
killExchange :: Internal -> IO ()
killExchange Internal{..} = do
  lastPids <- readIORef _lastPids
  for_ lastPids $ \(spid, wpid) -> do
    killThread spid
    killThread wpid