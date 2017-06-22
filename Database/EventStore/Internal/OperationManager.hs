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
  ( Manager
  , Decision(..)
  , KnownConnection(..)
  , new
  , submit
  , handle
  , cleanup
  , check
  ) where

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Callback
import Database.EventStore.Internal.Connection
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Manager.Operation.Registry
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Manager =
  Manager { _logger :: Logger
          , _reg    :: Registry
          }

--------------------------------------------------------------------------------
new :: LogManager -> Settings -> KnownConnection -> IO Manager
new mgr setts conn = Manager logger <$> newRegistry setts regLogger conn
  where
    logger    = getLogger "OperationManager" mgr
    regLogger = getLogger "Registry"  mgr

--------------------------------------------------------------------------------
submit :: Manager -> Operation a -> Callback a -> IO ()
submit Manager{..} op cb = register _reg op cb

--------------------------------------------------------------------------------
handle :: Manager -> Package -> IO (Maybe Decision)
handle Manager{..} pkg = handlePackage _reg pkg

--------------------------------------------------------------------------------
cleanup :: Manager -> IO ()
cleanup Manager{..} = do
  logMsg _logger Info "Cleaning up pending requests..."
  abortPendingRequests _reg
  logMsg _logger Info "cleanup done successfully."

--------------------------------------------------------------------------------
check :: Manager -> Connection -> IO ()
check Manager{..} conn = do
  checkAndRetry _reg conn
  startAwaitings _reg conn
