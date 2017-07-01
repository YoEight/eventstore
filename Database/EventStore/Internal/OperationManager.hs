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
  , new
  , submit
  , handle
  , cleanup
  , check
  ) where

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Callback
import Database.EventStore.Internal.Connection
import Database.EventStore.Internal.Control
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Manager.Operation.Registry
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Manager = Manager { _reg :: Registry }

--------------------------------------------------------------------------------
new :: ConnectionRef -> IO Manager
new = fmap Manager . newRegistry

--------------------------------------------------------------------------------
submit :: Manager -> Operation a -> Callback a -> EventStore ()
submit Manager{..} op cb = register _reg op cb

--------------------------------------------------------------------------------
handle :: Manager -> Package -> EventStore (Maybe Decision)
handle Manager{..} pkg = handlePackage _reg pkg

--------------------------------------------------------------------------------
cleanup :: Manager -> EventStore ()
cleanup Manager{..} = do
  $(logInfo) "Cleaning up pending requests..."
  abortPendingRequests _reg
  $(logInfo) "Cleanup done successfully."

--------------------------------------------------------------------------------
check :: Manager -> EventStore ()
check Manager{..} = do
  checkAndRetry _reg
  startAwaitings _reg
