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
import ClassyPrelude hiding (handle)

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Callback
import Database.EventStore.Internal.Connection
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Manager.Operation.Registry
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Manager =
  Manager { _logger :: Logger
          , _reg    :: Registry
          }

--------------------------------------------------------------------------------
new :: LogManager -> Settings -> IO Manager
new mgr setts = Manager logger <$> newRegistry setts
  where
    logger = getLogger "OperationManager" mgr

--------------------------------------------------------------------------------
submit :: Manager
       -> Operation a
       -> Callback a
       -> Maybe Connection
       -> IO ()
submit Manager{..} op cb outcome =
  case outcome of
    Just conn -> register _reg conn op cb
    Nothing   -> schedule _reg op cb

--------------------------------------------------------------------------------
handle :: Manager -> Package -> IO (Maybe Decision)
handle Manager{..} pkg = handlePackage _reg pkg

--------------------------------------------------------------------------------
cleanup :: Manager -> IO ()
cleanup Manager{..} = do
  logMsg _logger Info "Shutting down, cleaning up pending requests..."
  abortPendingRequests _reg
  logMsg _logger Info "cleanup done successfully."

--------------------------------------------------------------------------------
check :: Manager -> Connection -> IO ()
check Manager{..} conn = do
  checkAndRetry _reg conn
  startAwaitings _reg conn
