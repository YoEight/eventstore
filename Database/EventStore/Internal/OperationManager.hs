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
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
operationManager :: Logger -> Settings -> Bus -> IO ()
operationManager logger setts mainBus = do
  subscribe mainBus (onInit mainBus)

--------------------------------------------------------------------------------
onInit :: Bus -> SystemInit -> IO ()
onInit mainBus _ =
  publish mainBus (Initialized OperationManager)