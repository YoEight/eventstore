--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Writer
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Writer (writerThread) where

--------------------------------------------------------------------------------
import Control.Concurrent
import Control.Monad

--------------------------------------------------------------------------------
import Data.Serialize.Put

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Connection
import Database.EventStore.Internal.Packages
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
writerThread :: Chan Package -> Connection -> IO ()
writerThread chan c = forever $ do
    pkg <- readChan chan
    connSend c (runPut $ putPackage pkg)
    connFlush c
