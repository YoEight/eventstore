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
import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString as B
import           System.IO

--------------------------------------------------------------------------------
import Data.Serialize.Put

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Packages
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
writerThread :: Chan Package -> Handle -> IO ()
writerThread chan hdl = forever $ do
    pkg <- readChan chan
    B.hPut hdl (runPut $ putPackage pkg)
    hFlush hdl
