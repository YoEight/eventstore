--------------------------------------------------------------------------------
-- |
-- Module : Test.Common
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Test.Common where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
newtype Counter = Counter (TVar Int)

--------------------------------------------------------------------------------
newCounter :: IO Counter
newCounter = Counter <$> newTVarIO 0

--------------------------------------------------------------------------------
incrCounter :: Counter -> IO ()
incrCounter (Counter var) = atomically $ do
  i <- readTVar var
  writeTVar var (i+1)

--------------------------------------------------------------------------------
readCounterSTM :: Counter -> STM Int
readCounterSTM (Counter var) = readTVar var
