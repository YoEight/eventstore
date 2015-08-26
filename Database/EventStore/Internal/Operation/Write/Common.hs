--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.Write.Common
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.Write.Common where

--------------------------------------------------------------------------------
import Data.Int

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Returned after writing to a stream.
data WriteResult
    = WriteResult
      { writeNextExpectedVersion :: !Int32
        -- ^ Next expected version of the stream.
      , writePosition :: !Position
        -- ^ 'Position' of the write.
      }
    deriving (Eq, Show)
