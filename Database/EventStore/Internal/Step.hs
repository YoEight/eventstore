--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Step
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Step where

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Common pattern used in Processor and Operation module.
data Step m r
    = Done r (m r)
      -- ^ The model was able to produce a final value.
    | Send Package (m r)
      -- ^ The model wants that 'Package' to be sent to the server.
    | Cont (m r)
      -- ^ The model only update its internal state with no intermediary value.
