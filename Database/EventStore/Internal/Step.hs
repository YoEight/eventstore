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
import qualified Data.Sequence as S

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data NonEmpty a = Cons a (S.Seq a)

--------------------------------------------------------------------------------
singleton :: a -> NonEmpty a
singleton a = Cons a S.empty

--------------------------------------------------------------------------------
tailAdd :: a -> NonEmpty a -> NonEmpty a
tailAdd a (Cons h t) = Cons h (t S.|> a)

--------------------------------------------------------------------------------
-- | Common pattern used in Processor and Operation module.
data Step m r
    = Done r (m r)
      -- ^ The model was able to produce a value.
    | Send Package (m r)
      -- ^ The model wants that 'Package' to be sent to the server.
    | Cont (m r)
      -- ^ The model only update its internal state with no intermediary value.
    | Suspended
      -- ^ Need more input to proceed.
