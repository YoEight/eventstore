--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Generator
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
-- Pure UUID generator.
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Generator
    ( Generator
    , nextUUID
    , newGenerator
    , splitGenerator
    ) where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.UUID
import System.Random

--------------------------------------------------------------------------------
-- | Pure 'UUID' generator.
newtype Generator = Generator StdGen

--------------------------------------------------------------------------------
-- | Gets the next fresh 'UUID'.
nextUUID :: Generator -> (UUID, Generator)
nextUUID (Generator g) =  let (u, nxt) = random g in (u, Generator nxt)

--------------------------------------------------------------------------------
-- | Builds 2 new 'Generator's out of one.
splitGenerator :: Generator -> (Generator, Generator)
splitGenerator (Generator g) =
    let (g1, g2) = split g in (Generator g1, Generator g2)

--------------------------------------------------------------------------------
-- | Creates a new 'Generator'.
newGenerator :: IO Generator
newGenerator = fmap Generator getStdGen
