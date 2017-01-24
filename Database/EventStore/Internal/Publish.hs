--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Publish
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Publish where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
-- | Used to pass messages between internal entities.
newtype Publish a = Publish { publish :: a -> IO () }
