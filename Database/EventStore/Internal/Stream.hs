{-# LANGUAGE GADTs          #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Stream
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Stream where

--------------------------------------------------------------------------------
import Data.Text

--------------------------------------------------------------------------------
-- | A stream can either point to $all or a regular one.
data StreamType = All | RegularStream deriving (Eq, Ord)

--------------------------------------------------------------------------------
-- | Represents a regular stream name or $all stream.
data StreamName = StreamName Text | AllStream deriving Eq

--------------------------------------------------------------------------------
instance Show StreamName where
    show (StreamName t) = show t
    show AllStream      = "$all"
