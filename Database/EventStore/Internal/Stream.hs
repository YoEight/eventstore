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
data StreamType = All | RegularStream

--------------------------------------------------------------------------------
-- | Represents a regular stream name or $all stream.
data StreamName = StreamName Text | AllStream

--------------------------------------------------------------------------------
instance Show StreamName where
    show (StreamName t) = show t
    show AllStream      = "$all"
