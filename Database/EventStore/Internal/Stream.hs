{-# LANGUAGE GADTs             #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Represents a regular stream name or $all stream.
data StreamId loc where
    StreamName :: Text -> StreamId EventNumber
    All        :: StreamId Position

--------------------------------------------------------------------------------
-- | If the stream is the $all stream.
isAllStream :: StreamId t -> Bool
isAllStream StreamName{} = False
isAllStream _            = True

--------------------------------------------------------------------------------
instance Eq (StreamId t) where
  StreamName n == StreamName v = n == v
  All          == _            = True

--------------------------------------------------------------------------------
type StreamName = StreamId EventNumber

--------------------------------------------------------------------------------
streamIdRaw :: StreamId t -> Text
streamIdRaw (StreamName n) = n
streamIdRaw All            = ""

--------------------------------------------------------------------------------
instance Show (StreamId t) where
    show (StreamName n) = show n
    show All            = "$all"
