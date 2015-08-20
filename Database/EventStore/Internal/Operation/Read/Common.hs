{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.Read.Common
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.Read.Common where

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
import Data.Text

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Enumeration detailing the possible outcomes of reading a slice of stream.
data ReadResult        :: StreamType -> * -> * where
    ReadSuccess        :: a -> ReadResult t a
    ReadNoStream       :: ReadResult 'RegularStream a
    ReadStreamDeleted  :: ReadResult 'RegularStream a
    ReadNotModified    :: ReadResult t a
    ReadError          :: Maybe Text -> ReadResult t a
    ReadAccessDenied   :: ReadResult t a

--------------------------------------------------------------------------------
data Slice t where
    Slice { sliceStream          :: !Text
          , sliceFromEventNumber :: !Int32
          , sliceNextEventNumber :: !Int32
          , sliceLastEventNumber :: !Int32
          , sliceEOS             :: !Bool
          , sliceEvents          :: ![ResolvedEvent]
          , sliceDirection       :: !ReadDirection
          } :: Slice 'RegularType

    AllSlice { sliceFromPosition :: !Position
             , sliceNextPosition :: !Position
             , sliceEOS          :: !Bool
             , sliceEvents       :: ![ResolvedEvent]
             , sliceDirection    :: !ReadDirection
             } :: Slice 'All