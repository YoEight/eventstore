{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
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
import Data.Int

--------------------------------------------------------------------------------
import Data.Text

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Enumeration detailing the possible outcomes of reading a stream.
data ReadResult        :: StreamType -> * -> * where
    ReadSuccess        :: a -> ReadResult t a
    ReadNoStream       :: ReadResult 'RegularStream a
    ReadStreamDeleted  :: Text -> ReadResult 'RegularStream a
    ReadNotModified    :: ReadResult t a
    ReadError          :: Maybe Text -> ReadResult t a
    ReadAccessDenied   :: StreamName -> ReadResult t a

--------------------------------------------------------------------------------
instance Functor (ReadResult t) where
    fmap f (ReadSuccess a)       = ReadSuccess (f a)
    fmap _ ReadNoStream          = ReadNoStream
    fmap _ (ReadStreamDeleted s) = ReadStreamDeleted s
    fmap _ ReadNotModified       = ReadNotModified
    fmap _ (ReadError e)         = ReadError e
    fmap _ (ReadAccessDenied s)  = ReadAccessDenied s

--------------------------------------------------------------------------------
-- | Gathers common slice operations.
class Slice a where
    type Loc a

    sliceEvents :: a -> [ResolvedEvent]
    -- ^ Gets slice's 'ResolvedEvent's.
    sliceDirection :: a -> ReadDirection
    -- ^ Gets slice's reading direction.
    sliceEOS :: a -> Bool
    -- ^ If the slice reaches the end of the stream.
    sliceFrom :: a -> Loc a
    -- ^ Gets the starting location of this slice.
    sliceNext :: a -> Loc a
    -- ^ Gets the next location of this slice.

--------------------------------------------------------------------------------
-- | Regular stream slice.
data StreamSlice =
    StreamSlice
    { sliceStream :: !Text
    , sliceLast   :: !Int32
    , _ssDir      :: !ReadDirection
    , _ssFrom     :: !Int32
    , _ssNext     :: !Int32
    , _ssEvents   :: ![ResolvedEvent]
    , _ssEOS      :: !Bool
    }

--------------------------------------------------------------------------------
instance Slice StreamSlice where
    type Loc StreamSlice = Int32

    sliceEvents    = _ssEvents
    sliceDirection = _ssDir
    sliceEOS       = _ssEOS
    sliceFrom      = _ssFrom
    sliceNext      = _ssNext

--------------------------------------------------------------------------------
-- | Represents a slice of the $all stream.
data AllSlice =
    AllSlice
    { _saFrom   :: !Position
    , _saNext   :: !Position
    , _saDir    :: !ReadDirection
    , _saEvents :: ![ResolvedEvent]
    , _saEOS    :: !Bool
    }

--------------------------------------------------------------------------------
instance Slice AllSlice where
    type Loc AllSlice = Position

    sliceEvents    = _saEvents
    sliceDirection = _saDir
    sliceEOS       = _saEOS
    sliceFrom      = _saFrom
    sliceNext      = _saNext
