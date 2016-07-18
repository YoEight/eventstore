{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Results
-- Copyright : (C) 2016 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Results where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Foldable (foldMap)

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Stream
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

--------------------------------------------------------------------------------
-- | Represents the result of looking up a specific event number from a stream.
data ReadEvent
    = ReadEventNotFound
      { readEventStream :: !Text
      , readEventNumber :: !Int32
      }
    | ReadEvent
      { readEventStream   :: !Text
      , readEventNumber   :: !Int32
      , readEventResolved :: !ResolvedEvent
      } deriving Show

--------------------------------------------------------------------------------
-- | Enumeration detailing the possible outcomes of reading a stream.
data ReadResult       :: StreamType -> * -> * where
    ReadSuccess       :: a -> ReadResult t a
    ReadNoStream      :: ReadResult 'RegularStream a
    ReadStreamDeleted :: Text -> ReadResult 'RegularStream a
    ReadNotModified   :: ReadResult t a
    ReadError         :: Maybe Text -> ReadResult t a
    ReadAccessDenied  :: StreamName -> ReadResult t a

--------------------------------------------------------------------------------
instance Eq a => Eq (ReadResult t a) where
    ReadSuccess a       == ReadSuccess b       = a == b
    ReadNoStream        == ReadNoStream        = True
    ReadStreamDeleted s == ReadStreamDeleted v = s == v
    ReadNotModified     == ReadNotModified     = True
    ReadError e         == ReadError u         = e == u
    ReadAccessDenied s  == ReadAccessDenied v  = s == v
    _                   == _                   = False

--------------------------------------------------------------------------------
instance Show a => Show (ReadResult t a) where
    show (ReadSuccess a)       = "ReadSuccess " ++ show a
    show ReadNoStream          = "ReadNoStream"
    show (ReadStreamDeleted s) = "ReadStreamDeleted" ++ show s
    show ReadNotModified       = "ReadNoModified"
    show (ReadError e)         = "ReadError" ++ show e
    show (ReadAccessDenied s)  = "ReadAccessDenied " ++ show s

--------------------------------------------------------------------------------
instance Functor (ReadResult t) where
    fmap f (ReadSuccess a)       = ReadSuccess (f a)
    fmap _ ReadNoStream          = ReadNoStream
    fmap _ (ReadStreamDeleted s) = ReadStreamDeleted s
    fmap _ ReadNotModified       = ReadNotModified
    fmap _ (ReadError e)         = ReadError e
    fmap _ (ReadAccessDenied s)  = ReadAccessDenied s

--------------------------------------------------------------------------------
instance Foldable (ReadResult t) where
    foldMap f (ReadSuccess a) = f a
    foldMap _ _               = mempty

--------------------------------------------------------------------------------
instance Traversable (ReadResult t) where
    traverse f (ReadSuccess a)       = fmap ReadSuccess $ f a
    traverse _ ReadNoStream          = pure ReadNoStream
    traverse _ (ReadStreamDeleted s) = pure $ ReadStreamDeleted s
    traverse _ ReadNotModified       = pure ReadNotModified
    traverse _ (ReadError e)         = pure $ ReadError e
    traverse _ (ReadAccessDenied s)  = pure $ ReadAccessDenied s

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
    } deriving Show

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
    } deriving Show

--------------------------------------------------------------------------------
instance Slice AllSlice where
    type Loc AllSlice = Position

    sliceEvents    = _saEvents
    sliceDirection = _saDir
    sliceEOS       = _saEOS
    sliceFrom      = _saFrom
    sliceNext      = _saNext

--------------------------------------------------------------------------------
-- | Returned after deleting a stream. 'Position' of the write.
newtype DeleteResult = DeleteResult Position deriving (Eq, Show)