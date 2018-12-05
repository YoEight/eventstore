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
import Control.Applicative
import Data.Foldable
import Data.Maybe (isNothing)
import Data.Monoid
import Data.Traversable
import Data.Int

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
import Prelude

--------------------------------------------------------------------------------
-- | Enumeration detailing the possible outcomes of reading a stream.
data ReadResult t a where
    ReadSuccess       :: a -> ReadResult t a
    ReadNoStream      :: ReadResult EventNumber a
    ReadStreamDeleted :: StreamName -> ReadResult EventNumber a
    ReadNotModified   :: ReadResult t a
    ReadError         :: Maybe Text -> ReadResult t a
    ReadAccessDenied  :: StreamId t -> ReadResult t a

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
data Slice t
    = SliceEndOfStream
    | Slice ![ResolvedEvent] !(Maybe t)
    deriving Show

--------------------------------------------------------------------------------
-- | Empty slice.
emptySlice :: Slice t
emptySlice = SliceEndOfStream

--------------------------------------------------------------------------------
instance Functor Slice where
    fmap f SliceEndOfStream = SliceEndOfStream
    fmap f (Slice xs next)  = Slice xs (fmap f next)

--------------------------------------------------------------------------------
-- | Gets slice's 'ResolvedEvents's.
sliceEvents :: Slice t -> [ResolvedEvent]
sliceEvents SliceEndOfStream = []
sliceEvents (Slice xs _)     = xs

--------------------------------------------------------------------------------
-- | If the slice has reached the end of the stream.
sliceEOS :: Slice t -> Bool
sliceEOS SliceEndOfStream = True
sliceEOS (Slice _ next)   = isNothing next

--------------------------------------------------------------------------------
-- | Gets the next location of this slice.
sliceNext :: Slice t -> Maybe t
sliceNext SliceEndOfStream = Nothing
sliceNext (Slice _ next)   = next

--------------------------------------------------------------------------------
-- | Regular stream slice.
type StreamSlice = Slice EventNumber

--------------------------------------------------------------------------------
-- | Represents a slice of the $all stream.
type AllSlice = Slice Position
