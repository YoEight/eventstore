--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Prelude
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Prelude
  ( IsString(..)
  , Semigroup(..)
  , MonadIO(..)
  , Hashable(..)
  , Monoid(..)
  , Down(..)
  , HashMap
  , Seq
  , Set
  , ByteString
  , Text
  , UUID
  , Generic
  , Alternative(..)
  , MonadBaseControl(..)
  , atomically
  , zip
  , zipWith
  , tshow
  , length
  , lift
  , retrySTM
  , fromMaybe
  , unlessM
  , whenM
  , isJust
  , null
  , newUUID
  , module Prelude
  , module Control.Applicative
  , module Data.Int
  , module Data.Foldable
  , module Data.Traversable
  , module Control.Concurrent.Lifted
  , module Control.Concurrent.Async.Lifted
  , module Control.Concurrent.MVar.Lifted
  , module Control.Concurrent.STM
  , module Control.Concurrent.STM.TBMQueue
  , module Control.Exception.Safe
  , module Control.Monad
  , module Control.Monad.Base
  , module Control.Monad.Catch
  , module Control.Monad.Trans.Control
  , module Data.Containers
  , module Data.Functor
  , module Data.IORef.Lifted
  , module Data.Sequences
  , module Data.Time
  , module Data.Typeable
  , module Data.Word
  ) where

--------------------------------------------------------------------------------
import Prelude
  ( IO
  , FilePath
  , Num(..)
  , Show(..)
  , Eq(..)
  , Ord(..)
  , Enum(..)
  , Bounded(..)
  , Either(..)
  , Maybe(..)
  , Bool(..)
  , Integral(..)
  , Float
  , Fractional(..)
  , Ordering(..)
  , Double
  , Integer
  , (.)
  , id
  , ($)
  , otherwise
  , not
  , fromIntegral
  , truncate
  , print
  , realToFrac
  , (||)
  , (&&)
  , error
  , undefined
  , toRational
  , maybe
  , either
  , const
  , flip
  )
import Control.Monad
  ( Monad(..)
  , MonadPlus(..)
  , (=<<)
  , (<=<)
  , foldM
  , foldM_
  , when
  , unless
  , forever
  , ap
  )
import Control.Applicative (Applicative(..), Alternative(..))
import Data.Int
import Data.List (zip, zipWith)
import Data.Maybe (fromMaybe, isJust)
import Data.String (IsString(..))
import Data.Foldable
  ( Foldable
  , foldMap
  , traverse_
  , for_
  , toList
  , forM_
  , foldl'
  )
import Data.Traversable
import Data.Functor
  ( Functor(..)
  , (<$)
  , (<$>)
  )
import Data.Monoid(Monoid(..))
import Data.Ord (Down(..))
import Data.Typeable
import Data.Word
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
import           Control.Concurrent.Lifted hiding (throwTo, yield)
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.MVar.Lifted
import           Control.Concurrent.STM hiding (atomically, retry, check)
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM.TBMQueue
import           Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import           Control.Monad.Trans.Control hiding (embed, embed_)
import           Control.Exception.Safe hiding (handle, throwM, catch)
import           Control.Monad.Base
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import           Data.Containers
import           Data.IORef.Lifted
import           Data.Hashable (Hashable(..))
import           Data.HashMap.Strict (HashMap)
import           Data.MonoTraversable.Unprefixed (length, null)
import           Data.Semigroup
import           Data.Sequences hiding (group)
import           Data.Sequence (Seq)
import           Data.Set (Set)
import           Data.Text (Text)
import           Data.Time
import           Data.UUID (UUID)
import           Data.UUID.V4 (nextRandom)

--------------------------------------------------------------------------------
-- | Generalized version of 'STM.atomically'.
atomically :: MonadIO m => STM a -> m a
atomically = liftIO . STM.atomically

--------------------------------------------------------------------------------
tshow :: Show a => a -> Text
tshow = pack . show

--------------------------------------------------------------------------------
retrySTM :: STM a
retrySTM = STM.retry

--------------------------------------------------------------------------------
-- | Only perform the action if the predicate returns 'True'.
whenM :: Monad m => m Bool -> m () -> m ()
whenM mbool action = mbool >>= flip when action

--------------------------------------------------------------------------------
-- | Only perform the action if the predicate returns 'False'.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mbool action = mbool >>= flip unless action

--------------------------------------------------------------------------------
newUUID :: MonadBase IO m => m UUID
newUUID = liftBase nextRandom
