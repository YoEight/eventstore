--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Util.Sodium
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Util.Sodium where

--------------------------------------------------------------------------------
import Control.Concurrent (forkIO)
import Data.Functor (void)

--------------------------------------------------------------------------------
import FRP.Sodium

--------------------------------------------------------------------------------
pushAsync :: (a -> Reactive ()) -> a -> IO ()
pushAsync push a = void $ forkIO $ sync $ push a

--------------------------------------------------------------------------------
pushAsync2 :: (a -> b -> Reactive ()) -> a -> b -> IO ()
pushAsync2 push a b = void $ forkIO $ sync $ push a b

--------------------------------------------------------------------------------
pushAsync3 :: (a -> b -> c -> Reactive ()) -> a -> b -> c -> IO ()
pushAsync3 push a b c = void $ forkIO $ sync $ push a b c

--------------------------------------------------------------------------------
pushAsync4 :: (a -> b -> c -> d -> Reactive ()) -> a -> b -> c -> d -> IO ()
pushAsync4 push a b c d = void $ forkIO $ sync $ push a b c d
