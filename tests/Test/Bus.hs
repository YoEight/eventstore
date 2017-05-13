{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module : Test.Bus
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Test.Bus where

--------------------------------------------------------------------------------
import Data.Typeable

--------------------------------------------------------------------------------
import ClassyPrelude
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Messaging

--------------------------------------------------------------------------------
import Test.Common
import Test.Tasty
import Test.Tasty.Hspec

--------------------------------------------------------------------------------
data Foo = Foo deriving Typeable

--------------------------------------------------------------------------------
spec :: Spec
spec = do
  specify "Bus dispatches only one time" $ do
    mgr <- newLogManager testLoggerSettings
    bus <- newBus mgr "test"

    ref <- newIORef 0
    subscribe bus $ \Foo ->
      atomicModifyIORef' ref $ \i -> (i+1, ())

    publish bus Foo
    busStop bus

    cnt <- readIORef ref

    cnt `shouldBe` 1

  specify "Bus dispatches given and parent message type" $ do
    mgr <- newLogManager testLoggerSettings
    bus <- newBus mgr "test"

    ref <- newIORef 0
    subscribe bus $ \Foo ->
      atomicModifyIORef' ref $ \i -> (i+1, ())

    subscribe bus $ \(_ :: Message) ->
      atomicModifyIORef' ref $ \i -> (i+1, ())

    publish bus Foo
    busStop bus

    cnt <- readIORef ref
    cnt `shouldBe` 2
