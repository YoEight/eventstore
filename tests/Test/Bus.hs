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
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.Prelude

--------------------------------------------------------------------------------
import Test.Common
import Test.Tasty.Hspec

--------------------------------------------------------------------------------
spec :: LogManager -> Spec
spec mgr = do
  specify "Bus dispatches only one time" $ do
    bus <- newBus mgr "test"

    ref <- newIORef (0 :: Int)
    subscribe bus $ \Foo ->
      atomicModifyIORef' ref $ \i -> (i+1, ())

    publish bus Foo
    busStop bus
    busProcessedEverything bus

    cnt <- readIORef ref

    cnt `shouldBe` 1

  specify "Bus dispatches given and parent message type" $ do
    bus <- newBus mgr "test"

    ref <- newIORef (0 :: Int)
    subscribe bus $ \Foo ->
      atomicModifyIORef' ref $ \i -> (i+1, ())

    subscribe bus $ \(_ :: Message) ->
      atomicModifyIORef' ref $ \i -> (i+1, ())

    publish bus Foo
    busStop bus
    busProcessedEverything bus

    cnt <- readIORef ref
    cnt `shouldBe` 2
