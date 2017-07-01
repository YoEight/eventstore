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
import Database.EventStore.Internal.Control
import Database.EventStore.Internal.Prelude

--------------------------------------------------------------------------------
import Test.Common
import Test.Tasty.Hspec

--------------------------------------------------------------------------------
spec :: Spec
spec = beforeAll (createLoggerRef testGlobalLog) $ do
  specify "Bus dispatches only one time" $ \logRef -> do
    bus <- newBus logRef testSettings

    ref <- newIORef (0 :: Int)
    subscribe bus $ \Foo ->
      atomicModifyIORef' ref $ \i -> (i+1, ())

    publishWith bus Foo
    busStop bus
    busProcessedEverything bus

    cnt <- readIORef ref

    cnt `shouldBe` 1

  specify "Bus dispatches given and parent message type" $ \logRef -> do
    bus <- newBus logRef testSettings

    ref <- newIORef (0 :: Int)
    subscribe bus $ \Foo ->
      atomicModifyIORef' ref $ \i -> (i+1, ())

    subscribe bus $ \(_ :: Message) ->
      atomicModifyIORef' ref $ \i -> (i+1, ())

    publishWith bus Foo
    busStop bus
    busProcessedEverything bus

    cnt <- readIORef ref
    cnt `shouldBe` 2
