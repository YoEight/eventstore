{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Test.Integration
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
-- Main integration entry point.
--------------------------------------------------------------------------------
module Test.Integration (tests) where

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Test
import Test.Tasty
import Test.Tasty.Hspec

--------------------------------------------------------------------------------
import qualified Test.Integration.Tests as Tests

--------------------------------------------------------------------------------
tests :: IO [TestTree]
tests = testSpecs Tests.spec