--------------------------------------------------------------------------------
-- |
-- Module : Main
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
-- Main integration entry point.
--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
import Test.Tasty
import Test.Tasty.Hspec

--------------------------------------------------------------------------------
import qualified Test.Connection  as Connection
import qualified Test.Integration as Integration

--------------------------------------------------------------------------------
main :: IO ()
main = do
  internal    <- sequence [ testSpec "Connection" Connection.spec ]
  integration <- Integration.tests

  let tree = [ testGroup "Internal" internal
             , testGroup "Integration" integration
             ]

  defaultMain (testGroup "EventStore tests" tree)
