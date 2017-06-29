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
import Database.EventStore.Internal.Prelude
import Test.Tasty
import Test.Tasty.Hspec

--------------------------------------------------------------------------------
import qualified Test.Bus         as Bus
import qualified Test.Connection  as Connection
import qualified Test.Integration as Integration
import qualified Test.Operation   as Operation

--------------------------------------------------------------------------------
main :: IO ()
main = do
  internal <- sequence [ testSpec "Bus" Bus.spec
                       , testSpec "Connection" Connection.spec
                       , testSpec "Operation" Operation.spec
                       ]

  integration <- Integration.tests

  let tree = [ testGroup "Internal" internal
             , testGroup "Integration" integration
             ]

  defaultMain (testGroup "EventStore tests" tree)
