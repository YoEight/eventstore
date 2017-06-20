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
import Database.EventStore.Internal.Logger
import Test.Tasty
import Test.Tasty.Hspec

--------------------------------------------------------------------------------
import qualified Test.Bus         as Bus
import           Test.Common
import qualified Test.Connection  as Connection
import qualified Test.Integration as Integration
import qualified Test.Operation   as Operation

--------------------------------------------------------------------------------
main :: IO ()
main = do
  logMgr <- newLogManager testLoggerSettings
  internal <- sequence [ testSpec "Bus" (Bus.spec logMgr)
                       , testSpec "Connection" (Connection.spec logMgr)
                       , testSpec "Operation" (Operation.spec logMgr)
                       ]

  integration <- Integration.tests

  let tree = [ testGroup "Internal" internal
             , testGroup "Integration" integration
             ]

  defaultMain (testGroup "EventStore tests" tree)
