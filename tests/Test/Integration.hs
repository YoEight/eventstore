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
import ClassyPrelude
import Database.EventStore
import Test.Common
import Test.Tasty

--------------------------------------------------------------------------------
import qualified Test.Integration.Tests as Tests

--------------------------------------------------------------------------------
tests :: IO [TestTree]
tests = do
    let setts = defaultSettings
                { s_credentials = Just $ credentials "admin" "changeit"
                , s_reconnect_delay = 3
                , s_logger = Nothing
                , s_loggerSettings = testLoggerSettings
                }

    conn <- connect setts (Static "127.0.0.1" 1113)
    return $ Tests.tests conn
