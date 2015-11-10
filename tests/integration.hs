{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Main
-- Copyright : (C) 2015 Yorick Laupa
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
import Database.EventStore
import Test.Tasty
import Test.Tasty.Ingredients.Basic

--------------------------------------------------------------------------------
import Tests

--------------------------------------------------------------------------------
main :: IO ()
main = do
    let setts = defaultSettings
                { s_credentials = Just $ credentials "admin" "changeit"
                , s_reconnect_delay_secs = 1
                }
    conn <- connect setts "127.0.0.1" 1113
    let tree = tests conn
    defaultMainWithIngredients [consoleTestReporter] tree
