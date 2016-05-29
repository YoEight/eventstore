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
import Data.Time
import Database.EventStore
import Database.EventStore.Logging
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
                , s_logger = Nothing
                }
    conn <- connect setts (Static "127.0.0.1" 1113)
    let tree = tests conn
    defaultMainWithIngredients [consoleTestReporter] tree

--------------------------------------------------------------------------------
_logger :: Log -> IO ()
_logger l = do
    t <- getCurrentTime
    putStr "["
    putStr $ show t
    putStr "]  "
    showLog l
  where
    showLog (Info m) = do
        putStr "[INFO] "
        print m

    showLog (Error m) = do
        putStr "!!! ERROR !!! "
        print m
