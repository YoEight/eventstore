--------------------------------------------------------------------------------
-- |
-- Module : Test.Bogus.Connection
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
-- This module hosts unreliable 'ConnectionBuilder' implementation
-- for testing purpose.
--------------------------------------------------------------------------------
module Test.Bogus.Connection where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Connection

--------------------------------------------------------------------------------
-- | Creates a 'ConnectionBuilder' that always fails. The first parameter is an
--   action that will be executed every time the builder is ask to create a
--   connection.
alwaysFailConnectionBuilder :: IO () -> ConnectionBuilder
alwaysFailConnectionBuilder onConnect = ConnectionBuilder $ \_ -> do
  onConnect
  fail "we simulate a connection problem."
