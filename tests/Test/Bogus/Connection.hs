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
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Connection
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Creates a 'ConnectionBuilder' that always fails. The first parameter is an
--   action that will be executed every time the builder is ask to create a
--   connection.
alwaysFailConnectionBuilder :: IO () -> ConnectionBuilder
alwaysFailConnectionBuilder onConnect = ConnectionBuilder $ \_ -> do
  onConnect
  fail "we simulate a connection problem."

--------------------------------------------------------------------------------
-- | Creates a 'ConnectionBuilder' that allows to respond to 'Package' different
--   from heartbeat request.
respondWithConnectionBuilder :: (Package -> Package) -> ConnectionBuilder
respondWithConnectionBuilder resp = ConnectionBuilder $ \_ -> do
  chan <- newChan

  return Connection
         { sendPackage = \pkg ->
             case packageCmd pkg of
               cmd | cmd == getCommand 0x01 -> do
                       let rpkg = pkg { packageCmd = getCommand 0x02 }
                       writeChan chan rpkg
                   | otherwise -> writeChan chan (resp pkg)
         , receivePackage = Recv <$> readChan chan
         , dispose = return ()
         }
