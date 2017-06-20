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
import Data.UUID.V4

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Connection
import Database.EventStore.Internal.EndPoint
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Creates a 'ConnectionBuilder' that always fails. The first parameter is an
--   action that will be executed every time the builder is ask to create a
--   connection. In this case, it fails because it never sends to the
--   connection manager 'ConnectionEstablished' event.
alwaysFailConnectionBuilder :: IO () -> ConnectionBuilder
alwaysFailConnectionBuilder onConnect = ConnectionBuilder $ \ept -> do
  onConnect
  uuid <- nextRandom
  return Connection { connectionId       = uuid
                    , connectionEndPoint = ept
                    , enqueuePackage     = \_ -> return ()
                    , dispose            = return ()
                    }

--------------------------------------------------------------------------------
-- | Creates a 'ConnectionBuilder' that allows to respond to 'Package' different
--   from heartbeat request.
respondWithConnectionBuilder :: Publish
                             -> (Package -> Package)
                             -> ConnectionBuilder
respondWithConnectionBuilder pub resp =
  respondMWithConnectionBuilder pub (\_ -> return . resp)

--------------------------------------------------------------------------------
-- | Creates a 'ConnectionBuilder' that allows to respond to 'Package' different
--   from heartbeat request.
respondMWithConnectionBuilder :: Publish
                              -> (EndPoint -> Package -> IO Package)
                              -> ConnectionBuilder
respondMWithConnectionBuilder pub resp = ConnectionBuilder $ \ept -> do
  uuid <- nextRandom
  let conn = Connection
          { connectionId = uuid
          , connectionEndPoint = ept
          , enqueuePackage = \pkg ->
              case packageCmd pkg of
                cmd | cmd == getCommand 0x01 -> do
                        let rpkg = pkg { packageCmd = getCommand 0x02 }
                        publish pub (PackageArrived conn rpkg)
                    | otherwise -> do
                      rpkg <- resp ept pkg
                      publish pub (PackageArrived conn rpkg)
          , dispose = return ()
          }

  publish pub (ConnectionEstablished conn)
  return conn
