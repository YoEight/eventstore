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
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Control
import Database.EventStore.Internal.Connection
import Database.EventStore.Internal.EndPoint
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Creates a 'ConnectionBuilder' that always fails. The first parameter is an
--   action that will be executed every time the builder is ask to create a
--   connection. In this case, it fails because it never sends to the
--   connection manager 'ConnectionEstablished' event.
alwaysFailConnectionBuilder :: IO () -> ConnectionBuilder
alwaysFailConnectionBuilder onConnect = ConnectionBuilder $ \ept -> do
  liftIO onConnect
  uuid <- freshUUID
  return Connection { connectionId       = uuid
                    , connectionEndPoint = ept
                    , enqueuePackage     = \_ -> return ()
                    , dispose            = return ()
                    }

--------------------------------------------------------------------------------
-- | Creates a 'ConnectionBuilder' that allows to respond to 'Package' different
--   from heartbeat request.
respondWithConnectionBuilder :: (Package -> Package) -> ConnectionBuilder
respondWithConnectionBuilder resp =
  respondMWithConnectionBuilder (\_ -> return . resp)

--------------------------------------------------------------------------------
-- | Creates a 'ConnectionBuilder' that allows to respond to 'Package' different
--   from heartbeat request.
respondMWithConnectionBuilder :: (EndPoint -> Package -> IO Package)
                              -> ConnectionBuilder
respondMWithConnectionBuilder resp = ConnectionBuilder $ \ept -> do
  uuid <- freshUUID
  let conn = Connection
             { connectionId = uuid
             , connectionEndPoint = ept
             , enqueuePackage = \pkg ->
                 case packageCmd pkg of
                   cmd | cmd == getCommand 0x01 -> do
                           let rpkg = pkg { packageCmd = getCommand 0x02 }
                           publish (PackageArrived conn rpkg)
                       | otherwise -> do
                         rpkg <- liftIO $ resp ept pkg
                         publish (PackageArrived conn rpkg)
             , dispose = return ()
             }

  publish (ConnectionEstablished conn)
  return conn

--------------------------------------------------------------------------------
-- | Silent 'ConnectionBuilder'. It never publishes new 'Package's.
silentConnectionBuilder :: IO () -> ConnectionBuilder
silentConnectionBuilder onConnect = ConnectionBuilder $ \ept -> do
  uuid <- freshUUID
  liftIO onConnect
  let conn = Connection
             { connectionId = uuid
             , connectionEndPoint = ept
             , enqueuePackage = \_ -> return ()
             , dispose = return ()
             }

  publish (ConnectionEstablished conn)
  return conn