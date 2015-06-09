{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Connection
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Connection
    ( Connection
    , ConnectionException(..)
    , connUUID
    , connClose
    , connFlush
    , connSend
    , connRecv
    , newConnection
    ) where

--------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception
import qualified Data.ByteString as B
import           Data.Typeable
import           System.IO

--------------------------------------------------------------------------------
import Data.UUID
import Network
import System.Random

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Types
import Database.EventStore.Logging

--------------------------------------------------------------------------------
data ConnectionException =
    MaxAttempt HostName Int Int -- ^ HostName Port MaxAttempt's value
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception ConnectionException

--------------------------------------------------------------------------------
data Connection =
    Connection
    { connUUID  :: UUID
    , connClose :: IO ()
    , connFlush :: IO ()
    , connSend  :: B.ByteString -> IO ()
    , connRecv  :: Int -> IO B.ByteString
    }

--------------------------------------------------------------------------------
newConnection :: Settings -> HostName -> Int -> IO Connection
newConnection sett host port =
    case s_retry sett of
        AtMost n ->
            let loop i = do
                    _settingsLog sett (Info $ Connecting i)
                    catch (connect sett host port) $ \(_ :: SomeException) -> do
                        threadDelay delay
                        if n <= i
                            then do
                                _settingsLog sett
                                             $ Error
                                             $ MaxAttemptConnectionReached i
                                throwIO $ MaxAttempt host port n
                            else loop (i + 1) in
             loop 1
        KeepRetrying ->
            let endlessly i = do
                    _settingsLog sett (Info $ Connecting i)
                    catch (connect sett host port) $ \(_ :: SomeException) -> do
                        threadDelay delay >> endlessly (i + 1) in
             endlessly (1 :: Int)
  where
    delay = (s_reconnect_delay_secs sett) * secs

--------------------------------------------------------------------------------
secs :: Int
secs = 1000000

--------------------------------------------------------------------------------
connect :: Settings -> HostName -> Int -> IO Connection
connect sett host port = do
    hdl <- connectTo host (PortNumber $ fromIntegral port)
    hSetBuffering hdl NoBuffering
    uuid <- randomIO
    regularConnection sett hdl uuid

--------------------------------------------------------------------------------
regularConnection :: Settings -> Handle -> UUID -> IO Connection
regularConnection sett h uuid = do
    _settingsLog sett (Info $ Connected uuid)
    return Connection
           { connUUID  = uuid
           , connClose = do
               _settingsLog sett (Info $ ConnectionClosed uuid)
               hClose h
           , connFlush = hFlush h
           , connSend  = B.hPut h
           , connRecv  = B.hGet h
           }
