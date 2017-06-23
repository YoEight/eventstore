{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.TimerService
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.TimerService
  ( timerService ) where

--------------------------------------------------------------------------------
import Data.Typeable

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Internal =
  Internal { _logger  :: Logger
           , _mainBus :: Hub
           , _stopped :: IORef Bool
           }

--------------------------------------------------------------------------------
timerService :: Logger -> Hub -> IO ()
timerService logger mainBus = do

  internal <- Internal logger mainBus <$> newIORef False

  subscribe mainBus (onInit internal)
  subscribe mainBus (onShutdown internal)
  subscribe mainBus (onNew internal)

--------------------------------------------------------------------------------
delayed :: Typeable e => Internal -> e -> Duration -> Bool -> IO ()
delayed Internal{..} msg (Duration timespan) oneOff = () <$ fork (go timespan)
  where
    go i = do
      when (i > 0) $ do
        let waiting = min i (fromIntegral (maxBound :: Int))
        threadDelay $ fromIntegral waiting
        go (timespan - waiting)

      publish _mainBus msg
      stopped <- readIORef _stopped
      unless (oneOff || stopped) $ go timespan

--------------------------------------------------------------------------------
onInit :: Internal -> SystemInit -> IO ()
onInit Internal{..} _ = publish _mainBus (Initialized TimerService)

--------------------------------------------------------------------------------
onShutdown :: Internal -> SystemShutdown -> IO ()
onShutdown Internal{..} _ = do
  logMsg _logger Info "Shutting down..."
  atomicWriteIORef _stopped True
  publish _mainBus (ServiceTerminated TimerService)

--------------------------------------------------------------------------------
onNew :: Internal -> NewTimer -> IO ()
onNew i (NewTimer msg duration oneOff) = delayed i msg duration oneOff
