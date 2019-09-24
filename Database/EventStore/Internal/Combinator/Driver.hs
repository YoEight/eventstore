--------------------------------------------------------------------------------
-- |
-- Module    :  Database.EventStore.Internal.Combinator.Driver
-- Copyright :  (C) 2019 Vente Privée
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <ylaupa@vente-privee.com>
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Combinator.Driver where

--------------------------------------------------------------------------------
import Control.Concurrent.Async.Lifted
import qualified Control.Concurrent.STM.TMQueue as TMQueue
import Control.Exception (Exception)
import Data.Functor (void)
import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Control
import qualified Database.EventStore.Internal.Connection as ConnectionImpl
import Database.EventStore.Internal.Discovery
import Database.EventStore.Internal.Driver
import           Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Stopwatch
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data DriverRef =
  DriverRef
  { driverRefQueue :: TMQueue.TMQueue Msg }

--------------------------------------------------------------------------------
publishMsg :: DriverRef -> Msg -> IO ()
publishMsg ref = atomically . TMQueue.writeTMQueue (driverRefQueue ref)

--------------------------------------------------------------------------------
newtype Ticker = Ticker { tickerStopped :: IORef Bool }

--------------------------------------------------------------------------------
newTicker :: IO Ticker
newTicker = Ticker <$> newIORef False

--------------------------------------------------------------------------------
startTicker :: Ticker -> DriverRef -> Duration -> IO ()
startTicker (Ticker ref) driverRef (Duration timespan) = void . fork $ go timespan
  where
    go n = do
      when (n > 0) $ do
        let waiting = min n (fromIntegral (maxBound :: Int))
        threadDelay $ fromIntegral waiting
        go (timespan - waiting)

      publishMsg driverRef Tick
      stopped <- readIORef ref
      unless stopped $ go timespan

--------------------------------------------------------------------------------
newDriverRef :: IO DriverRef
newDriverRef = DriverRef <$> TMQueue.newTMQueueIO

--------------------------------------------------------------------------------
createDriverImpl :: DriverRef
                 -> Settings
                 -> Discovery
                 -> EventStore (Driver EventStore)
createDriverImpl ref setts disc = do
  builder <- liftIO $ ConnectionImpl.connectionBuilder setts
  watch <- newStopwatch
  let impl =
        Driver { connect = connectImpl builder
               , forceReconnect = forceReconnectImpl builder
               , generateId = generateIdImpl
               , discover = discoverImpl
               , getElapsedTime = getElapsedTimeImpl watch
               , stop = stopImpl
               , getSettings = pure setts
               , output = outputImpl
               }

  pure impl

  where
    connectImpl builder ept =
      createConnectionImpl =<< ConnectionImpl.connect builder ept

    forceReconnectImpl = undefined

    generateIdImpl = newUUID

    discoverImpl = void . fork $ do
      tryAny (runDiscovery disc Nothing) >>= \case
        Left e -> do
          $logError
            [i| Failed to resolve TCP endpoint to which to connect #{e}.|]
          liftIO $ publishMsg ref (CloseConnection e)
        Right opt ->
          case opt of
            Nothing ->
              $logWarn
                "Failed to resolve TCP endpoint to which to connect."
            Just ept -> liftIO $ publishMsg ref (EstablishConnection ept)

    getElapsedTimeImpl = stopwatchElapsed

    stopImpl = undefined

    outputImpl = undefined

--------------------------------------------------------------------------------
createConnectionImpl :: ConnectionImpl.Connection -> EventStore (Connection EventStore)
createConnectionImpl = undefined
