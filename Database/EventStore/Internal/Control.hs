{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Control
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Control
  (
    -- * Control
    EventStore
  , runEventStore
  , getSettings
  , freshUUID
    -- * Messaging
  , Pub(..)
  , Sub(..)
  , Hub
  , Subscribe
  , Publish
  , asPub
  , asSub
  , asHub
  , Bus
  , newBus
  , busStop
  , Message
  , toMsg
  , fromMsg
  , busProcessedEverything
  , publish
  , publishWith
  , subscribe
  , stopBus
  , publisher
    -- * Monitoring
  , monitorIncrPkgCount
  , monitorIncrConnectionDrop
  , monitorAddDataTransmitted
  , monitorIncrForceReconnect
  , monitorIncrHeartbeatTimeouts
    -- * Re-export
  , module Database.EventStore.Internal.Settings
  ) where

--------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ > 710
import Control.Monad.Fail
#endif
import Data.Typeable
#if __GLASGOW_HASKELL__ < 802
import Data.Typeable.Internal
#else
import GHC.Fingerprint
#endif

--------------------------------------------------------------------------------
import Control.Monad.Reader
import Data.UUID
import Data.UUID.V4
import System.Metrics
import System.Metrics.Counter hiding (add)
import System.Metrics.Distribution

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Settings

--------------------------------------------------------------------------------
data Env =
  Env { __logRef   :: LoggerRef
      , __settings :: Settings
      , __bus      :: Bus
      , __monitor  :: Maybe Monitoring
      }

--------------------------------------------------------------------------------
newtype EventStore a =
  EventStore { unEventStore :: ReaderT Env IO a }
  deriving ( Functor
           , Applicative
           , Monad
#if __GLASGOW_HASKELL__ > 710
           , MonadFail
#endif
           , MonadThrow
           , MonadCatch
           , MonadIO
           , MonadFix
           )

--------------------------------------------------------------------------------
getEnv :: EventStore Env
getEnv = EventStore ask

--------------------------------------------------------------------------------
getSettings :: EventStore Settings
getSettings = __settings <$> getEnv

--------------------------------------------------------------------------------
freshUUID :: EventStore UUID
freshUUID = liftIO nextRandom

--------------------------------------------------------------------------------
publisher :: EventStore Publish
publisher = fmap (asPub . __bus) getEnv

--------------------------------------------------------------------------------
stopBus :: EventStore ()
stopBus = busStop . __bus =<< getEnv

--------------------------------------------------------------------------------
instance MonadBase IO EventStore where
  liftBase m = EventStore $ liftBase m

--------------------------------------------------------------------------------
instance MonadBaseControl IO EventStore where
    type StM EventStore a = a
    liftBaseWith run = EventStore $ do
      env <- ask
      s   <- liftIO $ run (\m -> runReaderT (unEventStore m) env)
      restoreM s
    restoreM = return

--------------------------------------------------------------------------------
instance MonadLogger EventStore where
  monadLoggerLog loc src lvl msg  = do
    loggerRef <- __logRef <$> getEnv
    liftIO $ loggerCallback loggerRef loc src lvl (toLogStr msg)

--------------------------------------------------------------------------------
instance MonadLoggerIO EventStore where
  askLoggerIO = do
    loggerRef <- __logRef <$> getEnv
    return (loggerCallback loggerRef)

--------------------------------------------------------------------------------
runEventStore :: LoggerRef -> Settings -> Bus -> EventStore a -> IO a
runEventStore ref setts bus (EventStore action) =
  runReaderT action (Env ref setts bus (_monitoring bus))

--------------------------------------------------------------------------------
-- Messaging
--------------------------------------------------------------------------------
data Message where
  Message :: Typeable a => a -> Message
  deriving Typeable

--------------------------------------------------------------------------------
instance Show Message where
  show (Message a) = "Message: " <> show (typeOf a)

--------------------------------------------------------------------------------
toMsg :: Typeable a => a -> Message
toMsg = Message

--------------------------------------------------------------------------------
fromMsg :: Typeable a => Message -> Maybe a
fromMsg (Message a) = cast a

--------------------------------------------------------------------------------
class Pub p where
  publishSTM :: Typeable a => p -> a -> STM Bool

--------------------------------------------------------------------------------
publish :: Typeable a => a -> EventStore ()
publish a = do
  bus <- __bus <$> getEnv
  publishWith bus a

--------------------------------------------------------------------------------
publishWith :: (Pub p, Typeable a, MonadIO m) => p -> a -> m ()
publishWith p a = atomically $ do
  _ <- publishSTM p a
  return ()

--------------------------------------------------------------------------------
class Sub s where
  subscribeEventHandler :: s -> EventHandler -> IO ()

--------------------------------------------------------------------------------
subscribe :: (Sub s, Typeable a)
          => s
          -> (a -> EventStore ())
          -> IO ()
subscribe s k = subscribeEventHandler s (EventHandler Proxy k)

--------------------------------------------------------------------------------
data Publish = forall p. Pub p => Publish p

--------------------------------------------------------------------------------
instance Pub Publish where
  publishSTM (Publish p) a = publishSTM p a

--------------------------------------------------------------------------------
data Subscribe = forall p. Sub p => Subscribe p

--------------------------------------------------------------------------------
instance Sub Subscribe where
  subscribeEventHandler (Subscribe p) a = subscribeEventHandler p a

--------------------------------------------------------------------------------
data Hub = forall h. (Sub h, Pub h) => Hub h

--------------------------------------------------------------------------------
instance Sub Hub where
  subscribeEventHandler (Hub h) = subscribeEventHandler h

--------------------------------------------------------------------------------
instance Pub Hub where
  publishSTM (Hub h) = publishSTM h

--------------------------------------------------------------------------------
asSub :: Sub s => s -> Subscribe
asSub = Subscribe

--------------------------------------------------------------------------------
asPub :: Pub p => p -> Publish
asPub = Publish

--------------------------------------------------------------------------------
asHub :: (Sub h, Pub h) => h -> Hub
asHub = Hub

--------------------------------------------------------------------------------
data Type = Type TypeRep Fingerprint

--------------------------------------------------------------------------------
instance Show Type where
  show (Type rep _) = "type " <> show rep

--------------------------------------------------------------------------------
instance Eq Type where
  Type _ a == Type _ b = a == b

--------------------------------------------------------------------------------
instance Ord Type where
  compare (Type _ a) (Type _ b) = compare a b

--------------------------------------------------------------------------------
instance Hashable Type where
  hashWithSalt s (Type _ (Fingerprint b l)) = hashWithSalt s (b, l)

--------------------------------------------------------------------------------
data GetType
  = forall a. Typeable a => FromTypeable a
  | forall prx a. Typeable a => FromProxy (prx a)

--------------------------------------------------------------------------------
getFingerprint :: TypeRep -> Fingerprint
#if __GLASGOW_HASKELL__ == 708
getFingerprint (TypeRep fp _ _) = fp
#else
getFingerprint = typeRepFingerprint
#endif

--------------------------------------------------------------------------------
getType :: GetType -> Type
getType op = Type t (getFingerprint t)
  where
    t = case op of
          FromTypeable a -> typeOf a
          FromProxy prx  -> typeRep prx

--------------------------------------------------------------------------------
type EventHandlers = HashMap Type (Seq EventHandler)

--------------------------------------------------------------------------------
propagate :: Typeable a => a -> Seq EventHandler -> EventStore ()
propagate a = traverse_ $ \(EventHandler _ k) -> do
  let Just b = cast a
      tpe    = typeOf b
  outcome <- tryAny $ k b
  case outcome of
    Right _ -> return ()
    Left e  -> $(logError) [i|Exception when propagating #{tpe}: #{e}.|]

--------------------------------------------------------------------------------
data EventHandler where
  EventHandler :: Typeable a
               => Proxy a
               -> (a -> EventStore ())
               -> EventHandler

--------------------------------------------------------------------------------
instance Show EventHandler where
  show (EventHandler prx _) = "Handle " <> show (typeRep prx)

--------------------------------------------------------------------------------
data Bus =
  Bus { _busLoggerRef      :: LoggerRef
      , _busSettings       :: Settings
      , _busEventHandlers  :: IORef EventHandlers
      , _busQueue          :: TBMQueue Message
      , _workerAsync       :: Async ()
      , _monitoring        :: Maybe Monitoring
      }

--------------------------------------------------------------------------------
busStop :: MonadIO m => Bus -> m ()
busStop Bus{..} = atomically $ closeTBMQueue _busQueue

--------------------------------------------------------------------------------
busProcessedEverything :: Bus -> IO ()
busProcessedEverything Bus{..} = wait _workerAsync

--------------------------------------------------------------------------------
messageType :: Type
messageType = getType (FromProxy (Proxy :: Proxy Message))

--------------------------------------------------------------------------------
newBus :: LoggerRef -> Settings -> IO Bus
newBus ref setts = do
  bus <- mfix $ \b -> do
    Bus ref setts <$> newIORef mempty
                  <*> newTBMQueueIO 500
                  <*> async (worker b)
                  <*> traverse configureMonitoring (s_monitoring setts)

  return bus

--------------------------------------------------------------------------------
worker :: Bus -> IO ()
worker self@Bus{..} = loop
  where
    handleMsg (Message a) = do
      callbacks <- readIORef _busEventHandlers
      publishing self callbacks a
      loop

    loop = traverse_ handleMsg =<< atomically (readTBMQueue _busQueue)

--------------------------------------------------------------------------------
instance Sub Bus where
  subscribeEventHandler Bus{..} hdl@(EventHandler prx _) =
    atomicModifyIORef' _busEventHandlers update
    where
      update :: EventHandlers -> (EventHandlers, ())
      update callbacks =
        let tpe  = getType (FromProxy prx)
            next = alterMap $ \input ->
              case input of
                Nothing -> Just (singleton hdl)
                Just hs -> Just (snoc hs hdl) in
        (next tpe callbacks, ())

--------------------------------------------------------------------------------
instance Pub Bus where
  publishSTM Bus{..} a = do
    closed <- isClosedTBMQueue _busQueue
    writeTBMQueue _busQueue (toMsg a)
    return $ not closed

--------------------------------------------------------------------------------
publishing :: Typeable a => Bus -> EventHandlers -> a -> IO ()
publishing self@Bus{..} callbacks a = do
  let tpe = getType (FromTypeable a)
  runEventStore _busLoggerRef _busSettings self $ do
    $(logDebug) [i|Publishing message #{tpe}.|]
    traverse_ (propagate a) (lookup tpe callbacks)
    $(logDebug) [i|Message #{tpe} propagated.|]

    unless (tpe == messageType) $
      traverse_ (propagate (toMsg a)) (lookup messageType callbacks)

--------------------------------------------------------------------------------
-- Monitoring
--------------------------------------------------------------------------------
data Monitoring =
  Monitoring
  { _pkgCount     :: Counter
  , _connDrops    :: Counter
  , _dataTx       :: Distribution
  , _forceReco    :: Counter
  , _heartTimeout :: Counter
  }

--------------------------------------------------------------------------------
configureMonitoring :: Store -> IO Monitoring
configureMonitoring store =
  Monitoring <$> createCounter "eventstore.packages.received" store
             <*> createCounter "eventstore.connection.drops" store
             <*> createDistribution "eventstore.data.transmitted" store
             <*> createCounter "eventstore.force_reconnect" store
             <*> createCounter "eventstore.heartbeat.timeouts" store

--------------------------------------------------------------------------------
monitorIncrPkgCount :: EventStore ()
monitorIncrPkgCount = do
  Env{..} <- getEnv
  for_ __monitor  $ \Monitoring{..}->
    liftIO $ inc _pkgCount

--------------------------------------------------------------------------------
monitorIncrConnectionDrop :: EventStore ()
monitorIncrConnectionDrop = do
  Env{..} <- getEnv
  for_ __monitor  $ \Monitoring{..}->
    liftIO $ inc _connDrops

--------------------------------------------------------------------------------
monitorAddDataTransmitted :: Int -> EventStore ()
monitorAddDataTransmitted siz = do
  Env{..} <- getEnv
  for_ __monitor  $ \Monitoring{..}->
    liftIO $ add _dataTx (fromIntegral siz)

--------------------------------------------------------------------------------
monitorIncrForceReconnect :: EventStore ()
monitorIncrForceReconnect = do
  Env{..} <- getEnv
  for_ __monitor  $ \Monitoring{..}->
    liftIO $ inc _forceReco

--------------------------------------------------------------------------------
monitorIncrHeartbeatTimeouts :: EventStore ()
monitorIncrHeartbeatTimeouts = do
  Env{..} <- getEnv
  for_ __monitor  $ \Monitoring{..}->
    liftIO $ inc _heartTimeout
