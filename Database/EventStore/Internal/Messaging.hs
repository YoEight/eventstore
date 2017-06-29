{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Messaging
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Messaging
  ( Pub(..)
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
  , subscribe
  ) where

--------------------------------------------------------------------------------
import Data.Typeable
import Data.Typeable.Internal
import Control.Monad.Fix

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Message = forall a. Typeable a => Message a deriving Typeable

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
publish :: (Pub p, Typeable a, MonadIO m) => p -> a -> m ()
publish p a = atomically $ do
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
publishing Bus{..} callbacks a = do
  let tpe = getType (FromTypeable a)
  runEventStore _busLoggerRef _busSettings $ do
    $(logDebug) [i|Publishing message #{tpe}.|]
    traverse_ (propagate a) (lookup tpe callbacks)
    $(logDebug) [i|Message #{tpe} propagated.|]

    unless (tpe == messageType) $
      traverse_ (propagate (toMsg a)) (lookup messageType callbacks)
