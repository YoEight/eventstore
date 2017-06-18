{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
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
  , busName
  , Message
  , toMsg
  , fromMsg
  , busProcessedEverything
  , publish
  ) where

--------------------------------------------------------------------------------
import Data.Typeable
import Data.Typeable.Internal
import Control.Monad.Fix

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Logger

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
publish :: (Pub p, Typeable a) => p -> a -> IO ()
publish p a = atomically $ do
  _ <- publishSTM p a
  return ()

--------------------------------------------------------------------------------
class Sub s where
  subscribe :: Typeable a => s -> (a -> IO ()) -> IO ()

--------------------------------------------------------------------------------
data Publish = forall p. Pub p => Publish p

--------------------------------------------------------------------------------
instance Pub Publish where
  publishSTM (Publish p) a = publishSTM p a

--------------------------------------------------------------------------------
data Subscribe = forall p. Sub p => Subscribe p

--------------------------------------------------------------------------------
instance Sub Subscribe where
  subscribe (Subscribe p) a = subscribe p a

--------------------------------------------------------------------------------
data Hub = forall h. (Sub h, Pub h) => Hub h

--------------------------------------------------------------------------------
instance Sub Hub where
  subscribe (Hub h) = subscribe h

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
  hashWithSalt i (Type _ (Fingerprint b l)) = hashWithSalt i (b, l)

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
type Callbacks = HashMap Type (Seq Callback)

--------------------------------------------------------------------------------
propagate :: Typeable a => Logger -> a -> Seq Callback -> IO ()
propagate logger a = traverse_ $ \(Callback k) -> do
  let Just b = cast a
  outcome <- tryAny $ k b
  case outcome of
    Right _ ->
      return ()
    Left e ->
      logFormat logger Error "Exception when propagating {}" (Only $ Shown e)

--------------------------------------------------------------------------------
data Callback =
  forall a. Typeable a => Callback { _callbackKey :: a -> IO () }

--------------------------------------------------------------------------------
instance Show Callback where
  show (Callback (_ :: a -> IO ())) =
    "Handle " <> show (typeRep (Proxy :: Proxy a))

--------------------------------------------------------------------------------
data Bus =
  Bus { busName        :: Text
      , _logger        :: Logger
      , _busCallbacks  :: IORef Callbacks
      , _busQueue      :: TBMQueue Message
      , _workerAsync   :: Async ()
      }

--------------------------------------------------------------------------------
busStop :: Bus -> IO ()
busStop Bus{..} = atomically $ closeTBMQueue _busQueue

--------------------------------------------------------------------------------
busProcessedEverything :: Bus -> IO ()
busProcessedEverything Bus{..} = waitAsync _workerAsync

--------------------------------------------------------------------------------
messageType :: Type
messageType = getType (FromProxy (Proxy :: Proxy Message))

--------------------------------------------------------------------------------
newBus :: LogManager -> Text -> IO Bus
newBus logMgr name = do
  bus <- mfix $ \b -> do

    let logger = getLogger name logMgr
    Bus name logger <$> newIORef mempty
                    <*> newTBMQueueIO 500
                    <*> async (worker b)

  return bus

--------------------------------------------------------------------------------
worker :: Bus -> IO ()
worker Bus{..} = loop
  where
    handleMsg (Message a) = do
      callbacks <- readIORef _busCallbacks
      publishing _logger callbacks a
      loop

    loop = traverse_ handleMsg =<< atomically (readTBMQueue _busQueue)

--------------------------------------------------------------------------------
instance Sub Bus where
  subscribe Bus{..} (k :: a -> IO ()) =
    atomicModifyIORef' _busCallbacks update
    where
      update :: Callbacks -> (Callbacks, ())
      update callbacks =
        let tpe  = getType (FromProxy (Proxy :: Proxy a))
            hdl  = Callback k
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
publishing :: Typeable a => Logger -> Callbacks -> a -> IO ()
publishing logger callbacks a = do
  let tpe = getType (FromTypeable a)
  logFormat logger Debug "Publishing message {}." (Only $ Shown tpe)
  traverse_ (propagate logger a) (lookup tpe callbacks)
  logFormat logger Debug "Message {} propagated." (Only $ Shown tpe)
  if tpe == messageType
    then return ()
    else traverse_ (propagate logger (toMsg a)) (lookup messageType callbacks)
