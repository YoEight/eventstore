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
  ) where

--------------------------------------------------------------------------------
import Data.Typeable
import Data.Typeable.Internal
import Control.Monad.Fix

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Sequence (ViewL(..), viewl, (|>))

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
  publish :: Typeable a => p -> a -> IO ()

--------------------------------------------------------------------------------
class Sub s where
  subscribe :: Typeable a => s -> (a -> IO ()) -> IO ()

--------------------------------------------------------------------------------
data Publish = forall p. Pub p => Publish p

--------------------------------------------------------------------------------
instance Pub Publish where
  publish (Publish p) a = publish p a

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
  publish (Hub h) = publish h

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
      , _busCallbacks  :: TVar Callbacks
      , _busQueue      :: TQueue Message
      , _busStopped    :: TVar Bool
      , _workerRunning :: TVar Bool
      }

--------------------------------------------------------------------------------
busStop :: Bus -> IO ()
busStop Bus{..} = atomically $ writeTVar _busStopped True

--------------------------------------------------------------------------------
busProcessedEverything :: Bus -> IO ()
busProcessedEverything Bus{..} = atomically $ do
  stopped <- readTVar _busStopped
  empty   <- isEmptyTQueue _busQueue
  running <- readTVar _workerRunning

  unless (stopped && empty && not running) $
    retrySTM

--------------------------------------------------------------------------------
messageType :: Type
messageType = getType (FromProxy (Proxy :: Proxy Message))

--------------------------------------------------------------------------------
newBus :: LogManager -> Text -> IO Bus
newBus logMgr name = do
  bus <- mfix $ \b -> do
    _ <- fork (worker b)

    let logger = getLogger name logMgr
    Bus name logger <$> newTVarIO mempty
                    <*> newTQueueIO
                    <*> newTVarIO False
                    <*> newTVarIO True

  return bus

--------------------------------------------------------------------------------
worker :: Bus -> IO ()
worker b@Bus{..} = loop
  where
    loop = do
      decision <- atomically $ do
        stopped   <- readTVar _busStopped
        outcome   <- tryReadTQueue _busQueue
        callbacks <- readTVar _busCallbacks
        case outcome of
          Nothing
            | stopped -> do
                writeTVar _workerRunning False
                return Nothing
            | otherwise -> retrySTM
          Just (Message a) -> return $ Just (publishing _logger callbacks a)
      case decision of
        Nothing     -> return ()
        Just action -> do
          action
          loop

--------------------------------------------------------------------------------
instance Sub Bus where
  subscribe Bus{..} (k :: a -> IO ()) = atomically $ do
      stopped <- readTVar _busStopped

      unless stopped $ do
        m <- readTVar _busCallbacks
        let tpe  = getType (FromProxy (Proxy :: Proxy a))
            hdl  = Callback k
            next = alterMap $ \input ->
              case input of
                Nothing -> Just (singleton hdl)
                Just hs -> Just (snoc hs hdl)

        writeTVar _busCallbacks (next tpe m)

--------------------------------------------------------------------------------
instance Pub Bus where
  publish Bus{..} a = atomically $ do
    stopped <- readTVar _busStopped

    unless stopped $
      writeTQueue _busQueue (toMsg a)

--------------------------------------------------------------------------------
publishing :: Typeable a => Logger -> Callbacks -> a -> IO ()
publishing logger callbacks a = do
  let tpe = getType (FromTypeable a)

  traverse_ (propagate logger a) (lookup tpe callbacks)
  if tpe == messageType
    then return ()
    else traverse_ (propagate logger (toMsg a)) (lookup messageType callbacks)
