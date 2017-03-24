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
  , Subscribe
  , Publish
  , subscribe
  , SubDecision(..)
  , asPub
  , asSub
  , Bus
  , newBus
  , busStop
  , busName
  , Message
  , toMsg
  , fromMsg
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
data Message = forall a. Typeable a => Message a

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
data SubDecision
  = Continue
  | Done

--------------------------------------------------------------------------------
class Sub s where
  subscribeTo :: Typeable a => s -> (a -> IO SubDecision) -> IO ()

--------------------------------------------------------------------------------
subscribe :: (Sub s, Typeable a) => s -> (a -> IO ()) -> IO ()
subscribe s k = subscribeTo s $ \msg -> Continue <$ k msg

--------------------------------------------------------------------------------
data Publish = forall p. Pub p => Publish p

--------------------------------------------------------------------------------
instance Pub Publish where
  publish (Publish p) a = publish p a

--------------------------------------------------------------------------------
data Subscribe = forall p. Sub p => Subscribe p

--------------------------------------------------------------------------------
instance Sub Subscribe where
  subscribeTo (Subscribe p) a = subscribeTo p a

--------------------------------------------------------------------------------
asSub :: Sub s => s -> Subscribe
asSub = Subscribe

--------------------------------------------------------------------------------
asPub :: Pub p => p -> Publish
asPub = Publish

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
getType :: GetType -> Type
getType (FromTypeable a) = let t@(TypeRep fp _ _ _) = typeOf a in Type t fp
getType (FromProxy prx)  = let t@(TypeRep fp _ _ _) = typeRep prx in Type t fp

--------------------------------------------------------------------------------
type Callbacks = HashMap Type (Seq Callback)

--------------------------------------------------------------------------------
propagate :: Typeable a => a -> Seq Callback -> IO (Seq Callback)
propagate a = go mempty
  where
    go acc cur =
      case viewl cur of
        EmptyL -> return acc
        h@(Callback k) :< rest -> do
          let Just b = cast a
          outcome <- tryAny $ k b
          case outcome of
            Left _         -> go acc rest
            Right decision ->
              case decision of
                Continue -> go (acc |> h) rest
                Done     -> go acc rest

--------------------------------------------------------------------------------
data Callback =
  forall a. Typeable a =>
  Callback { _callbackKey :: a -> IO SubDecision }

--------------------------------------------------------------------------------
instance Show Callback where
  show (Callback (_ :: a -> IO SubDecision)) =
    "Handle " <> show (typeRep (Proxy :: Proxy a))

--------------------------------------------------------------------------------
data Bus =
  Bus { busName       :: Text
      , _logger       :: Logger
      , _busCallbacks :: IORef Callbacks
      , _busQueue     :: TQueue Message
      , _busStopped   :: TVar Bool
      }

--------------------------------------------------------------------------------
busStop :: Bus -> IO ()
busStop Bus{..} = do
  atomically $ writeTVar _busStopped True

  atomically $
    unlessM (isEmptyTQueue _busQueue) $
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
    Bus name logger <$> newIORef mempty
                    <*> newTQueueIO
                    <*> newTVarIO False

  return bus

--------------------------------------------------------------------------------
worker :: Bus -> IO ()
worker b@Bus{..} = loop
  where
    loop = do
      decision <- atomically $ do
        stopped <- readTVar _busStopped
        outcome <- tryReadTQueue _busQueue
        case outcome of
          Nothing
            | stopped   -> return Nothing
            | otherwise -> retrySTM
          Just msg -> return $ Just msg
      case decision of
        Nothing          -> return ()
        Just (Message a) -> do
          publishing b a
          loop

--------------------------------------------------------------------------------
instance Sub Bus where
  subscribeTo Bus{..} (k :: a -> IO SubDecision) =
    atomicModifyIORef' _busCallbacks $ \m ->
      let tpe  = getType (FromProxy (Proxy :: Proxy a))
          hdl  = Callback k
          next = alterMap $ \input ->
            case input of
              Nothing -> Just (singleton hdl)
              Just hs -> Just (snoc hs hdl) in
      (next tpe m, ())

--------------------------------------------------------------------------------
instance Pub Bus where
  publish Bus{..} a = atomically $ do
    stopped <- readTVar _busStopped

    unless stopped $
      writeTQueue _busQueue (toMsg a)

--------------------------------------------------------------------------------
publishing :: Typeable a => Bus -> a -> IO ()
publishing Bus{..} a = do
  cs <- readIORef _busCallbacks
  let tpe = getType (FromTypeable a)

  act1 <- traverse (propagate a) (lookup tpe cs)
  act2 <- if tpe == messageType
            then return Nothing
            else traverse (propagate (toMsg a)) (lookup messageType cs)

  for_ act1 $ \callbacks ->
    atomicModifyIORef' _busCallbacks $ \m ->
      (insertMap tpe callbacks m, ())

  for_ act2 $ \callbacks ->
    atomicModifyIORef' _busCallbacks $ \m ->
      (insertMap messageType callbacks m, ())
