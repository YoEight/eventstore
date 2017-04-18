{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Callback
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Callback
  ( Callback
  , newPromise
  , newCallback
  , newCallbackSimple
  , fulfill
  , reject
  , retrieve
  , tryRetrieve
  , fromEither
  ) where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
newtype Callback a =
  Callback { runCallback :: forall b. Stage a b -> IO b }

--------------------------------------------------------------------------------
data Stage a b where
  Fulfill  :: a -> Stage a ()
  Reject   :: Exception e => e -> Stage a ()
  Retrieve :: Stage a (Either SomeException a)

--------------------------------------------------------------------------------
fulfill :: Callback a -> a -> IO ()
fulfill cb a = runCallback cb (Fulfill a)

--------------------------------------------------------------------------------
reject :: Exception e => Callback a -> e -> IO ()
reject cb e = runCallback cb (Reject e)

--------------------------------------------------------------------------------
tryRetrieve :: Callback a -> IO (Either SomeException a)
tryRetrieve cb = runCallback cb Retrieve

--------------------------------------------------------------------------------
retrieve :: Callback a -> IO a
retrieve p = do
  outcome <- tryRetrieve p
  case outcome of
    Left e  -> throwIO e
    Right a -> return a


--------------------------------------------------------------------------------
fromEither :: Exception e => Callback a -> Either e a -> IO ()
fromEither p (Left e)  = reject p e
fromEither p (Right a) = fulfill p a

--------------------------------------------------------------------------------
newPromise :: IO (Callback a)
newPromise = do
  mvar <- newEmptyTMVarIO
  return $ promise mvar

--------------------------------------------------------------------------------
newCallback :: (Either SomeException a -> IO ()) -> IO (Callback a)
newCallback k = do
  mvar <- newEmptyTMVarIO
  return $ callback mvar k

--------------------------------------------------------------------------------
newCallbackSimple :: (a -> IO ()) -> IO (Callback a)
newCallbackSimple k = newCallback $ \outcome ->
  case outcome of
    Right a -> k a
    Left (e :: SomeException) -> throw e

--------------------------------------------------------------------------------
promise :: forall a. TMVar (Either SomeException a) -> Callback a
promise mvar = Callback go
  where
    go :: forall b. Stage a b -> IO b
    go (Fulfill a) = atomically $
      whenM (isEmptyTMVar mvar) $
        putTMVar mvar (Right a)

    go (Reject e) = atomically $
      whenM (isEmptyTMVar mvar) $
        putTMVar mvar (Left $ toException e)

    go Retrieve = atomically $ readTMVar mvar

--------------------------------------------------------------------------------
callback :: forall a. TMVar (Either SomeException a)
         -> (Either SomeException a -> IO ())
         -> Callback a
callback mvar k = Callback go
  where
    go :: forall b. Stage a b -> IO b
    go (Fulfill a) = do
      atomically $
        unlessM (tryPutTMVar mvar (Right a)) $ do
          _ <- swapTMVar mvar (Right a)
          return ()
      k (Right a)
    go (Reject e) = do
      let err = Left $ toException e
      atomically $ do
        unlessM (tryPutTMVar mvar err) $ do
          _ <- swapTMVar mvar err

          return ()
      k err
    go Retrieve = atomically $ readTMVar mvar