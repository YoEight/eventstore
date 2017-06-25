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
  , fulfill
  , reject
  , retrieve
  , tryRetrieve
  , fromEither
  ) where

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Prelude

--------------------------------------------------------------------------------
newtype Callback a =
  Callback { runCallback :: forall b. Stage a b -> IO b }

--------------------------------------------------------------------------------
data Stage a b where
  Fulfill  :: a -> Stage a ()
  Reject   :: Exception e => e -> Stage a ()
  Retrieve :: Stage a (Either SomeException a)

--------------------------------------------------------------------------------
fulfill :: MonadIO m => Callback a -> a -> m ()
fulfill cb a = liftIO $ runCallback cb (Fulfill a)

--------------------------------------------------------------------------------
reject :: (Exception e, MonadIO m) => Callback a -> e -> m ()
reject cb e = liftIO $ runCallback cb (Reject e)

--------------------------------------------------------------------------------
tryRetrieve :: Callback a -> IO (Either SomeException a)
tryRetrieve cb = runCallback cb Retrieve

--------------------------------------------------------------------------------
retrieve :: Callback a -> IO a
retrieve p = do
  tryRetrieve p >>= \case
    Left e  -> throw e
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