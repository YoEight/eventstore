--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Promise
-- Copyright : (C) 2016 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Promise
    ( Promise
    , newPromise
    , fulfill
    , fulfill_
    , failed
    , retrieve
    , retrieveThrow
    ) where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
data State a
    = Unresolved
    | Resolved a
    | Errored SomeException

--------------------------------------------------------------------------------
-- | Simple datastructure allowing to await for an asynchronous action.
newtype Promise a = Promise (TVar (State a))

--------------------------------------------------------------------------------
retrieveSTM :: Promise a -> STM (Either SomeException a)
retrieveSTM (Promise var) = do
    s <- readTVar var
    case s of
        Unresolved -> retrySTM
        Resolved a -> return $ Right a
        Errored e -> return $ Left e

--------------------------------------------------------------------------------
-- | Creates a new 'Promise'.
newPromise :: IO (Promise a)
newPromise = fmap Promise $ newTVarIO Unresolved

--------------------------------------------------------------------------------
-- | Reports a result for a 'Promise'. You can only 'fulfill' one time. Meaning,
--   if a 'Promise' has already been 'fulfill'ed or 'failed', 'fulfill' would be
--   a no-op.
fulfill :: Promise a -> a -> IO ()
fulfill (Promise var) a = atomically $ do
    s <- readTVar var
    case s of
        Unresolved -> writeTVar var (Resolved a)
        _ -> return ()

--------------------------------------------------------------------------------
-- | Like 'fulfill' but specialized to unit.
fulfill_ :: Promise () -> IO ()
fulfill_ p = fulfill p ()

--------------------------------------------------------------------------------
-- | Reports an exception to a 'Promise'. You can only fail a 'Promise' one
--   time. Meaning, if a 'Promise' has already been 'fulfill'ed or 'failed',
--   'failed' would be a no-op.
failed :: Exception e => Promise a -> e -> IO ()
failed (Promise var) e = atomically $ do
    s <- readTVar var
    case s of
        Unresolved -> writeTVar var (Errored $ SomeException e)
        _ -> return ()

--------------------------------------------------------------------------------
-- | Gets either a 'Promise' produced a value or an exception.
retrieve :: Promise a -> IO (Either SomeException a)
retrieve = atomically . retrieveSTM

--------------------------------------------------------------------------------
-- | Like 'retrieve' except if a 'Promise' produced an exception, it would be
-- thrown through 'throwIO'.
retrieveThrow :: Promise a -> IO a
retrieveThrow p = either throwIO return =<< retrieve p