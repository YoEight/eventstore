--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Execution.TCQueue
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Execution.TCQueue
    ( TCQueue
    , newTCQueue
    , readTCQueue
    , writeTCQueue
    , clearTCQueue
    , isEmptyTCQueue
    , updateTCQueue
    , unsafeUpdateTCQueue
    ) where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
-- | Used to determine if we hit the end of the queue.
data Slot a = Slot !a | End

--------------------------------------------------------------------------------
-- | A 'TQueue' that can be cycled.
newtype TCQueue a = TCQueue (TQueue (Slot a))

--------------------------------------------------------------------------------
-- | Creates an empty 'TCQueue'.
newTCQueue :: IO (TCQueue a)
newTCQueue = fmap TCQueue newTQueueIO

--------------------------------------------------------------------------------
-- | Gets an element from the 'TCQueue'.
readTCQueue :: TCQueue a -> STM a
readTCQueue (TCQueue q) = do
    Slot a <- readTQueue q
    return a

--------------------------------------------------------------------------------
-- | Writes an element to the 'TCQueue'.
writeTCQueue :: TCQueue a -> a -> STM ()
writeTCQueue (TCQueue q) a = writeTQueue q (Slot a)

--------------------------------------------------------------------------------
-- | Empties a 'TCQueue'.
clearTCQueue :: TCQueue a -> STM ()
clearTCQueue (TCQueue q) = writeTQueue q End >> go
  where
    go = do
        s <- readTQueue q
        case s of
            End -> return ()
            _   -> go

--------------------------------------------------------------------------------
-- | Updates a 'TCQueue'.
updateTCQueue :: TCQueue a -> (a -> STM (Maybe a)) -> STM ()
updateTCQueue (TCQueue q) k = writeTQueue q End >> go
  where
    go = do
        s <- readTQueue q
        case s of
            End    -> return ()
            Slot a -> do
                r <- k a
                case r of
                    Nothing -> go
                    Just a' -> writeTQueue q (Slot a') >> go

--------------------------------------------------------------------------------
-- | Like 'updateTCQueue' execpt everything happens in the 'IO' monad. It's no
--   longer atomic.
unsafeUpdateTCQueue :: TCQueue a -> (a -> IO (Maybe a)) -> IO ()
unsafeUpdateTCQueue (TCQueue q) k = atomically (writeTQueue q End) >> go
  where
    go = do
        s <- atomically $ readTQueue q
        case s of
            End    -> return ()
            Slot a -> do
                r <- k a
                case r of
                    Nothing -> go
                    Just a' -> atomically (writeTQueue q (Slot a')) >> go

--------------------------------------------------------------------------------
-- | Indicates if a 'TCQueue' is empty.
isEmptyTCQueue :: TCQueue a -> STM Bool
isEmptyTCQueue (TCQueue q) = isEmptyTQueue q
