{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Promise
-- Copyright : (C) 2017 Yorick Laupa
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
  , reject
  , retrieve
  , tryRetrieve
  ) where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
data Promise a =
  Promise { fulfill     :: a -> IO ()
          , reject      :: forall e. Exception e => e -> IO ()
          , tryRetrieve :: IO (Either SomeException a)
          }

--------------------------------------------------------------------------------
newPromise :: IO (Promise a)
newPromise = do
  mvar <- newEmptyTMVarIO

  let p = Promise { fulfill     = _fulfill mvar
                  , reject      = _reject mvar
                  , tryRetrieve = _tryRetrieve mvar
                  }

  return p

--------------------------------------------------------------------------------
_fulfill :: TMVar (Either SomeException a) -> a -> IO ()
_fulfill mvar a = atomically $
  whenM (isEmptyTMVar mvar) $
    putTMVar mvar (Right a)

--------------------------------------------------------------------------------
_reject :: Exception e => TMVar (Either SomeException a) -> e -> IO ()
_reject mvar e = atomically $
  whenM (isEmptyTMVar mvar) $
    putTMVar mvar (Left $ toException e)

--------------------------------------------------------------------------------
_tryRetrieve :: TMVar (Either SomeException a) -> IO (Either SomeException a)
_tryRetrieve mvar = atomically $ readTMVar mvar

--------------------------------------------------------------------------------
retrieve :: Promise a -> IO a
retrieve p = do
  outcome <- tryRetrieve p
  case outcome of
    Left e  -> throwIO e
    Right a -> return a