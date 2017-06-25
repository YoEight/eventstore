--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Stopwatch
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Stopwatch
  ( Stopwatch
  , newStopwatch
  , stopwatchElapsed
  ) where

--------------------------------------------------------------------------------
import Data.Time

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Prelude

--------------------------------------------------------------------------------
data Internal =
  Internal { _lastTime :: !UTCTime
           , _acc      :: !NominalDiffTime
           }

--------------------------------------------------------------------------------
initInternal :: UTCTime -> Internal
initInternal now = Internal now 0

--------------------------------------------------------------------------------
update :: UTCTime -> Internal -> Internal
update now (Internal before acc) = Internal now acc'
  where
    acc' = acc + diffUTCTime now before

--------------------------------------------------------------------------------
newtype Stopwatch = Stopwatch (MVar Internal)

--------------------------------------------------------------------------------
newStopwatch :: MonadBase IO m => m Stopwatch
newStopwatch =
  fmap Stopwatch . newMVar . initInternal =<< liftBase getCurrentTime

--------------------------------------------------------------------------------
stopwatchElapsed :: MonadBaseControl IO m => Stopwatch -> m NominalDiffTime
stopwatchElapsed (Stopwatch var) =
  modifyMVar var $ \prev -> do
    now <- liftBase getCurrentTime
    let next = update now prev
    return (next, _acc next)