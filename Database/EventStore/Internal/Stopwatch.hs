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
import ClassyPrelude
import Data.Time

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
newStopwatch :: IO Stopwatch
newStopwatch = fmap Stopwatch . newMVar . initInternal =<< getCurrentTime

--------------------------------------------------------------------------------
stopwatchElapsed :: Stopwatch -> IO NominalDiffTime
stopwatchElapsed (Stopwatch var) =
  modifyMVar var $ \prev -> do
    now <- getCurrentTime
    let next = update now prev
    return (next, _acc next)