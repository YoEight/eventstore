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
import qualified Data.IORef as IORef

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
newtype Stopwatch = Stopwatch (IORef.IORef Internal)

--------------------------------------------------------------------------------
newStopwatch :: MonadBase IO m => m Stopwatch
newStopwatch = liftBase $
  fmap Stopwatch . IORef.newIORef . initInternal =<< getCurrentTime

--------------------------------------------------------------------------------
stopwatchElapsed :: MonadBase IO m => Stopwatch -> m NominalDiffTime
stopwatchElapsed (Stopwatch ref) = liftBase $ do
  now <- getCurrentTime
  IORef.atomicModifyIORef ref $ \prev ->
    let next = update now prev in (next, _acc next)
