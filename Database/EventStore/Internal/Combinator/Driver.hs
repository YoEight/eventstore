--------------------------------------------------------------------------------
-- |
-- Module    :  Database.EventStore.Internal.Combinator.Driver
-- Copyright :  (C) 2019 Vente Privée
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <ylaupa@vente-privee.com>
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Combinator.Driver where

--------------------------------------------------------------------------------
import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Control
import Database.EventStore.Internal.Connection
import Database.EventStore.Internal.Driver
import Database.EventStore.Internal.Stopwatch
import Database.EventStore.Internal.Prelude

--------------------------------------------------------------------------------
data DriverRef =
  DriverRef
  { refStopwatch :: Stopwatch }

--------------------------------------------------------------------------------
createDriverRef :: MonadIO m => Settings -> m (Driver m)
createDriverRef setts = do
  builder <- liftIO $ connectionBuilder setts
  undefined
