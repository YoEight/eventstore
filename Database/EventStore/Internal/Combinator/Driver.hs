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
import Prelude

--------------------------------------------------------------------------------
import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import Polysemy
import Polysemy.Internal.Combinators

--------------------------------------------------------------------------------
import Database.EventStore.Internal.ConnectionNew
import Database.EventStore.Internal.Effect.Driver
import Database.EventStore.Internal.Stopwatch

--------------------------------------------------------------------------------
data DriverRef =
  DriverRef
  { refStopwatch :: Stopwatch }

--------------------------------------------------------------------------------
runDriver :: Member (Embed IO) r
          => DriverRef
          -> Sem (Driver ': r) a
          -> Sem r a
runDriver ref = interpret $ \case
  GenerateId -> embed nextRandom

  GetElapsedTime -> embed (stopwatchElapsed $ refStopwatch ref)

  Connect edp -> undefined
