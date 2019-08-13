{-# LANGUAGE StrictData #-}
--------------------------------------------------------------------------------
-- |
-- Module    :  Database.EventStore.Internal.Run.Driver
-- Copyright :  (C) 2019 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Run.Driver where

--------------------------------------------------------------------------------
import           Prelude
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.UUID (UUID)
import           Polysemy
import           Polysemy.Internal.Combinators (stateful)
import           Polysemy.Input

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Effect.Driver
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Exchange =
  Exchange
  { exchangeCount :: Int
  , exchangeRequest :: Package
  }

-------------------------------------------------------------------------------
data Reg
  = Active ConnectionId (HashMap UUID Exchange)
  | Awaiting [Package]

--------------------------------------------------------------------------------
data DriverState =
  DriverState
  { driverStateStage :: Stage
  , driverStateReg :: Reg
  }

--------------------------------------------------------------------------------
initial :: DriverState
initial =
  DriverState
  { driverStateStage = undefined
  , driverStateReg = Awaiting []
  }

--------------------------------------------------------------------------------
runDriver :: forall r a. Members '[Input UUID] r
          => Sem (Driver ': r) a
          -> Sem r a
runDriver = fmap snd . stateful go initial
  where
    go :: forall m x. Members '[Input UUID] r
       => Driver m x
       -> DriverState
       -> Sem r (DriverState, x)
    go (Connect _) cur = do
      uuid <- input
      pure (cur, ConnectionId uuid)

    go GenerateId cur = do
      uuid <- input
      pure (cur, uuid)

    go GetStage cur =
      pure (cur, driverStateStage cur)

    go (SetStage s) cur =
      pure (cur { driverStateStage = s }, ())

    go (IsMapped pkgId) cur =
      case driverStateReg cur of
        Awaiting _ ->
          pure (cur, False)

        Active _ reg ->
          pure (cur, HashMap.member pkgId reg)

    go (Register pkg) cur =
      case driverStateReg cur of
        Awaiting xs ->
          let next =
                cur { driverStateReg = Awaiting (pkg:xs) } in

          pure (next, ())

        Active cid reg ->
          let newExc =
                Exchange
                { exchangeCount = 0
                , exchangeRequest = pkg
                }

              nextReg = HashMap.insert (packageCorrelation pkg) newExc reg
              next = cur { driverStateReg = Active cid nextReg } in

          pure (next, ())
