{-# LANGUAGE StrictData #-}
--------------------------------------------------------------------------------
-- |
-- Module    :  Database.EventStore.Internal.Runner
-- Copyright :  (C) 2019 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Runner where

--------------------------------------------------------------------------------
import qualified Data.HashMap.Strict as HashMap
import           Data.Void (absurd)
--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Report a
  = Value a
  | OperationFailed OperationError

--------------------------------------------------------------------------------
data Runner m =
  Runner
  { runnerNewId :: m UUID
  , runnerSendPkg :: Package -> m ()
  }

--------------------------------------------------------------------------------
data Request m where
  Submit :: Operation a -> (Report a -> m ()) -> Request m

--------------------------------------------------------------------------------
data SM m = forall x. SM (x -> Request m -> m x) x

--------------------------------------------------------------------------------
data Session m =
  forall a.
  Session
  { sessionState :: Operation a
  , sessionReport :: Report a -> m ()
  }

--------------------------------------------------------------------------------
type Sessions m = HashMap.HashMap UUID (Session m)

--------------------------------------------------------------------------------
operationRunnerSM :: Monad m => Runner m -> SM m
operationRunnerSM eff = SM go HashMap.empty
  where
    go s (Submit op cb) = runOperation eff s (Session op cb)

data Poo = Poo Int

--------------------------------------------------------------------------------
runOperation :: Monad m => Runner m -> Sessions m -> Session m -> m (Sessions m)
runOperation eff seed (Session initialState report) =
  let go cur = \case
        Return x ->
          absurd x

        Effect action ->
          case action of
            Failed e ->
              cur <$ report (OperationFailed e)

            Retry ->
              go cur initialState

            Proceed inner ->
              go cur inner

        Step step ->
          case step of
            Stop ->
              pure cur

            Yield a next ->
              report (Value a) *> go cur next

            Await k tpe _ ->
              case tpe of
                NeedUUID ->
                  go cur . k =<< runnerNewId eff

                NeedRemote p -> do
                  pkgId <- runnerNewId eff

                  let pkg =
                        Package
                        { packageCmd = payloadCmd p
                        , packageCorrelation = pkgId
                        , packageData = payloadData p
                        , packageCred = payloadCreds p
                        }

                  runnerSendPkg eff pkg
                  return cur


  in go seed initialState
