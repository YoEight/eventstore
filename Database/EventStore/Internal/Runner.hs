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

--------------------------------------------------------------------------------
data Report a
  = Value a
  | OperationFailed OperationError

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
operationRunnerSM :: Monad m => SM m
operationRunnerSM = SM go HashMap.empty
  where
    go s (Submit op cb) = runOperation s (Session op cb)

--------------------------------------------------------------------------------
runOperation :: Monad m => Sessions m -> Session m -> m (Sessions m)
runOperation seed (Session initialState report) =
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
            Stop -> pure cur

            Yield a next ->
              report (Value a) *> go cur next


  in go seed initialState
