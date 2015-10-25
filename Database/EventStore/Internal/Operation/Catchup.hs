{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.Catchup
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.Catchup
    ( CatchupType(..)
    , catchup
    ) where

--------------------------------------------------------------------------------
import Control.Monad
import Data.Int
import Data.Maybe

--------------------------------------------------------------------------------
import Data.Text (Text)

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Read.Common
import Database.EventStore.Internal.Operation.ReadAllEvents
import Database.EventStore.Internal.Operation.ReadStreamEvents
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
defaultBatchSize :: Int32
defaultBatchSize = 500

--------------------------------------------------------------------------------
streamNotFound :: OperationError
streamNotFound = InvalidOperation "Catchup. inexistant stream"

--------------------------------------------------------------------------------
data CatchupType
    = RegularCatchup Text Int32
    | AllCatchup Int64 Int64

--------------------------------------------------------------------------------
catchup :: Settings
        -> CatchupType
        -> Bool
        -> Maybe Int32
        -> Operation ([ResolvedEvent], Bool)
catchup setts init_tpe tos bat_siz = go init_tpe
  where
    batch = fromMaybe defaultBatchSize bat_siz
    go tpe = do
        let action =
                case tpe of
                    RegularCatchup stream cur_evt ->
                        let op = readStreamEvents setts Forward stream batch
                                 cur_evt tos in
                        mapOp Left op
                    AllCatchup c_pos p_pos ->
                        let op = readAllEvents setts c_pos p_pos batch
                                 tos Forward in
                        mapOp Right op

        foreach action $ \res -> do
            (eos, evts, nxt_tpe) <- case res of
                Right as -> do
                    let Position nxt_c nxt_p = sliceNext as
                        tmp_tpe = AllCatchup nxt_c nxt_p
                    return (sliceEOS as, sliceEvents as, tmp_tpe)
                Left rr -> fromReadResult rr $ \as ->
                    let RegularCatchup s _ = tpe
                        nxt = sliceNext as
                        tmp_tpe = RegularCatchup s nxt in
                    return (sliceEOS as, sliceEvents as, tmp_tpe)

            yield (evts, eos)
            when (not eos) $ go nxt_tpe

--------------------------------------------------------------------------------
fromReadResult :: ReadResult 'RegularStream a -> (a -> SM b x) -> SM b x
fromReadResult res k =
    case res of
        ReadNoStream        -> failure streamNotFound
        ReadStreamDeleted s -> failure $ StreamDeleted s
        ReadNotModified     -> failure $ ServerError Nothing
        ReadError e         -> failure $ ServerError e
        ReadAccessDenied s  -> failure $ AccessDenied s
        ReadSuccess ss      -> k ss
