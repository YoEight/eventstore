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
    ( CatchupState(..)
    , CatchupOpResult(..)
    , Checkpoint(..)
    , catchup
    , catchupStreamName
    ) where

--------------------------------------------------------------------------------
import Data.Int
import Data.Maybe

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Read.Common
import Database.EventStore.Internal.Operation.ReadAllEvents
import Database.EventStore.Internal.Operation.ReadStreamEvents
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Represents the next checkpoint to reach on a catchup subscription. Wheither
--   it's a regular stream or the $all stream, it either point to an 'Int32' or
--   a 'Position'.
data Checkpoint = CheckpointNumber Int32 | CheckpointPosition Position

--------------------------------------------------------------------------------
defaultBatchSize :: Int32
defaultBatchSize = 500

--------------------------------------------------------------------------------
streamNotFound :: Text -> OperationError
streamNotFound stream =
  InvalidOperation $ "Catchup. inexistant stream [" <> stream <> "]"

--------------------------------------------------------------------------------
-- | Catchup operation state.
data CatchupState
    = RegularCatchup Text Int32
      -- ^ Indicates the stream name and the next event number to start from.
    | AllCatchup Int64 Int64
      -- ^ Indicates the commit and prepare position. Used when catching up from
      --   the $all stream.

--------------------------------------------------------------------------------
catchupStreamName :: CatchupState -> Text
catchupStreamName (RegularCatchup stream _) = stream
catchupStreamName _ = "$all"

--------------------------------------------------------------------------------
data CatchupOpResult =
    CatchupOpResult { catchupReadEvents :: ![ResolvedEvent]
                    , catchupEndOfStream :: !Bool
                    , catchupCheckpoint :: !Checkpoint
                    }

--------------------------------------------------------------------------------
-- | Stream catching up operation.
catchup :: Settings
        -> CatchupState
        -> Bool
        -> Maybe Int32
        -> Operation CatchupOpResult -- ([ResolvedEvent], Bool, Checkpoint)
catchup setts init_tpe tos bat_siz = go init_tpe
  where
    batch = fromMaybe defaultBatchSize bat_siz
    go tpe = do
        let action =
                case tpe of
                    RegularCatchup stream cur_evt ->
                        let op = readStreamEvents setts Forward stream cur_evt
                                 batch tos in
                        mapOp Left op
                    AllCatchup c_pos p_pos ->
                        let op = readAllEvents setts c_pos p_pos batch
                                 tos Forward in
                        mapOp Right op

        foreach action $ \res -> do
            (eos, evts, nchk, nxt_tpe) <- case res of
                Right as -> do
                    let Position nxt_c nxt_p = sliceNext as
                        tmp_tpe = AllCatchup nxt_c nxt_p
                        chk     = CheckpointPosition $ sliceNext as
                    return (sliceEOS as, sliceEvents as, chk, tmp_tpe)
                Left rr -> fromReadResult (catchupStreamName tpe) rr $ \as ->
                    let RegularCatchup s _ = tpe
                        nxt = sliceNext as
                        tmp_tpe = RegularCatchup s nxt
                        chk = CheckpointNumber nxt in
                    return (sliceEOS as, sliceEvents as, chk, tmp_tpe)

            yield (CatchupOpResult evts eos nchk)
            unless eos $ go nxt_tpe

--------------------------------------------------------------------------------
fromReadResult :: Text
               -> ReadResult 'RegularStream a
               -> (a -> SM b x)
               -> SM b x
fromReadResult stream res k =
    case res of
        ReadNoStream        -> failure $ streamNotFound stream
        ReadStreamDeleted s -> failure $ StreamDeleted s
        ReadNotModified     -> failure $ ServerError Nothing
        ReadError e         -> failure $ ServerError e
        ReadAccessDenied s  -> failure $ AccessDenied s
        ReadSuccess ss      -> k ss
