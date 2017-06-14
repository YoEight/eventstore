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
makeOp :: Settings
       -> Int32
       -> Bool
       -> CatchupState
       -> Operation (Either (ReadResult 'RegularStream StreamSlice) AllSlice)
makeOp setts batch tos tpe =
    case tpe of
        RegularCatchup stream fromEvt ->
            fmap Left
                (readStreamEvents setts Forward stream fromEvt batch tos)
        AllCatchup comPos prePos ->
            fmap Right
                (readAllEvents setts comPos prePos batch tos Forward)

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
streaming :: Settings
          -> Int32
          -> Bool
          -> CatchupState
          -> Operation CatchupOpResult
streaming setts batch tos = construct . go
  where
    go state = do
        outcome <- deconstruct (fmap Left (makeOp setts batch tos state))
        (eos, evts, nchk, nextState) <- extract state outcome
        yield (CatchupOpResult evts eos nchk)
        unless eos $ go nextState

    extract _ (Right as) = do
        let Position nxt_c nxt_p = sliceNext as
            tmp_tpe = AllCatchup nxt_c nxt_p
            chk     = CheckpointPosition $ sliceNext as
        return (sliceEOS as, sliceEvents as, chk, tmp_tpe)
    extract state (Left rr) =
        fromReadResult (catchupStreamName state) rr $ \as -> do
            let RegularCatchup s _ = state
                nxt = sliceNext as
                tmp_tpe = RegularCatchup s nxt
                chk = CheckpointNumber nxt
            return (sliceEOS as, sliceEvents as, chk, tmp_tpe)

--------------------------------------------------------------------------------
-- | Stream catching up operation.
catchup :: Settings
        -> CatchupState
        -> Bool
        -> Maybe Int32
        -> Operation CatchupOpResult
catchup setts initState tos bat_siz =
    streaming setts batch tos initState
  where
    batch = fromMaybe defaultBatchSize bat_siz

--------------------------------------------------------------------------------
fromReadResult :: Text
               -> ReadResult 'RegularStream a
               -> (a -> Code o x)
               -> Code o x
fromReadResult stream res k =
    case res of
        ReadNoStream        -> failure $ streamNotFound stream
        ReadStreamDeleted s -> failure $ StreamDeleted s
        ReadNotModified     -> failure $ ServerError Nothing
        ReadError e         -> failure $ ServerError e
        ReadAccessDenied s  -> failure $ AccessDenied s
        ReadSuccess ss      -> k ss
