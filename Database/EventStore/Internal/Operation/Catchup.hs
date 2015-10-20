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
module Database.EventStore.Internal.Operation.Catchup (catchup) where

--------------------------------------------------------------------------------
import Control.Monad
import Data.Int
import Data.Maybe

--------------------------------------------------------------------------------
import Data.Text (Text)

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Read.Common
import Database.EventStore.Internal.Operation.ReadStreamEvents
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
defaultBatchSize :: Int32
defaultBatchSize = 500

--------------------------------------------------------------------------------
streamNotFound :: OperationError
streamNotFound = InvalidOperation "Catchup. inexistant stream"

--------------------------------------------------------------------------------
catchup :: Settings
        -> Text
        -> Bool
        -> Maybe Int32
        -> Maybe Int32
        -> Operation ([ResolvedEvent], Bool)
catchup setts stream tos lst_chk bat_siz =
    let nxt_evt = fromMaybe 0 lst_chk in go nxt_evt
  where
    batch = fromMaybe defaultBatchSize bat_siz
    go cur_evt = do
        let action =
                readStreamEvents setts Forward stream batch cur_evt tos
        foreach action $ \res ->
            case res of
                ReadNoStream        -> failure streamNotFound
                ReadStreamDeleted s -> failure $ StreamDeleted s
                ReadNotModified     -> failure $ ServerError Nothing
                ReadError e         -> failure $ ServerError e
                ReadAccessDenied s  -> failure $ AccessDenied s
                ReadSuccess ss -> do
                    yield (sliceEvents ss, sliceEOS ss)
                    when (not $ sliceEOS ss) $ go $ sliceNext ss
