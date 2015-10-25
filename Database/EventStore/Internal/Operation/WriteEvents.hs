{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.WriteEvents
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.WriteEvents
    ( writeEvents ) where

--------------------------------------------------------------------------------
import Data.Maybe
import Data.Traversable

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Text

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Write.Common
import Database.EventStore.Internal.Operation.WriteEvents.Message
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
import Prelude

--------------------------------------------------------------------------------
writeEvents :: Settings
            -> Text
            -> ExpectedVersion
            -> [Event]
            -> Operation WriteResult
writeEvents Settings{..} s v evts = do
    nevts <- traverse eventToNewEvent evts
    let msg = newRequest s (expVersionInt32 v) nevts s_requireMaster
    resp <- send 0x82 0x83 msg
    let r            = getField $ _result resp
        com_pos      = getField $ _commitPosition resp
        prep_pos     = getField $ _preparePosition resp
        lst_num      = getField $ _lastNumber resp
        com_pos_int  = fromMaybe (-1) com_pos
        prep_pos_int = fromMaybe (-1) prep_pos
        pos          = Position com_pos_int prep_pos_int
        res          = WriteResult lst_num pos
    case r of
        OP_SUCCESS                -> yield res
        OP_PREPARE_TIMEOUT        -> retry
        OP_FORWARD_TIMEOUT        -> retry
        OP_COMMIT_TIMEOUT         -> retry
        OP_WRONG_EXPECTED_VERSION -> wrongVersion s v
        OP_STREAM_DELETED         -> streamDeleted s
        OP_INVALID_TRANSACTION    -> invalidTransaction
        OP_ACCESS_DENIED          -> accessDenied (StreamName s)
