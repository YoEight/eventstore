{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.DeleteStream
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.DeleteStream
    ( DeleteResult(..)
    , deleteStream
    ) where

--------------------------------------------------------------------------------
import Data.Maybe

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Text

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.DeleteStream.Message
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Returned after deleting a stream. 'Position' of the write.
newtype DeleteResult = DeleteResult Position deriving (Eq, Show)

--------------------------------------------------------------------------------
deleteStream :: Settings
             -> Text
             -> ExpectedVersion
             -> Maybe Bool
             -> Operation DeleteResult
deleteStream Settings{..} s v hard = do
    let msg = newRequest s (expVersionInt32 v) s_requireMaster hard
    resp <- send 0x8A 0x8B msg
    let r            = getField $ _result resp
        com_pos      = getField $ _commitPosition resp
        prep_pos     = getField $ _preparePosition resp
        com_pos_int  = fromMaybe (-1) com_pos
        prep_pos_int = fromMaybe (-1) prep_pos
        pos          = Position com_pos_int prep_pos_int
        res          = DeleteResult pos
    case r of
        OP_SUCCESS                -> yield res
        OP_PREPARE_TIMEOUT        -> retry
        OP_FORWARD_TIMEOUT        -> retry
        OP_COMMIT_TIMEOUT         -> retry
        OP_WRONG_EXPECTED_VERSION -> wrongVersion s v
        OP_STREAM_DELETED         -> streamDeleted s
        OP_INVALID_TRANSACTION    -> invalidTransaction
        OP_ACCESS_DENIED          -> accessDenied (StreamName s)
