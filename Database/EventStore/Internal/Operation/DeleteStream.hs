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

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.DeleteStream.Message
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Settings
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Returned after deleting a stream. 'Position' of the write.
newtype DeleteResult = DeleteResult Position deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Delete a regular stream operation.
deleteStream :: Settings
             -> Text
             -> ExpectedVersion
             -> Maybe Bool
             -> Maybe Credentials
             -> Operation DeleteResult
deleteStream Settings{..} s v hard cred = construct $ do
    let msg = newRequest s (expVersionInt64 v) s_requireMaster hard
    resp <- send deleteStreamCmd deleteStreamCompletedCmd cred msg
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
