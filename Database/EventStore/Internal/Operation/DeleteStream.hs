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
import Data.Serialize
import Data.Text

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.DeleteStream.Message
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Returned after deleting a stream. 'Position' of the write.
newtype DeleteResult = DeleteResult Position deriving (Eq, Show)

--------------------------------------------------------------------------------
deleteStream :: Settings
             -> Text
             -> ExpectedVersion
             -> Maybe Bool
             -> Operation 'Init DeleteResult
deleteStream Settings{..} s v hard = Operation create
  where
    create :: forall a. Input 'Init DeleteResult a -> a
    create (Create uuid) =
        let msg = newRequest s (expVersionInt32 v) s_requireMaster hard
            pkg = Package
                  { packageCmd         = 0x8A
                  , packageCorrelation = uuid
                  , packageData        = runPut $ encodeMessage msg
                  , packageCred        = s_credentials
                  } in
        (pkg, Operation pending)

    pending :: forall a. Input 'Pending DeleteResult a -> a
    pending (Arrived Package{..})
        | packageCmd == 0x8B =
            Right $ decodeResp packageData $ \resp ->
                let r            = getField $ _result resp
                    com_pos      = getField $ _commitPosition resp
                    prep_pos     = getField $ _preparePosition resp
                    com_pos_int  = fromMaybe (-1) com_pos
                    prep_pos_int = fromMaybe (-1) prep_pos
                    pos          = Position com_pos_int prep_pos_int
                    res          = DeleteResult pos in
                case r of
                    OP_SUCCESS                -> success res
                    OP_PREPARE_TIMEOUT        -> retry create
                    OP_FORWARD_TIMEOUT        -> retry create
                    OP_COMMIT_TIMEOUT         -> retry create
                    OP_WRONG_EXPECTED_VERSION -> wrongVersion s v
                    OP_STREAM_DELETED         -> streamDeleted s
                    OP_INVALID_TRANSACTION    -> invalidTransaction
                    OP_ACCESS_DENIED          -> accessDenied (StreamName s)
        | otherwise = Left $ Operation pending