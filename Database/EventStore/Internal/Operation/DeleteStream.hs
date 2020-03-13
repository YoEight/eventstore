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
import Database.EventStore.Internal.Communication (Transmit(..))
import Database.EventStore.Internal.Control (publishWith)
import Database.EventStore.Internal.Exec
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.DeleteStream.Message
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Settings
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Returned after deleting a stream. 'Position' of the write.
newtype DeleteResult = DeleteResult Position deriving (Eq, Show)

-------------------------------------------------------------------------------
deleteStream
  :: Settings
  -> Exec
  -> Text
  -> ExpectedVersion
  -> Maybe Bool
  -> Maybe Credentials
  -> IO (Async DeleteResult)
deleteStream setts exec stream version hard creds
  = do m <- mailboxNew
       async $
         do let req = newRequest stream (expVersionInt64 version) (s_requireMaster setts) hard

            pkg <- createPkg deleteStreamCmd creds req

            keepLooping $ do
              publishWith exec (Transmit m OneTime pkg)
              outcome <- mailboxReadDecoded m
              case outcome of
                Left e
                  -> throw e
                Right resp
                  -> let r = getField $ _result resp
                         com_pos = getField $ _commitPosition resp
                         prep_pos = getField $ _preparePosition resp
                         com_pos_int = fromMaybe (-1) com_pos
                         prep_pos_int = fromMaybe (-1) prep_pos
                         pos = Position com_pos_int prep_pos_int
                         res = DeleteResult pos in
                     case r of
                       OP_SUCCESS -> pure $ Break res
                       OP_PREPARE_TIMEOUT -> pure Loop
                       OP_FORWARD_TIMEOUT -> pure Loop
                       OP_COMMIT_TIMEOUT -> pure Loop
                       OP_WRONG_EXPECTED_VERSION -> throw $ WrongExpectedVersion stream version
                       OP_STREAM_DELETED -> throw $ StreamDeleted $ StreamName stream
                       OP_INVALID_TRANSACTION -> throw InvalidTransaction
                       OP_ACCESS_DENIED -> throw $ AccessDenied (StreamName stream)


