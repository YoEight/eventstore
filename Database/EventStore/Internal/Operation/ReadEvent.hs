{-# LANGUAGE DataKinds       #-}
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
module Database.EventStore.Internal.Operation.ReadEvent
    ( ReadEvent(..)
    , readEventSM
    , readEvent
    ) where

--------------------------------------------------------------------------------
import Data.Int

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Serialize
import Data.Text
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.ReadEvent.Message
import Database.EventStore.Internal.Operation.Read.Common
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data ReadEvent
    = ReadEventNotFound
      { readEventStream :: !Text
      , readEventNumber :: !Int32
      }
    | ReadEvent
      { readEventStream   :: !Text
      , readEventNumber   :: !Int32
      , readEventResolved :: !ResolvedEvent
      } deriving Show

--------------------------------------------------------------------------------
readEventSM :: Settings
            -> Text
            -> Int32
            -> Bool
            -> UUID
            -> SM (ReadResult 'RegularStream ReadEvent) ()
readEventSM Settings{..} s evtn tos uuid = do
    let msg = newRequest s evtn tos s_requireMaster
        pkg = Package
              { packageCmd         = 0xB0
              , packageCorrelation = uuid
              , packageData        = runPut $ encodeMessage msg
              , packageCred        = s_credentials
              }
    Package{..} <- send pkg
    if packageCmd == exp_cmd
        then do
            resp <- decodeResp packageData
            let r   = getField $ _result resp
                evt = newResolvedEvent $ getField $ _indexedEvent resp
                err = getField $ _error resp
                not_found = ReadSuccess $ ReadEventNotFound s evtn
                found     = ReadSuccess $ ReadEvent s evtn evt
            case r of
                NOT_FOUND      -> yield not_found
                NO_STREAM      -> yield ReadNoStream
                STREAM_DELETED -> yield $ ReadStreamDeleted s
                ERROR          -> yield (ReadError err)
                ACCESS_DENIED  -> yield $ ReadAccessDenied $ StreamName s
                SUCCESS        -> yield found
        else invalidServerResponse exp_cmd packageCmd
  where
    exp_cmd = 0xB1

--------------------------------------------------------------------------------
readEvent :: Settings
          -> Text
          -> Int32
          -> Bool
          -> Operation (ReadResult 'RegularStream ReadEvent)
readEvent setts s evtn tos =
    operation $ readEventSM setts s evtn tos
