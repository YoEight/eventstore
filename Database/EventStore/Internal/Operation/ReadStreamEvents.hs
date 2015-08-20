{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.ReadStreamEvents
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.ReadStreamEvents
    ( readStreamEvents ) where

--------------------------------------------------------------------------------
import Data.Maybe

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Serialize
import Data.Text

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Read.Common
import Database.EventStore.Internal.Operation.ReadStreamEvents.Message
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
readStreamEvents :: Settings
                 -> ReadDirection
                 -> Text
                 -> Int32
                 -> Int32
                 -> Bool
                 -> Operation 'Init (ReadResult 'RegularStream
                                                (Slice 'RegularStream))
readStreamEvents Settings{..} dir s st cnt tos = Operation create
  where
    req_cmd =
        case dir of
            Forward  -> 0xB2
            Backward -> 0xB4

    resp_cmd =
        case dir of
            Forward  -> 0xB3
            Backward -> 0xB5

    create :: forall a. Input 'Init (ReadResult 'RegularStream
                                                (Slice 'RegularStream))
    create (Create uuid) =
        let msg = newRequest s st cnt tos s_requireMaster
            pkg = Package
                  { packageCmd = req_cmd
                  , packageCorrelation = uuid
                  , packageData        = runPut $ encodeMessage msg
                  , packageCred        = s_credentials
                  } in
            (pkg, Package pending)

    pending :: forall a. Input 'Pending (ReadResult 'RegularStream
                                                    (Slice 'RegularStream))
    pending (Arrived Package{..})
        | packageCmd  == resp_cmd =
            Right $ decodeResp packageData $ \resp ->
                let r     = getField $ _result resp
                    es    = getField $ _events resp
                    evts  = fmap newResolvedEvent eos
                    err   = getField $ _error resp
                    eos   = getField $ _endOfStream resp
                    nxt   = getField $ _nextNumber resp
                    lst   = getField $ _lastNumber resp
                    c_pos = getField $ _lastCommitPosition resp
                    found = Slice s st nxt lst eos evts dir in
                case r of
                    NO_STREAM       -> success ReadNoStream
                    STREAM_DELETED	-> success ReadStreamDeleted
                    NOT_MODIFIED    -> success ReadNotModified
                    ERROR           -> success (ReadError err)
                    ACCESS_DENIED   -> success ReadAccessDenied
                    SUCCESS         -> success (ReadSuccess found)
        | otherwise = Left $ Operation pending