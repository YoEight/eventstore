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
module Database.EventStore.Internal.Operation.ReadEvent (readEvent) where

--------------------------------------------------------------------------------
import Data.Int
import Data.Maybe

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Serialize
import Data.Text

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.ReadEvent.Message
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
readEvent :: Settings
          -> Text
          -> Int32
          -> Bool
          -> Operation 'Init (ReadResult 'RegularStream ReadEvent)
readEvent Settings{..} s evtn tos = Operation create
  where
    create :: forall a. Input 'Init (ReadResult 'RegularStream ReadEvent) a -> a
    create (Create uuid) =
        let msg = newRequest s evtn tos s_requireMaster
            pkg = Package
                  { packageCmd         = 0xB0
                  , packageCorrelation = uuid
                  , packageData        = runPut $ encodeMessage msg
                  , packageCred        = s_credentials
                  } in
        (pkg, Operation pending)

    pending :: forall a. Input 'Pending (ReadResult 'RegularStream ReadEvent) a
            -> a
    pending (Arrived Package{..})
        | packageCmd == 0xB1 =
            Right $ decodeResp packageData $ \resp ->
                let r   = getField $ _result resp
                    evt = newResolvedEvent $ getField $ _indexedEvent resp
                    err = getField $ _error resp
                    not_found = ReadSuccess $ ReadEventNotFound s evtn
                    found     = ReadSuccess $ ReadEvent s evtn evt in
                case r of
                    NOT_FOUND      -> success not_found
                    NO_STREAM      -> success ReadNoStream
                    STREAM_DELETED -> success ReadStreamDeleted
                    ERROR          -> success (ReadError err)
                    ACCESS_DENIED  -> success ReadAccessDenied
                    SUCCESS        -> success found
        | otherwise = Left $ Operation pending