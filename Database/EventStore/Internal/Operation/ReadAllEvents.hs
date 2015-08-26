{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.ReadAllEvents
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.ReadAllEvents
    (readAllEvents) where

--------------------------------------------------------------------------------
import Data.Int
import Data.Maybe
import Data.Word

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Serialize

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Read.Common
import Database.EventStore.Internal.Operation.ReadAllEvents.Message
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
readAllEvents :: Settings
              -> Int64
              -> Int64
              -> Int32
              -> Bool
              -> ReadDirection
              -> Operation 'Init AllSlice
readAllEvents Settings{..} c_pos p_pos max_c tos dir = Operation create
  where
    create :: forall a. Input 'Init AllSlice a -> a
    create (Create uuid) =
        let msg      = newRequest c_pos p_pos max_c tos s_requireMaster
            cmd      = case dir of
                           Forward  -> 0xB6
                           Backward -> 0xB8

            resp_cmd = case dir of
                           Forward  -> 0xB7
                           Backward -> 0xB9

            pkg      = Package
                       { packageCmd         = cmd
                       , packageCorrelation = uuid
                       , packageData        = runPut $ encodeMessage msg
                       , packageCred        = s_credentials
                       } in
        (pkg, Operation $ pending resp_cmd)

    pending :: forall a. Word8 -> Input 'Pending AllSlice a -> a
    pending cmd (Arrived Package{..})
        | packageCmd == cmd =
            Right $ decodeResp packageData $ \resp ->
                let r      = getField $ _Result resp
                    err    = getField $ _Error resp
                    nc_pos = getField $ _NextCommitPosition resp
                    np_pos = getField $ _NextPreparePosition resp
                    es     = getField $ _Events resp
                    evts   = fmap newResolvedEventFromBuf es
                    eos    = null evts
                    f_pos  = Position c_pos p_pos
                    n_pos  = Position nc_pos np_pos
                    slice  = AllSlice f_pos n_pos dir evts eos in
                case fromMaybe SUCCESS r of
                    ERROR         -> serverError err
                    ACCESS_DENIED -> accessDenied AllStream
                    _             -> success slice
        | otherwise = Left $ Operation $ pending cmd
