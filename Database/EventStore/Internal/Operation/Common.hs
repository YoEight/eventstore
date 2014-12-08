--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.Common
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.Common
    ( OperationParams(..)
    , createOperation
    -- * Re-exports
    , module Data.ProtocolBuffers
    ) where

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Serialize
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data OperationParams request response
    = OperationParams
      { opSettings    :: !Settings
      , opRequestCmd  :: !Command
      , opResponseCmd :: !Command

      , opRequest     :: IO request
      , opSuccess     :: response -> IO Decision
      , opFailure     :: OperationException -> IO Decision
      }

--------------------------------------------------------------------------------
createOperation :: (Encode a, Decode b) => OperationParams a b -> Operation
createOperation params =
    Operation
    { operationCreatePackage = createPackage params
    , operationInspect       = inspection params
    }

--------------------------------------------------------------------------------
createPackage :: Encode a => OperationParams a b -> UUID -> IO Package
createPackage params uuid = do
    req <- opRequest params

    let pack = Package
               { packageCmd         = opRequestCmd params
               , packageCorrelation = uuid
               , packageFlag        = None
               , packageData        = runPut $ encodeMessage req
               }

    return pack

--------------------------------------------------------------------------------
inspection :: Decode b => OperationParams a b -> Package -> IO Decision
inspection params pack
    | found == exp_v = deeperInspection params pack
    | otherwise      = failed (InvalidServerResponse exp_v found)
  where
    exp_v  = opResponseCmd params
    failed = opFailure params
    found  = packageCmd pack

--------------------------------------------------------------------------------
deeperInspection :: Decode b => OperationParams a b -> Package -> IO Decision
deeperInspection params pack =
    case runGet decodeMessage bytes of
        Left e    -> failed (ProtobufDecodingError e)
        Right msg -> succeed msg
  where
    failed  = opFailure params
    succeed = opSuccess params
    bytes   = packageData pack
