--------------------------------------------------------------------------------
-- |
-- Module : Database.Eventstore.Internal.Packages
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.Eventstore.Internal.Packages
    ( -- * Package Smart Contructors
      heartbeatPackage
    , heartbeatResponsePackage
    , writeEventsPackage
      -- * Cereal Put
    , putPackage
      -- * Decode
    , getWriteEventsCompleted
    ) where

--------------------------------------------------------------------------------
import qualified Data.ByteString as B

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Serialize.Get
import Data.Serialize.Put
import Data.UUID
import System.Random

--------------------------------------------------------------------------------
import Database.Eventstore.Internal.Types

--------------------------------------------------------------------------------
-- Encode
--------------------------------------------------------------------------------
writeEventsPackage :: Flag -> WriteEvents -> IO Package
writeEventsPackage flag msg = fmap go randomIO
  where
    go uuid = Package
              { packageCmd         = WriteEventsCmd
              , packageFlag        = flag
              , packageCorrelation = uuid
              , packageData        = runPut $ encodeMessage msg
              }

--------------------------------------------------------------------------------
heartbeatPackage :: IO Package
heartbeatPackage = do
    uuid <- randomIO
    let pack = Package
               { packageCmd         = HeartbeatRequest
               , packageFlag        = None
               , packageCorrelation = uuid
               , packageData        = B.empty
               }

    return pack

--------------------------------------------------------------------------------
heartbeatResponsePackage :: UUID -> Package
heartbeatResponsePackage uuid =
    Package
    { packageCmd         = HeartbeatResponse
    , packageFlag        = None
    , packageCorrelation = uuid
    , packageData        = B.empty
    }

--------------------------------------------------------------------------------
putPackage :: Package -> Put
putPackage pack =
    putWord32le length_prefix    >>
    putWord8 cmd_word8           >>
    putWord8 flag_word8          >>
    putLazyByteString corr_bytes >>
    putByteString pack_data
  where
    pack_data     = packageData pack
    length_prefix = fromIntegral (B.length pack_data + mandatorySize)
    cmd_word8     = cmdWord8 $ packageCmd pack
    flag_word8    = flagWord8 $ packageFlag pack
    corr_bytes    = toByteString $ packageCorrelation pack

--------------------------------------------------------------------------------
-- Decode
--------------------------------------------------------------------------------
getWriteEventsCompleted :: Get WriteEventsCompleted
getWriteEventsCompleted = decodeMessage

--------------------------------------------------------------------------------
mandatorySize :: Int
mandatorySize = 18
