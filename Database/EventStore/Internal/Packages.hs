--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Packages
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Packages
    ( -- * Package Smart Contructors
      heartbeatPackage
    , heartbeatResponsePackage
      -- * Cereal Put
    , putPackage
    ) where

--------------------------------------------------------------------------------
import qualified Data.ByteString as B

--------------------------------------------------------------------------------
import Data.Serialize.Put
import Data.UUID
import System.Random

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- Encode
--------------------------------------------------------------------------------
heartbeatPackage :: IO Package
heartbeatPackage = do
    uuid <- randomIO
    let pack = Package
               { packageCmd         = 0x01
               , packageFlag        = None
               , packageCorrelation = uuid
               , packageData        = B.empty
               }

    return pack

--------------------------------------------------------------------------------
heartbeatResponsePackage :: UUID -> Package
heartbeatResponsePackage uuid =
    Package
    { packageCmd         = 0x02
    , packageFlag        = None
    , packageCorrelation = uuid
    , packageData        = B.empty
    }

--------------------------------------------------------------------------------
putPackage :: Package -> Put
putPackage pack =
    putWord32le length_prefix    >>
    putWord8 (packageCmd pack)   >>
    putWord8 flag_word8          >>
    putLazyByteString corr_bytes >>
    putByteString pack_data
  where
    pack_data     = packageData pack
    length_prefix = fromIntegral (B.length pack_data + mandatorySize)
    flag_word8    = flagWord8 $ packageFlag pack
    corr_bytes    = toByteString $ packageCorrelation pack

--------------------------------------------------------------------------------
mandatorySize :: Int
mandatorySize = 18
