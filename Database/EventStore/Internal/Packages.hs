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
import           Data.Foldable (for_)

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
               , packageCorrelation = uuid
               , packageData        = B.empty
               , packageCred        = Nothing
               }

    return pack

--------------------------------------------------------------------------------
heartbeatResponsePackage :: UUID -> Package
heartbeatResponsePackage uuid =
    Package
    { packageCmd         = 0x02
    , packageCorrelation = uuid
    , packageData        = B.empty
    , packageCred        = Nothing
    }

--------------------------------------------------------------------------------
putPackage :: Package -> Put
putPackage pack = do
    putWord32le length_prefix
    putWord8 (packageCmd pack)
    putWord8 flag_word8
    putLazyByteString corr_bytes
    for_ cred_m $ \(Credentials login passw) -> do
        putWord8 $ fromIntegral $ B.length login
        putByteString login
        putWord8 $ fromIntegral $ B.length passw
        putByteString passw
    putByteString pack_data
  where
    pack_data     = packageData pack
    cred_len      = maybe 0 credSize cred_m
    length_prefix = fromIntegral (B.length pack_data + mandatorySize + cred_len)
    cred_m        = packageCred pack
    flag_word8    = maybe 0x00 (const 0x01) cred_m
    corr_bytes    = toByteString $ packageCorrelation pack

--------------------------------------------------------------------------------
credSize :: Credentials -> Int
credSize (Credentials login passw) = B.length login + B.length passw + 2

--------------------------------------------------------------------------------
mandatorySize :: Int
mandatorySize = 18
