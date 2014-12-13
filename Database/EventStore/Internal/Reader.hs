--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Reader
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Reader (readerThread) where

--------------------------------------------------------------------------------
import           Prelude hiding (take)
import           Control.Monad
import qualified Data.ByteString as B
import           System.IO
import           Text.Printf

--------------------------------------------------------------------------------
import Data.Serialize.Get
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
readerThread :: (Package -> IO ()) -> Handle -> IO ()
readerThread push_p h = forever $ do
    header_bs <- B.hGet h 4
    case runGet getLengthPrefix header_bs of
        Left _
            -> error "Wrong package framing"
        Right length_prefix
            -> B.hGet h length_prefix >>= parsePackage
  where
    parsePackage bs =
        case runGet getPackage bs of
            Left e     -> error $ printf "Parsing error [%s]" e
            Right pack -> push_p pack

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------
getLengthPrefix :: Get Int
getLengthPrefix = fmap fromIntegral getWord32le

--------------------------------------------------------------------------------
getPackage :: Get Package
getPackage = do
    cmd  <- getWord8
    flg  <- getFlag
    col  <- getUUID
    rest <- remaining
    dta  <- getBytes rest

    let pack = Package
               { packageCmd         = cmd
               , packageFlag        = flg
               , packageCorrelation = col
               , packageData        = dta
               }

    return pack

--------------------------------------------------------------------------------
getFlag :: Get Flag
getFlag = do
    wd <- getWord8
    case wd of
        0x00 -> return None
        0x01 -> return Authenticated
        _    -> fail $ printf "TCP: Unhandled flag value 0x%x" wd

--------------------------------------------------------------------------------
getUUID :: Get UUID
getUUID = do
    bs <- getLazyByteString 16
    case fromByteString bs of
        Just uuid -> return uuid
        _         -> fail "TCP: Wrong UUID format"
