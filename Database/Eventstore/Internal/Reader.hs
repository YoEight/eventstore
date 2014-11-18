--------------------------------------------------------------------------------
-- |
-- Module : Database.Eventstore.Internal.Reader
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.Eventstore.Internal.Reader (readerThread) where

--------------------------------------------------------------------------------
import           Prelude hiding (take)
import           Control.Concurrent.STM
import qualified Data.ByteString as B
import           Data.Word
import           System.IO
import           Text.Printf

--------------------------------------------------------------------------------
import Data.Serialize.Get
import Data.UUID

--------------------------------------------------------------------------------
import Database.Eventstore.Internal.Types

--------------------------------------------------------------------------------
readerThread :: TChan Msg -> Handle -> IO ()
readerThread chan h = loop
  where
    carbage ""   = True
    carbage "\n" = True
    carbage _    = False

    parsePackage bs =
        case runGet getPackage bs of
            Left e     -> send chan (Notice $ printf "Parsing error [%s]\n" e)
            Right pack -> send chan (RecvPackage pack)

    loop = do
        header_bs <- B.hGet h 4
        case runGet getLengthPrefix header_bs of
            Left e
                -> send chan (Notice "Wrong package framing\n")
            Right length_prefix
                -> B.hGet h length_prefix >>= parsePackage
        loop

--------------------------------------------------------------------------------
send :: TChan Msg -> Msg -> IO ()
send chan msg = atomically $ writeTChan chan msg

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------
getLengthPrefix :: Get Int
getLengthPrefix = fmap fromIntegral getWord32le

--------------------------------------------------------------------------------
getPackage :: Get Package
getPackage = do
    cmd  <- getCmd
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
getCmd :: Get Command
getCmd = do
    wd <- getWord8
    case word8Cmd wd of
        Just cmd -> return cmd
        _        -> fail $ printf "TCP: Unhandled command value 0x%x" wd

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
