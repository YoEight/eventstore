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
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Word
import           System.IO
import           Text.Printf

--------------------------------------------------------------------------------
import Data.Attoparsec.ByteString hiding (word8)
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.Binary.Get
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

    loop = do
        header <- BL.hGet h 4
        let length_prefix = fromIntegral $ runGet getWord32le header

        msg <- B.hGet h length_prefix
        case parseOnly parsePackage msg of
            Right pack ->
                send chan (RecvPackage pack)
            Left e ->
                send chan (Notice $ printf "Parsing error [%s]\n" e)
        loop

--------------------------------------------------------------------------------
send :: TChan Msg -> Msg -> IO ()
send chan msg = atomically $ writeTChan chan msg

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------
parsePackage :: Parser Package
parsePackage = do
    cmd <- parseCmd
    flg <- parseFlag
    col <- parseUUID

    let pack = Package
               { packageCmd         = cmd
               , packageFlag        = flg
               , packageCorrelation = col
               }

    return pack

--------------------------------------------------------------------------------
parseCmd :: Parser Command
parseCmd = do
    wd <- anyWord8
    case word8Cmd wd of
        Just cmd -> return cmd
        _        -> fail $ printf "TCP: Unhandled command value 0x%x" wd

--------------------------------------------------------------------------------
parseLength :: Parser Int
parseLength = decimal

--------------------------------------------------------------------------------
parseFlag :: Parser Flag
parseFlag = do
    wd <- anyWord8
    case wd of
        0x00 -> return None
        0x01 -> return Authenticated
        _    -> fail $ printf "TCP: Unhandled flag value 0x%x" wd

--------------------------------------------------------------------------------
parseUUID :: Parser UUID
parseUUID = do
    bs <- take 16
    case fromByteString $ BL.fromStrict bs of
        Just uuid -> return uuid
        _         -> fail "TCP: Wrong UUID format"
