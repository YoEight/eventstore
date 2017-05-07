--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Connection
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Connection
  ( ConnectionBuilder(..)
  , Connection(..)
  , RecvOutcome(..)
  , connectionBuilder
  ) where

--------------------------------------------------------------------------------
import Text.Printf

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Serialize
import Data.UUID
import Network.Connection hiding (Connection)

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.EndPoint
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
newtype ConnectionBuilder =
  ConnectionBuilder { connect :: EndPoint -> IO Connection }

--------------------------------------------------------------------------------
data RecvOutcome
  = ResetByPeer
  | Recv Package
  | WrongFraming
  | ParsingError

--------------------------------------------------------------------------------
data Connection =
  Connection { sendPackage    :: Package -> IO ()
             , receivePackage :: IO RecvOutcome
             , dispose        :: IO ()
             }

--------------------------------------------------------------------------------
connectionBuilder :: Settings -> IO ConnectionBuilder
connectionBuilder setts = do
  ctx <- initConnectionContext
  return $ ConnectionBuilder $ \ept -> do
    let host   = endPointIp ept
        port   = fromIntegral $ endPointPort ept
        params = ConnectionParams host port (s_ssl setts) Nothing

    conn <- connectTo ctx params
    return Connection { sendPackage = \pkg ->
                          connectionPut conn (runPut $ putPackage pkg)
                      , receivePackage = do
                          outcome <- tryAny $ connectionGetExact conn 4
                          case outcome of
                            Left _         -> return ResetByPeer
                            Right headerBs -> do
                              case runGet getLengthPrefix headerBs of
                                Left _       -> return WrongFraming
                                Right prefix -> do
                                  bs <- connectionGetExact conn prefix
                                  case runGet getPackage bs of
                                    Left _    -> return ParsingError
                                    Right pkg -> return (Recv pkg)
                      , dispose = connectionClose conn
                      }

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------
-- | Serializes a 'Package' into raw bytes.
putPackage :: Package -> Put
putPackage pkg = do
    putWord32le length_prefix
    putWord8 (cmdWord8 $ packageCmd pkg)
    putWord8 flag_word8
    putLazyByteString corr_bytes
    for_ cred_m $ \(Credentials login passw) -> do
        putWord8 $ fromIntegral $ olength login
        putByteString login
        putWord8 $ fromIntegral $ olength passw
        putByteString passw
    putByteString pack_data
  where
    pack_data     = packageData pkg
    cred_len      = maybe 0 credSize cred_m
    length_prefix = fromIntegral (olength pack_data + mandatorySize + cred_len)
    cred_m        = packageCred pkg
    flag_word8    = maybe 0x00 (const 0x01) cred_m
    corr_bytes    = toByteString $ packageCorrelation pkg

--------------------------------------------------------------------------------
credSize :: Credentials -> Int
credSize (Credentials login passw) = olength login + olength passw + 2

--------------------------------------------------------------------------------
-- | The minimun size a 'Package' should have. It's basically a command byte,
--   correlation bytes ('UUID') and a 'Flag' byte.
mandatorySize :: Int
mandatorySize = 18

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------
getLengthPrefix :: Get Int
getLengthPrefix = fmap fromIntegral getWord32le

--------------------------------------------------------------------------------
getPackage :: Get Package
getPackage = do
    cmd  <- getWord8
    flg  <- getFlag
    col  <- getUUID
    cred <- getCredentials flg
    rest <- remaining
    dta  <- getBytes rest

    let pkg = Package
              { packageCmd         = getCommand cmd
              , packageCorrelation = col
              , packageData        = dta
              , packageCred        = cred
              }

    return pkg

--------------------------------------------------------------------------------
getFlag :: Get Flag
getFlag = do
    wd <- getWord8
    case wd of
        0x00 -> return None
        0x01 -> return Authenticated
        _    -> fail $ printf "TCP: Unhandled flag value 0x%x" wd

--------------------------------------------------------------------------------
getCredEntryLength :: Get Int
getCredEntryLength = fmap fromIntegral getWord8

--------------------------------------------------------------------------------
getCredentials :: Flag -> Get (Maybe Credentials)
getCredentials None = return Nothing
getCredentials _ = do
    loginLen <- getCredEntryLength
    login    <- getBytes loginLen
    passwLen <- getCredEntryLength
    passw    <- getBytes passwLen
    return $ Just $ credentials login passw

--------------------------------------------------------------------------------
getUUID :: Get UUID
getUUID = do
    bs <- getLazyByteString 16
    case fromByteString bs of
        Just uuid -> return uuid
        _         -> fail "TCP: Wrong UUID format"
