{-# LANGUAGE DeriveDataTypeable #-}
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
  , PackageArrived(..)
  , ConnectionError(..)
  , CloseConnection(..)
  , ConnectionEstablished(..)
  , connectionBuilder
  , dumbConnection
  ) where

--------------------------------------------------------------------------------
import Control.Monad.Fix
import Text.Printf

--------------------------------------------------------------------------------
import ClassyPrelude
import Control.Concurrent.Async (uninterruptibleCancel)
import Data.Serialize
import Data.UUID
import Data.UUID.V4
import qualified Network.Connection as Network

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.EndPoint
import Database.EventStore.Internal.Messaging
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
  Connection { connectionId       :: UUID
             , connectionEndPoint :: EndPoint
             , enqueuePackage     :: Package -> IO ()
             , dispose            :: IO ()
             }

--------------------------------------------------------------------------------
dumbConnection :: Connection
dumbConnection =
  Connection { connectionId       = nil
             , connectionEndPoint = EndPoint "dumb" 0
             , enqueuePackage     = \_ -> return ()
             , dispose            = return ()
             }

--------------------------------------------------------------------------------
data ConnectionState =
  ConnectionState { _bus       :: Publish
                  , _sendQueue :: TQueue Package
                  , _sending   :: TVar Bool
                  }

--------------------------------------------------------------------------------
data PackageArrived = PackageArrived Connection Package deriving Typeable

--------------------------------------------------------------------------------
data ConnectionError =
  ConnectionError Connection SomeException deriving Typeable

--------------------------------------------------------------------------------
data CloseConnection = CloseConnection SomeException deriving Typeable

--------------------------------------------------------------------------------
data ConnectionEstablished =
  ConnectionEstablished Connection deriving Typeable

--------------------------------------------------------------------------------
connectionBuilder :: Settings -> Publish -> IO ConnectionBuilder
connectionBuilder setts bus = do
  ctx <- Network.initConnectionContext
  return $ ConnectionBuilder $ \ept -> do
    state <- createState bus
    uuid  <- nextRandom

    mfix $ \self -> do
      tcpConnAsync <- async $
        tryAny (createConnection setts ctx ept) >>= \case
          Left e -> do
            publish bus (CloseConnection e)
            throw e
          Right conn -> do
            publish bus (ConnectionEstablished self)
            return conn

      sendAsync <- async (sending state self tcpConnAsync)
      recvAsync <- async (receiving state self tcpConnAsync)

      return Connection { connectionId       = uuid
                        , connectionEndPoint = ept
                        , enqueuePackage     = enqueue state
                        , dispose            = do
                            disposeConnection tcpConnAsync
                            uninterruptibleCancel sendAsync
                            uninterruptibleCancel recvAsync
                        }

--------------------------------------------------------------------------------
createState :: Publish -> IO ConnectionState
createState pub =
  ConnectionState pub <$> newTQueueIO
                      <*> newTVarIO False

--------------------------------------------------------------------------------
createConnection :: Settings
                 -> Network.ConnectionContext
                 -> EndPoint
                 -> IO Network.Connection
createConnection setts ctx ept = Network.connectTo ctx params
  where
    host   = endPointIp ept
    port   = fromIntegral $ endPointPort ept
    params = Network.ConnectionParams host port (s_ssl setts) Nothing

--------------------------------------------------------------------------------
disposeConnection :: Async Network.Connection -> IO ()
disposeConnection as = traverse_ tryDisposing =<< pollAsync as
  where
    tryDisposing = traverse_ disposing
    disposing    = Network.connectionClose


--------------------------------------------------------------------------------
receivePackage :: Network.Connection -> IO Package
receivePackage conn = do
  frame <- Network.connectionGetExact conn 4
  case runGet getLengthPrefix frame of
    Left _       -> throwString "Package framing error"
    Right prefix -> do
      payload <- Network.connectionGetExact conn prefix
      case runGet getPackage payload of
        Left _    -> throwString "Package parsing error"
        Right pkg -> return pkg

--------------------------------------------------------------------------------
receiving :: ConnectionState
          -> Connection
          -> Async Network.Connection
          -> IO (Async ())
receiving ConnectionState{..} self tcpConnAsync = async loop
  where
    loop = do
      tcpConn <- waitAsync tcpConnAsync
      tryAny (receivePackage tcpConn) >>= \case
        Left e    -> publish _bus (ConnectionError self e)
        Right pkg -> do
          publish _bus (PackageArrived self pkg)
          loop
      return ()

--------------------------------------------------------------------------------
enqueue :: ConnectionState -> Package -> IO ()
enqueue ConnectionState{..} pkg = atomically $ writeTQueue _sendQueue pkg

--------------------------------------------------------------------------------
sending :: ConnectionState
        -> Connection
        -> Async Network.Connection
        -> IO (Async ())
sending ConnectionState{..} self tcpConnAsync = async loop
  where
    loop = do
      conn    <- waitAsync tcpConnAsync
      outcome <- atomically $
        tryReadTQueue _sendQueue >>= \case
          Just pkg -> return (Just pkg)
          _        -> Nothing <$ writeTVar _sending False

      case outcome of
        Nothing  -> return ()
        Just pkg -> do
          let bytes = runPut $ putPackage pkg
          tryAny (Network.connectionPut conn bytes) >>= \case
            Left e  -> publish _bus (ConnectionError self e)
            Right _ -> loop

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
