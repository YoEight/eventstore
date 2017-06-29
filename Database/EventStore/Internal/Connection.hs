{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
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
  , ConnectionEstablished(..)
  , ConnectionClosed(..)
  , ConnectionRef(..)
  , getConnection
  , connectionBuilder
  , connectionError
  ) where

--------------------------------------------------------------------------------
import Prelude (String)
import Text.Printf

--------------------------------------------------------------------------------
import Control.Monad.Reader
import Data.Serialize
import Data.UUID
import qualified Network.Connection as Network

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.EndPoint
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
newtype ConnectionBuilder =
  ConnectionBuilder { connect :: EndPoint -> EventStore Connection }

--------------------------------------------------------------------------------
data RecvOutcome
  = ResetByPeer
  | Recv Package
  | WrongFraming
  | ParsingError

--------------------------------------------------------------------------------
type ConnectionId = UUID

--------------------------------------------------------------------------------
newtype ConnectionRef =
  ConnectionRef { maybeConnection :: EventStore (Maybe Connection) }

--------------------------------------------------------------------------------
getConnection :: ConnectionRef -> EventStore Connection
getConnection ref =
  maybeConnection ref >>= \case
    Just conn -> return conn
    Nothing   -> do
      $(logError) "Expected a connection but got none."
      throwString "No current connection (impossible situation)"

--------------------------------------------------------------------------------
data Connection =
  Connection { connectionId       :: ConnectionId
             , connectionEndPoint :: EndPoint
             , enqueuePackage     :: Package -> EventStore ()
             , dispose            :: EventStore ()
             }

--------------------------------------------------------------------------------
instance Show Connection where
  show Connection{..} = "Connection [" <> show connectionId <> "] on "
                        <> show connectionEndPoint

--------------------------------------------------------------------------------
instance Eq Connection where
  a == b = connectionId a == connectionId b

--------------------------------------------------------------------------------
data ConnectionState =
  ConnectionState { _bus       :: Publish
                  , _sendQueue :: TBMQueue Package
                  }

--------------------------------------------------------------------------------
data PackageArrived = PackageArrived Connection Package deriving Typeable

--------------------------------------------------------------------------------
data ConnectionError =
  ConnectionError Connection SomeException deriving Typeable

--------------------------------------------------------------------------------
connectionError :: Exception e => Connection -> e -> ConnectionError
connectionError c = ConnectionError c . toException

--------------------------------------------------------------------------------
data ConnectionClosed = ConnectionClosed Connection SomeException
  deriving Typeable

--------------------------------------------------------------------------------
data ConnectionEstablished =
  ConnectionEstablished Connection deriving Typeable

--------------------------------------------------------------------------------
newtype ConnectionResetByPeer = ConnectionResetByPeer SomeException
  deriving Typeable

--------------------------------------------------------------------------------
instance Show ConnectionResetByPeer where
  show (ConnectionResetByPeer reason) =
    "Connection reset by peer: " <> show reason

--------------------------------------------------------------------------------
instance Exception ConnectionResetByPeer

--------------------------------------------------------------------------------
data ProtocolError
  = WrongFramingError !String
  | PackageParsingError !String
  deriving Typeable

--------------------------------------------------------------------------------
instance Show ProtocolError where
  show (WrongFramingError reason)   = "Package framing error: " <> reason
  show (PackageParsingError reason) = "Package parsing error: " <> reason

--------------------------------------------------------------------------------
instance Exception ProtocolError

--------------------------------------------------------------------------------
connectionBuilder :: Settings -> Publish -> IO ConnectionBuilder
connectionBuilder setts bus = do
  ctx <- Network.initConnectionContext
  return $ ConnectionBuilder $ \ept -> do
    cid <- freshUUID
    state <- createState bus

    mfix $ \self -> do
      tcpConnAsync <- async $
        tryAny (createConnection setts ctx ept) >>= \case
          Left e -> do
            publish bus (ConnectionClosed self e)
            throw e
          Right conn -> do
            publish bus (ConnectionEstablished self)
            return conn

      sendAsync <- async (sending state self tcpConnAsync)
      recvAsync <- async (receiving state self tcpConnAsync)
      return Connection { connectionId       = cid
                        , connectionEndPoint = ept
                        , enqueuePackage     = enqueue state
                        , dispose            = do
                            closeState state
                            disposeConnection tcpConnAsync
                            cancel sendAsync
                            cancel recvAsync
                        }

--------------------------------------------------------------------------------
createState :: Publish -> EventStore ConnectionState
createState pub = ConnectionState pub <$> liftIO (newTBMQueueIO 500)

--------------------------------------------------------------------------------
closeState :: ConnectionState -> EventStore ()
closeState ConnectionState{..} = atomically $ closeTBMQueue _sendQueue

--------------------------------------------------------------------------------
createConnection :: Settings
                 -> Network.ConnectionContext
                 -> EndPoint
                 -> EventStore Network.Connection
createConnection setts ctx ept = liftIO $ Network.connectTo ctx params
  where
    host   = endPointIp ept
    port   = fromIntegral $ endPointPort ept
    params = Network.ConnectionParams host port (s_ssl setts) Nothing

--------------------------------------------------------------------------------
disposeConnection :: Async Network.Connection -> EventStore ()
disposeConnection as = traverse_ tryDisposing =<< poll as
  where
    tryDisposing = traverse_ disposing
    disposing    = liftIO . Network.connectionClose

--------------------------------------------------------------------------------
receivePackage :: Publish
               -> Connection
               -> Network.Connection
               -> EventStore Package
receivePackage pub self conn =
  tryAny (liftIO $ Network.connectionGetExact conn 4) >>= \case
    Left e -> do
      publish pub (ConnectionClosed self e)
      throw e
    Right frame ->
      case runGet getLengthPrefix frame of
        Left reason -> do
          let cause = WrongFramingError reason
          publish pub (connectionError self cause)
          throw cause
        Right prefix -> do
          tryAny (liftIO $ Network.connectionGetExact conn prefix) >>= \case
            Left e -> do
              publish pub (ConnectionClosed self e)
              throw e
            Right payload ->
              case runGet getPackage payload of
                Left reason -> do
                  let cause = PackageParsingError reason
                  publish pub (connectionError self cause)
                  throw cause
                Right pkg -> return pkg

--------------------------------------------------------------------------------
receiving :: ConnectionState
          -> Connection
          -> Async Network.Connection
          -> EventStore ()
receiving ConnectionState{..} self tcpConnAsync =
  forever . go =<< wait tcpConnAsync
  where
    go conn =
      publish _bus . PackageArrived self =<< receivePackage _bus self conn

--------------------------------------------------------------------------------
enqueue :: ConnectionState -> Package -> EventStore ()
enqueue ConnectionState{..} pkg@Package{..} = do
  $(logDebug) [i|Package enqueued: #{pkg}|]
  atomically $ writeTBMQueue _sendQueue pkg

--------------------------------------------------------------------------------
sending :: ConnectionState
        -> Connection
        -> Async Network.Connection
        -> EventStore ()
sending ConnectionState{..} self tcpConnAsync = go =<< wait tcpConnAsync
  where
    go conn =
      let loop     = traverse_ send =<< atomically (readTBMQueue _sendQueue)
          send pkg =
            tryAny (liftIO $ Network.connectionPut conn bytes) >>= \case
              Left e  -> publish _bus (ConnectionClosed self e)
              Right _ -> loop
            where
              bytes = runPut $ putPackage pkg in
      loop

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
        putWord8 $ fromIntegral $ length login
        putByteString login
        putWord8 $ fromIntegral $ length passw
        putByteString passw
    putByteString pack_data
  where
    pack_data     = packageData pkg
    cred_len      = maybe 0 credSize cred_m
    length_prefix = fromIntegral (length pack_data + mandatorySize + cred_len)
    cred_m        = packageCred pkg
    flag_word8    = maybe 0x00 (const 0x01) cred_m
    corr_bytes    = toByteString $ packageCorrelation pkg

--------------------------------------------------------------------------------
credSize :: Credentials -> Int
credSize (Credentials login passw) = length login + length passw + 2

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
