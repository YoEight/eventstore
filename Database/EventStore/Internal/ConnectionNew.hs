{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
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
module Database.EventStore.Internal.ConnectionNew
  ( ConnectionBuilder(..)
  , Connection(..)
  , connectionBuilder
  ) where

--------------------------------------------------------------------------------
import Prelude (String)
import Text.Printf

--------------------------------------------------------------------------------
import           Control.Monad.Reader
import           Data.Serialize
import           Data.UUID
import           Data.UUID.V4
import qualified Network.Connection as Network

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Control
import Database.EventStore.Internal.EndPoint
import Database.EventStore.Internal.Logger
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
newtype ConnectionBuilder =
  ConnectionBuilder { connect :: EndPoint -> IO Connection }

--------------------------------------------------------------------------------
type ConnectionId = UUID

--------------------------------------------------------------------------------
data ConnectionEvent
  = ConnectionClosed ConnectionId SomeException
  | ConnectionEstablished ConnectionId
  | ConnectionPackageArrived ConnectionId Package
  | ConnectionError ConnectionId ProtocolError

--------------------------------------------------------------------------------
newtype SubmitConnectionEvent =
  SubmitConnectionEvent { submitConnectionEvent :: ConnectionEvent -> IO () }

--------------------------------------------------------------------------------
data Connection =
  Connection { connectionId       :: ConnectionId
             , connectionEndPoint :: EndPoint
             , enqueuePackage     :: Package -> IO ()
             , dispose            :: IO ()
             }

--------------------------------------------------------------------------------
instance Show Connection where
  show Connection{..} = "Connection [" <> show connectionId <> "] on "
                        <> show connectionEndPoint

--------------------------------------------------------------------------------
instance Eq Connection where
  a == b = connectionId a == connectionId b

--------------------------------------------------------------------------------
newtype ConnectionState =
  ConnectionState { _sendQueue :: TBMQueue Package }

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
connectionBuilder :: Settings -> SubmitConnectionEvent -> IO ConnectionBuilder
connectionBuilder setts sub = do
  ctx <- Network.initConnectionContext
  return $ ConnectionBuilder $ \ept -> do
    cid <- nextRandom
    state <- createState

    mfix $ \self -> do
      tcpConnAsync <- async $
        tryAny (createConnection setts ctx ept) >>= \case
          Left e -> do
            submitConnectionEvent sub (ConnectionClosed cid e)
            throw e
          Right conn ->
            conn <$
              submitConnectionEvent sub (ConnectionEstablished cid)

      sendAsync <- async (sending sub state self tcpConnAsync)
      recvAsync <- async (receiving sub state self tcpConnAsync)
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
createState :: IO ConnectionState
createState = ConnectionState <$> newTBMQueueIO 500

--------------------------------------------------------------------------------
closeState :: ConnectionState -> IO ()
closeState ConnectionState{..} = atomically $ closeTBMQueue _sendQueue

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
disposeConnection as = traverse_ tryDisposing =<< poll as
  where
    tryDisposing = traverse_ disposing
    disposing    = Network.connectionClose

--------------------------------------------------------------------------------
receivePackage :: SubmitConnectionEvent
               -> Connection
               -> Network.Connection
               -> IO Package
receivePackage sub self conn =
  tryAny (Network.connectionGetExact conn 4) >>= \case
    Left e -> do
      submitConnectionEvent sub (ConnectionClosed (connectionId self) e)
      throw e
    Right frame ->
      case runGet getLengthPrefix frame of
        Left reason -> do
          let cause = WrongFramingError reason
          submitConnectionEvent sub (ConnectionError (connectionId self) cause)
          throw cause
        Right prefix -> do
          tryAny (Network.connectionGetExact conn prefix) >>= \case
            Left e -> do
              submitConnectionEvent sub (ConnectionClosed (connectionId self) e)
              throw e
            Right payload ->
              case runGet getPackage payload of
                Left reason -> do
                  let cause = PackageParsingError reason
                  submitConnectionEvent sub (ConnectionError (connectionId self) cause)
                  throw cause
                Right pkg -> return pkg

--------------------------------------------------------------------------------
receiving :: SubmitConnectionEvent
          -> ConnectionState
          -> Connection
          -> Async Network.Connection
          -> IO ()
receiving sub ConnectionState{..} self tcpConnAsync =
  forever . go =<< wait tcpConnAsync
  where
    go conn =
      submitConnectionEvent sub . ConnectionPackageArrived (connectionId self)
        =<< receivePackage sub self conn

--------------------------------------------------------------------------------
enqueue :: ConnectionState -> Package -> IO ()
enqueue ConnectionState{..} pkg@Package{..} = do
  -- $(logDebug) [i|Package enqueued: #{pkg}|]
  atomically $ writeTBMQueue _sendQueue pkg

--------------------------------------------------------------------------------
sending :: SubmitConnectionEvent
        -> ConnectionState
        -> Connection
        -> Async Network.Connection
        -> IO ()
sending sub ConnectionState{..} self tcpConnAsync = go =<< wait tcpConnAsync
  where
    cid = connectionId self

    go conn =
      let loop = traverse_ send =<< atomically (readTBMQueue _sendQueue)

          send pkg =
            tryAny (Network.connectionPut conn bytes) >>= \case
              Left e  -> submitConnectionEvent sub (ConnectionClosed cid e)
              Right _ -> do
                -- monitorAddDataTransmitted (length bytes)
                loop
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
