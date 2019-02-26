{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Discovery
-- Copyright : (C) 2016 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Discovery
    ( Discovery(..)
    , GossipSeed
    , DnsDiscoveryException(..)
    , ClusterSettings(..)
    , DnsServer(..)
    , EndPoint(..)
    , staticEndPointDiscovery
    , clusterDnsEndPointDiscovery
    , gossipSeedClusterSettings
    , simpleDnsEndPointDiscovery
    , dnsClusterSettings
    , gossipSeed
    , gossipSeedWithHeader
    , gossipSeedHeader
    , gossipSeedHost
    , gossipSeedPort
    ) where

--------------------------------------------------------------------------------
import Prelude (String, putStrLn)
import Data.Maybe

--------------------------------------------------------------------------------
import Control.Exception.Safe (SomeException, tryAny)
import Data.Aeson
import Data.Aeson.Types
import Data.Array.IO
import Data.DotNet.TimeSpan
import Data.List.NonEmpty (NonEmpty)
import Data.UUID
import Network.HTTP.Client
import Network.DNS hiding (decode)
import System.Random

--------------------------------------------------------------------------------
import Database.EventStore.Internal.EndPoint
import Database.EventStore.Internal.Prelude

--------------------------------------------------------------------------------
data DnsDiscoveryException
    = MaxDiscoveryAttemptReached ByteString
    | DNSDiscoveryError DNSError
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception DnsDiscoveryException

--------------------------------------------------------------------------------
httpRequest :: EndPoint -> String -> IO Request
httpRequest (EndPoint ip p) path = parseUrlThrow url
  where
    url = "http://" <> ip <> ":" <> show p <> path

--------------------------------------------------------------------------------
-- | Represents a source of cluster gossip.
data GossipSeed =
    GossipSeed
    { gossipEndpoint :: !EndPoint
      -- ^ The endpoint for the external HTTP endpoint of the gossip seed. The
      --   HTTP endpoint is used rather than the TCP endpoint because it is
      --   required for the client to exchange gossip with the server.
      --   standard port which should be used here in 2113.
    , gossipSeedHeader :: !String
      -- ^ The host header to be sent when requesting gossip.
    } deriving Show

--------------------------------------------------------------------------------
-- | Creates a 'GossipSeed'.
gossipSeed :: String -> Int -> GossipSeed
gossipSeed h p = GossipSeed (EndPoint h p) ""

--------------------------------------------------------------------------------
-- | Creates a 'GossipSeed' with a specific HTTP header.
gossipSeedWithHeader :: String -> Int -> String -> GossipSeed
gossipSeedWithHeader h p hd = GossipSeed (EndPoint h p) hd

--------------------------------------------------------------------------------
-- | Returns 'GossipSeed' host IP address.
gossipSeedHost :: GossipSeed -> String
gossipSeedHost = endPointIp . gossipEndpoint

--------------------------------------------------------------------------------
-- | Returns 'GossipSeed' port.
gossipSeedPort :: GossipSeed -> Int
gossipSeedPort = endPointPort . gossipEndpoint

--------------------------------------------------------------------------------
emptyGossipSeed :: GossipSeed
emptyGossipSeed = GossipSeed emptyEndPoint ""

--------------------------------------------------------------------------------
-- | Procedure used to discover an network 'EndPoint'.
newtype Discovery =
    Discovery { runDiscovery :: Maybe EndPoint -> IO (Maybe EndPoint) }

--------------------------------------------------------------------------------
staticEndPointDiscovery :: String -> Int -> Discovery
staticEndPointDiscovery host port =
    Discovery $ \_ -> return $ Just $ EndPoint host port

--------------------------------------------------------------------------------
simpleDnsEndPointDiscovery :: ByteString -> Maybe DnsServer -> Int -> Discovery
simpleDnsEndPointDiscovery domain srv port = Discovery $ \_ -> do
    let conf =
            case srv of
                Nothing  -> defaultResolvConf
                Just tpe ->
                    let rc =
                            case tpe of
                                DnsFilePath p   -> RCFilePath p
                                DnsHostName h   -> RCHostName h
                                DnsHostPort h p -> RCHostPort h (fromIntegral p)
                    in defaultResolvConf { resolvInfo = rc }
    dnsSeed <- makeResolvSeed conf
    res     <- withResolver dnsSeed $ \resv -> lookupA resv domain
    case res of
        Left e    -> throwIO $ DNSDiscoveryError e
        Right ips -> do
            let pts = [ EndPoint (show ip) port | ip <- ips ]
            case pts of
                []   -> return Nothing
                pt:_ -> return $ Just pt

--------------------------------------------------------------------------------
-- | Tells how the DNS server should be contacted.
data DnsServer
    = DnsFilePath String
    | DnsHostName String
    | DnsHostPort String Int

--------------------------------------------------------------------------------
-- | Contains settings related to a connection to a cluster.
data ClusterSettings =
    ClusterSettings
    { clusterDns :: !ByteString
      -- ^ The DNS name to use for discovering endpoints.
    , clusterMaxDiscoverAttempts :: !Int
      -- ^ The maximum number of attempts for discovering endpoints.
    , clusterExternalGossipPort :: !Int
      -- ^ The well-known endpoint on which cluster managers are running.
    , clusterGossipSeeds :: (Maybe (NonEmpty GossipSeed))
      -- ^ Endpoints for seeding gossip if not using DNS.
    , clusterGossipTimeout :: !TimeSpan
      -- ^ Timeout for cluster gossip.
    , clusterDnsServer :: !(Maybe DnsServer)
      -- ^ Indicates a specific DNS server should be contacted.
    }

--------------------------------------------------------------------------------
-- | Configures a 'ClusterSettings' for connecting to a cluster using gossip
--   seeds.
--   clusterDns                 = ""
--   clusterMaxDiscoverAttempts = 10
--   clusterExternalGossipPort  = 0
--   clusterGossipTimeout       = 1s
gossipSeedClusterSettings :: NonEmpty GossipSeed -> ClusterSettings
gossipSeedClusterSettings xs =
    ClusterSettings
    { clusterDns                 = ""
    , clusterMaxDiscoverAttempts = 10
    , clusterExternalGossipPort  = 0
    , clusterGossipSeeds         = Just xs
    , clusterGossipTimeout       = fromSeconds 1
    , clusterDnsServer           = Nothing
    }

--------------------------------------------------------------------------------
-- | Configures a 'ClusterSettings' for connecting to a cluster using DNS
--   discovery.
--   clusterMaxDiscoverAttempts = 10
--   clusterExternalGossipPort  = 0
--   clusterGossipSeeds         = Nothing
--   clusterGossipTimeout       = 1s
dnsClusterSettings :: ByteString -> ClusterSettings
dnsClusterSettings clusterDns = ClusterSettings{..}
  where
    clusterMaxDiscoverAttempts = 10
    clusterExternalGossipPort  = 0
    clusterGossipSeeds         = Nothing
    clusterGossipTimeout       = fromSeconds 1
    clusterDnsServer           = Nothing

--------------------------------------------------------------------------------
clusterDnsEndPointDiscovery :: ClusterSettings -> IO Discovery
clusterDnsEndPointDiscovery settings = do
    ref     <- newIORef Nothing
    manager <- newManager defaultManagerSettings
    return $ Discovery $ \fend -> discoverEndPoint manager ref fend settings

--------------------------------------------------------------------------------
data VNodeState
    = Initializing
    | Unknown
    | PreReplica
    | CatchingUp
    | Clone
    | Slave
    | PreMaster
    | Master
    | Manager
    | ShuttingDown
    | Shutdown
    deriving (Eq, Ord, Generic, Show)

--------------------------------------------------------------------------------
instance FromJSON VNodeState

--------------------------------------------------------------------------------
newtype GUUID = GUUID UUID deriving Show

--------------------------------------------------------------------------------
instance FromJSON GUUID where
    parseJSON (String txt) =
        case fromText txt of
            Just uuid -> return $ GUUID uuid
            _         -> fail $ "Wrong UUID format " <> show txt
    parseJSON invalid = typeMismatch "UUID" invalid

--------------------------------------------------------------------------------
data MemberInfo =
    MemberInfo
    { _instanceId :: !GUUID
    , _state :: !VNodeState
    , _isAlive :: !Bool
    , _internalTcpIp :: !String
    , _internalTcpPort :: !Int
    , _externalTcpIp :: !String
    , _externalTcpPort :: !Int
    , _internalHttpIp :: !String
    , _internalHttpPort :: !Int
    , _externalHttpIp :: !String
    , _externalHttpPort :: !Int
    , _lastCommitPosition :: !Int64
    , _writerCheckpoint :: !Int64
    , _chaserCheckpoint :: !Int64
    , _epochPosition :: !Int64
    , _epochNumber :: !Int
    , _epochId :: !GUUID
    , _nodePriority :: !Int
    } deriving Show

--------------------------------------------------------------------------------
instance FromJSON MemberInfo where
    parseJSON (Object m) =
        MemberInfo
        <$> m .: "instanceId"
        <*> m .: "state"
        <*> m .: "isAlive"
        <*> m .: "internalTcpIp"
        <*> m .: "internalTcpPort"
        <*> m .: "externalTcpIp"
        <*> m .: "externalTcpPort"
        <*> m .: "internalHttpIp"
        <*> m .: "internalHttpPort"
        <*> m .: "externalHttpIp"
        <*> m .: "externalHttpPort"
        <*> m .: "lastCommitPosition"
        <*> m .: "writerCheckpoint"
        <*> m .: "chaserCheckpoint"
        <*> m .: "epochPosition"
        <*> m .: "epochNumber"
        <*> m .: "epochId"
        <*> m .: "nodePriority"
    parseJSON invalid = typeMismatch "MemberInfo" invalid

--------------------------------------------------------------------------------
data ClusterInfo =
    ClusterInfo { members :: [MemberInfo] }
    deriving (Show, Generic)

--------------------------------------------------------------------------------
instance FromJSON ClusterInfo

--------------------------------------------------------------------------------
discoverEndPoint :: Manager
                 -> IORef (Maybe [MemberInfo])
                 -> Maybe EndPoint
                 -> ClusterSettings
                 -> IO (Maybe EndPoint)
discoverEndPoint mgr ref fend settings = do
    old_m <- readIORef ref
    writeIORef ref Nothing
    candidates <- case old_m of
        Nothing  -> gossipCandidatesFromDns settings
        Just old -> gossipCandidatesFromOldGossip fend old
    forArrayFirst candidates $ \i -> do
        c   <- readArray candidates i
        res <- tryGetGossipFrom settings mgr c
        print (i, res)
        let fin_end = do
                info <- res
                best <- tryDetermineBestNode $ members info
                return (info, best)
        case fin_end of
            Nothing   -> return Nothing
            Just (info, best) -> do
                writeIORef  ref (Just $ members info)
                return $ Just best

--------------------------------------------------------------------------------
tryGetGossipFrom :: ClusterSettings
                 -> Manager
                 -> GossipSeed
                 -> IO (Maybe ClusterInfo)
tryGetGossipFrom ClusterSettings{..} mgr seed = do
    init_req <- httpRequest (gossipEndpoint seed) "/gossip?format=json"
    let timeout = truncate (totalMillis clusterGossipTimeout * 1000)
        req     = init_req { responseTimeout = responseTimeoutMicro timeout }
    eithResp <- tryAny $ httpLbs req mgr
    case eithResp of
        Right resp -> return $ decode $ responseBody resp
        Left err   -> pure Nothing -- FIXME Notify of the failed attempt somehow

--------------------------------------------------------------------------------
tryDetermineBestNode :: [MemberInfo] -> Maybe EndPoint
tryDetermineBestNode members = node_m
  where
    nodes = [m | m <- members
               , _isAlive m
               , allowedState $ _state m
               ]

    node_m =
        case sortOn (Down . _state) nodes of
            []  -> Nothing
            n:_ -> Just $ EndPoint (_externalTcpIp n) (_externalTcpPort n)

    allowedState Manager      = False
    allowedState ShuttingDown = False
    allowedState Shutdown     = False
    allowedState _            = True

--------------------------------------------------------------------------------
gossipCandidatesFromOldGossip :: Maybe EndPoint
                              -> [MemberInfo]
                              -> IO (IOArray Int GossipSeed)
gossipCandidatesFromOldGossip fend_m oldGossip =
    arrangeGossipCandidates candidates
  where
    candidates =
        case fend_m of
            Nothing   -> oldGossip
            Just fend -> [ c | c <- oldGossip
                             , _externalTcpPort c == endPointPort fend
                             , _externalTcpIp c   == endPointIp fend
                             ]

--------------------------------------------------------------------------------
data AState = AState !Int !Int

--------------------------------------------------------------------------------
arrangeGossipCandidates :: [MemberInfo] -> IO (IOArray Int GossipSeed)
arrangeGossipCandidates members = do
    arr        <- newArray (0, len) emptyGossipSeed
    AState i j <- foldM (go arr) (AState (-1) len) members

    shuffle arr 0 i         -- shuffle nodes
    shuffle arr j (len - 1) -- shuffle managers

    return arr
  where
    len = length members

    go :: IOArray Int GossipSeed -> AState -> MemberInfo -> IO AState
    go arr (AState i j) m =
        case _state m of
            Manager -> do
                let new_j = j - 1
                writeArray arr new_j seed
                return (AState i new_j)
            _ -> do
                let new_i = i + 1
                writeArray arr new_i seed
                return (AState new_i j)
      where
        end  = EndPoint (_externalHttpIp m) (_externalHttpPort m)
        seed = GossipSeed end ""

--------------------------------------------------------------------------------
gossipCandidatesFromDns :: ClusterSettings -> IO (IOArray Int GossipSeed)
gossipCandidatesFromDns settings@ClusterSettings{..} = do
    arr <- endpoints
    shuffleAll arr
    return arr
  where
    endpoints =
        case clusterGossipSeeds of
            Nothing -> resolveDns settings
            Just ss -> let ls  = toList ss
                           len = length ls
                  in newListArray (0, len - 1) ls

--------------------------------------------------------------------------------
resolveDns :: ClusterSettings -> IO (IOArray Int GossipSeed)
resolveDns ClusterSettings{..} = do
    let timeoutMicros = totalMillis clusterGossipTimeout * 1000
        conf =
            case clusterDnsServer of
                Nothing  -> defaultResolvConf
                Just tpe ->
                    let rc =
                            case tpe of
                                DnsFilePath p   -> RCFilePath p
                                DnsHostName h   -> RCHostName h
                                DnsHostPort h p -> RCHostPort h (fromIntegral p)
                    in defaultResolvConf { resolvInfo = rc  }
    dnsSeed <- makeResolvSeed conf
               { resolvTimeout = truncate timeoutMicros
               , resolvRetry   = clusterMaxDiscoverAttempts
               }
    withResolver dnsSeed $ \resv -> do
        result <- lookupA resv clusterDns
        case result of
            Left e    -> throwIO $ DNSDiscoveryError e
            Right ips -> do
                let len = length ips - 1
                arr <- newArray_ (0, len)
                forM_ (zip [0..] ips) $ \(idx, ip) -> do
                    let end  = EndPoint (show ip) clusterExternalGossipPort
                        seed = GossipSeed end ""
                    writeArray arr idx seed
                return arr

--------------------------------------------------------------------------------
shuffleAll :: IOArray Int a -> IO ()
shuffleAll arr = do
    (low, hig) <- getBounds arr
    shuffle arr low hig

--------------------------------------------------------------------------------
shuffle :: IOArray Int a -> Int -> Int -> IO ()
shuffle arr from to = forRange_ from to $ \cur -> do
    idx   <- randomRIO (cur, to)
    tmp   <- readArray arr idx
    value <- readArray arr cur
    writeArray arr idx value
    writeArray arr cur tmp

--------------------------------------------------------------------------------
forRange_ :: Int -> Int -> (Int -> IO ()) -> IO ()
forRange_ from to k = do
    when (from <= to) $ loop (to + 1) from
  where
    loop len cur
        | len == cur = return ()
        | otherwise  = do
              k cur
              loop len (cur + 1)

--------------------------------------------------------------------------------
forArrayFirst :: IOArray Int a
              -> (Int -> IO (Maybe b))
              -> IO (Maybe b)
forArrayFirst arr k = do
    (low, hig) <- getBounds arr
    forRangeFirst low hig k

--------------------------------------------------------------------------------
forRangeFirst :: Int
              -> Int
              -> (Int -> IO (Maybe b))
              -> IO (Maybe b)
forRangeFirst from to k = do
    if from <= to then loop (to + 1) from else return Nothing
  where
    loop len cur
        | len == cur = return Nothing
        | otherwise  = do
              res <- k cur
              if isJust res then return res else loop len (cur + 1)
