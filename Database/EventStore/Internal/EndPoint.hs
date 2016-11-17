--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.EndPoint
-- Copyright : (C) 2016 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.EndPoint where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
-- | Gathers both an IPv4 and a port.
data EndPoint =
    EndPoint
    { endPointIp   :: !String
    , endPointPort :: !Int
    } deriving Show

--------------------------------------------------------------------------------
emptyEndPoint :: EndPoint
emptyEndPoint = EndPoint "" 0

--------------------------------------------------------------------------------
data NodeEndPoints =
    NodeEndPoints
    { tcpEndPoint :: !EndPoint
    , secureEndPoint :: !(Maybe EndPoint)
    } deriving Show
