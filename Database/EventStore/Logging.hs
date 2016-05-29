--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Logging
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Logging
    ( Log(..)
    , ErrorMessage(..)
    , InfoMessage(..)
    ) where

--------------------------------------------------------------------------------
import Control.Exception
import Data.Word
import Numeric

--------------------------------------------------------------------------------
import Data.UUID

--------------------------------------------------------------------------------
-- | Logging main data structure.
data Log
    = Error ErrorMessage
    | Info InfoMessage
    deriving Show

--------------------------------------------------------------------------------
-- | Classifies error-like log messages.
data ErrorMessage = UnexpectedException SomeException deriving Show

--------------------------------------------------------------------------------
-- | Classifies info-like log messages.
data InfoMessage
    = Connecting Int
      -- ^ Indicates current attempt.
    | ConnectionClosed UUID
      -- ^ Indicates connection 'UUID'.
    | Connected UUID
      -- ^ Indicates connection 'UUID'.
    | Disconnected UUID
      -- ^ Indicates connection 'UUID'
    | PackageSent Word8 UUID
      -- ^ Indicates a package has been sent.
    | PackageReceived Word8 UUID
      -- ^ Indicates the client's received a package from the server.

--------------------------------------------------------------------------------
instance Show InfoMessage where
    show (Connecting i) =
        "Connexion attempt n°" ++ show i
    show (ConnectionClosed u) =
        "Connection [" ++ show u ++ "] closed"
    show (Connected u) =
        "Connected [" ++ show u ++ "]"
    show (Disconnected u) =
        "Disconnected [" ++ show u ++ "]"
    show (PackageSent cmd u)  =
        "Package send 0x" ++ padding (showHex cmd "") ++ " [" ++ show u ++ "]"
    show (PackageReceived cmd u) =
        "Package received 0x" ++
        padding (showHex cmd "") ++ " [" ++ show u ++ "]"

--------------------------------------------------------------------------------
padding :: String -> String
padding [x] = ['0',x]
padding xs  = xs
