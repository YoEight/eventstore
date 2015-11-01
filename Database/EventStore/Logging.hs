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
module Database.EventStore.Logging where

--------------------------------------------------------------------------------
import Data.Word

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
data ErrorMessage
    = MaxAttemptConnectionReached Int
      -- ^ Indicates max attempt value.
    | WrongPackageFraming
    | PackageParsingError String
      -- ^ Indicates parsing error message.
    deriving Show

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
    deriving Show
