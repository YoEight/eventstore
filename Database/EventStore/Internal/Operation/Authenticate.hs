--------------------------------------------------------------------------------
-- |
-- Module    :  Database.EventStore.Internal.Operation.Authenticate
-- Copyright :  (C) 2018 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.Authenticate
  ( newAuthenticatePkg ) where

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Settings
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
newAuthenticatePkg :: MonadBase IO m => Credentials -> m Package
newAuthenticatePkg cred = do
  uuid <- newUUID
  let pkg = Package { packageCmd         = authenticateCmd
                    , packageCorrelation = uuid
                    , packageData        = ""
                    , packageCred        = Just cred
                    }
  pure pkg
