{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}
--------------------------------------------------------------------------------
-- |
-- Module    :  Database.EventStore.Internal.Operation.Identify
-- Copyright :  (C) 2017 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.Identify
  ( newRequest
  , newIdentifyPkg
  ) where

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Serialize (runPut)

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Request =
  Request { _version :: Required 1 (Value Int32)
          , _name    :: Optional 2 (Value Text)
          } deriving (Generic, Show)

--------------------------------------------------------------------------------
newRequest :: Int32 -> Text -> Request
newRequest ver name =
  Request { _version = putField ver
          , _name    = putField $ Just name
          }

--------------------------------------------------------------------------------
newIdentifyPkg :: MonadBase IO m => Int32 -> Text -> m Package
newIdentifyPkg version name = do
  uuid <- newUUID
  let msg = newRequest version name
      pkg = Package { packageCmd         = identifyClientCmd
                    , packageCorrelation = uuid
                    , packageData        = runPut $ encodeMessage msg
                    , packageCred        = Nothing
                    }

  pure pkg

--------------------------------------------------------------------------------
instance Encode Request

