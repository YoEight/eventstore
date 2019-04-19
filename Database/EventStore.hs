--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore
-- Copyright : (C) 2019 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore
    ( -- * Connection
      Connection
    , ConnectionType(..)
    , Credentials
    , Settings(..)
    , Retry
    , atMost
    , keepRetrying
    , credentials
    , defaultSettings
    , defaultSSLSettings
    , connect
    , shutdown
    , waitTillClosed
    , connectionSettings
      -- * Cluster Connection
    , ClusterSettings(..)
    , DnsServer(..)
    , GossipSeed
    , gossipSeed
    , gossipSeedWithHeader
    , gossipSeedHost
    , gossipSeedHeader
    , gossipSeedPort
    , gossipSeedClusterSettings
    , dnsClusterSettings
      -- * Event
    , Event
    , EventData
    , EventType(..)
    , createEvent
    , withJson
    , withJsonAndMetadata
    , withBinary
    , withBinaryAndMetadata
     -- * Event number
    , EventNumber
    , streamStart
    , streamEnd
    , eventNumber
    , rawEventNumber
    , eventNumberToInt64
     -- * Common Operation types
    , OperationMaxAttemptReached(..)
     -- * Read Operations
    , StreamMetadataResult(..)
    , BatchResult
    , ResolveLink(..)
    , readEvent
    , readEventsBackward
    , readEventsForward
    , getStreamMetadata
      -- * Write Operations
    , StreamACL(..)
    , StreamMetadata(..)
    , getCustomPropertyValue
    , getCustomProperty
    , emptyStreamACL
    , emptyStreamMetadata
    , deleteStream
    , sendEvent
    , sendEvents
    , setStreamMetadata
      -- * Builder
    , Builder
      -- * Stream ACL Builder
    , StreamACLBuilder
    , buildStreamACL
    , modifyStreamACL
    , setReadRoles
    , setReadRole
    , setWriteRoles
    , setWriteRole
    , setDeleteRoles
    , setDeleteRole
    , setMetaReadRoles
    , setMetaReadRole
    , setMetaWriteRoles
    , setMetaWriteRole
      -- * Stream Metadata Builder
    , StreamMetadataBuilder
    , buildStreamMetadata
    , modifyStreamMetadata
    , setMaxCount
    , setMaxAge
    , setTruncateBefore
    , setCacheControl
    , setACL
    , modifyACL
    , setCustomProperty
      -- * Transaction
    , Transaction
    , TransactionId
    , startTransaction
    , transactionId
    , transactionCommit
    , transactionRollback
    , transactionWrite
      -- * Subscription
    , SubscriptionClosed(..)
    , SubscriptionId
    , SubscriptionStream(..)
    , Subscription
    , SubDropReason(..)
    , SubDetails
    , waitConfirmation
    , unsubscribeConfirmed
    , unsubscribeConfirmedSTM
    , waitUnsubscribeConfirmed
    , nextEventMaybeSTM
    , getSubscriptionDetailsSTM
    , unsubscribe
      -- * Volatile Subscription
    , RegularSubscription
    , subscribe
    , getSubscriptionId
    , nextEvent
    , nextEventMaybe
      -- * Catch-up Subscription
    , CatchupSubscription
    , subscribeFrom
    , waitTillCatchup
    , hasCaughtUp
    , hasCaughtUpSTM
     -- * Persistent Subscription
    , PersistentSubscription
    , PersistentSubscriptionSettings(..)
    , SystemConsumerStrategy(..)
    , NakAction(..)
    , PersistActionException(..)
    , acknowledge
    , acknowledgeEvents
    , failed
    , eventsFailed
    , notifyEventsProcessed
    , notifyEventsFailed
    , defaultPersistentSubscriptionSettings
    , createPersistentSubscription
    , updatePersistentSubscription
    , deletePersistentSubscription
    , connectToPersistentSubscription
     -- * Results
    , Slice(..)
    , sliceEvents
    , sliceEOS
    , sliceNext
    , emptySlice
    , AllSlice
    , Op.DeleteResult(..)
    , WriteResult(..)
    , ReadResult(..)
    , RecordedEvent(..)
    , Op.ReadEvent(..)
    , StreamSlice
    , Position(..)
    , ReadDirection(..)
    , ResolvedEvent(..)
    , OperationError(..)
    , StreamId(..)
    , StreamName
    , isAllStream
    , isEventResolvedLink
    , resolvedEventOriginal
    , resolvedEventDataAsJson
    , resolvedEventOriginalStreamId
    , resolvedEventOriginalId
    , resolvedEventOriginalEventNumber
    , recordedEventDataAsJson
    , positionStart
    , positionEnd
      -- * Logging
    , LogLevel(..)
    , LogType(..)
    , LoggerFilter(..)
      -- * Misc
    , Command
    , DropReason(..)
    , ExpectedVersion
    , anyVersion
    , noStreamVersion
    , emptyStreamVersion
    , exactEventVersion
    , streamExists
    , msDiffTime
      -- * Re-export
    , (<>)
    , NonEmpty(..)
    , nonEmpty
    , TLSSettings
    , NominalDiffTime
    ) where

--------------------------------------------------------------------------------
import Data.List.NonEmpty(NonEmpty(..), nonEmpty)

--------------------------------------------------------------------------------
import Network.Connection (TLSSettings)

--------------------------------------------------------------------------------
import           Database.EventStore.Internal
import           Database.EventStore.Internal.Command
import           Database.EventStore.Internal.Discovery
import           Database.EventStore.Internal.Logger
import           Database.EventStore.Internal.Prelude
import           Database.EventStore.Internal.Operation (OperationError(..))
import qualified Database.EventStore.Internal.Operations as Op
import           Database.EventStore.Internal.Operation.Read.Common
import           Database.EventStore.Internal.Operation.Write.Common
import           Database.EventStore.Internal.Settings
import           Database.EventStore.Internal.Stream
import           Database.EventStore.Internal.Subscription.Api
import           Database.EventStore.Internal.Subscription.Catchup
import           Database.EventStore.Internal.Subscription.Message
import           Database.EventStore.Internal.Subscription.Persistent
import           Database.EventStore.Internal.Subscription.Types
import           Database.EventStore.Internal.Subscription.Regular
import           Database.EventStore.Internal.Manager.Operation.Registry
import           Database.EventStore.Internal.Types
