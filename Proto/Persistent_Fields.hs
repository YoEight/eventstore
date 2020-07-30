{- This file was auto-generated from persistent.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Persistent_Fields where
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Monoid as Data.Monoid
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens as Data.ProtoLens
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Bytes as Data.ProtoLens.Encoding.Bytes
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Growing as Data.ProtoLens.Encoding.Growing
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Parser.Unsafe as Data.ProtoLens.Encoding.Parser.Unsafe
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Wire as Data.ProtoLens.Encoding.Wire
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Field as Data.ProtoLens.Field
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum as Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Runtime.Lens.Family2 as Lens.Family2
import qualified Data.ProtoLens.Runtime.Lens.Family2.Unchecked as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Runtime.Data.Text as Data.Text
import qualified Data.ProtoLens.Runtime.Data.Map as Data.Map
import qualified Data.ProtoLens.Runtime.Data.ByteString as Data.ByteString
import qualified Data.ProtoLens.Runtime.Data.ByteString.Char8 as Data.ByteString.Char8
import qualified Data.ProtoLens.Runtime.Data.Text.Encoding as Data.Text.Encoding
import qualified Data.ProtoLens.Runtime.Data.Vector as Data.Vector
import qualified Data.ProtoLens.Runtime.Data.Vector.Generic as Data.Vector.Generic
import qualified Data.ProtoLens.Runtime.Data.Vector.Unboxed as Data.Vector.Unboxed
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read
import qualified Proto.Shared
ack ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "ack" a) =>
  Lens.Family2.LensLike' f s a
ack = Data.ProtoLens.Field.field @"ack"
action ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "action" a) =>
  Lens.Family2.LensLike' f s a
action = Data.ProtoLens.Field.field @"action"
bufferSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "bufferSize" a) =>
  Lens.Family2.LensLike' f s a
bufferSize = Data.ProtoLens.Field.field @"bufferSize"
checkpointAfter ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "checkpointAfter" a) =>
  Lens.Family2.LensLike' f s a
checkpointAfter = Data.ProtoLens.Field.field @"checkpointAfter"
commitPosition ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "commitPosition" a) =>
  Lens.Family2.LensLike' f s a
commitPosition = Data.ProtoLens.Field.field @"commitPosition"
customMetadata ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "customMetadata" a) =>
  Lens.Family2.LensLike' f s a
customMetadata = Data.ProtoLens.Field.field @"customMetadata"
data' ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "data'" a) =>
  Lens.Family2.LensLike' f s a
data' = Data.ProtoLens.Field.field @"data'"
event ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "event" a) =>
  Lens.Family2.LensLike' f s a
event = Data.ProtoLens.Field.field @"event"
extraStatistics ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "extraStatistics" a) =>
  Lens.Family2.LensLike' f s a
extraStatistics = Data.ProtoLens.Field.field @"extraStatistics"
groupName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "groupName" a) =>
  Lens.Family2.LensLike' f s a
groupName = Data.ProtoLens.Field.field @"groupName"
historyBufferSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "historyBufferSize" a) =>
  Lens.Family2.LensLike' f s a
historyBufferSize = Data.ProtoLens.Field.field @"historyBufferSize"
id ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "id" a) =>
  Lens.Family2.LensLike' f s a
id = Data.ProtoLens.Field.field @"id"
ids ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "ids" a) =>
  Lens.Family2.LensLike' f s a
ids = Data.ProtoLens.Field.field @"ids"
key ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "key" a) =>
  Lens.Family2.LensLike' f s a
key = Data.ProtoLens.Field.field @"key"
link ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "link" a) =>
  Lens.Family2.LensLike' f s a
link = Data.ProtoLens.Field.field @"link"
liveBufferSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "liveBufferSize" a) =>
  Lens.Family2.LensLike' f s a
liveBufferSize = Data.ProtoLens.Field.field @"liveBufferSize"
maxCheckpointCount ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxCheckpointCount" a) =>
  Lens.Family2.LensLike' f s a
maxCheckpointCount
  = Data.ProtoLens.Field.field @"maxCheckpointCount"
maxRetryCount ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxRetryCount" a) =>
  Lens.Family2.LensLike' f s a
maxRetryCount = Data.ProtoLens.Field.field @"maxRetryCount"
maxSubscriberCount ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxSubscriberCount" a) =>
  Lens.Family2.LensLike' f s a
maxSubscriberCount
  = Data.ProtoLens.Field.field @"maxSubscriberCount"
maybe'ack ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'ack" a) =>
  Lens.Family2.LensLike' f s a
maybe'ack = Data.ProtoLens.Field.field @"maybe'ack"
maybe'commitPosition ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'commitPosition" a) =>
  Lens.Family2.LensLike' f s a
maybe'commitPosition
  = Data.ProtoLens.Field.field @"maybe'commitPosition"
maybe'content ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'content" a) =>
  Lens.Family2.LensLike' f s a
maybe'content = Data.ProtoLens.Field.field @"maybe'content"
maybe'count ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'count" a) =>
  Lens.Family2.LensLike' f s a
maybe'count = Data.ProtoLens.Field.field @"maybe'count"
maybe'event ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'event" a) =>
  Lens.Family2.LensLike' f s a
maybe'event = Data.ProtoLens.Field.field @"maybe'event"
maybe'id ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'id" a) =>
  Lens.Family2.LensLike' f s a
maybe'id = Data.ProtoLens.Field.field @"maybe'id"
maybe'link ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'link" a) =>
  Lens.Family2.LensLike' f s a
maybe'link = Data.ProtoLens.Field.field @"maybe'link"
maybe'nack ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'nack" a) =>
  Lens.Family2.LensLike' f s a
maybe'nack = Data.ProtoLens.Field.field @"maybe'nack"
maybe'noPosition ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'noPosition" a) =>
  Lens.Family2.LensLike' f s a
maybe'noPosition = Data.ProtoLens.Field.field @"maybe'noPosition"
maybe'noRetryCount ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'noRetryCount" a) =>
  Lens.Family2.LensLike' f s a
maybe'noRetryCount
  = Data.ProtoLens.Field.field @"maybe'noRetryCount"
maybe'options ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'options" a) =>
  Lens.Family2.LensLike' f s a
maybe'options = Data.ProtoLens.Field.field @"maybe'options"
maybe'position ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'position" a) =>
  Lens.Family2.LensLike' f s a
maybe'position = Data.ProtoLens.Field.field @"maybe'position"
maybe'retryCount ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'retryCount" a) =>
  Lens.Family2.LensLike' f s a
maybe'retryCount = Data.ProtoLens.Field.field @"maybe'retryCount"
maybe'settings ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'settings" a) =>
  Lens.Family2.LensLike' f s a
maybe'settings = Data.ProtoLens.Field.field @"maybe'settings"
maybe'streamIdentifier ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'streamIdentifier" a) =>
  Lens.Family2.LensLike' f s a
maybe'streamIdentifier
  = Data.ProtoLens.Field.field @"maybe'streamIdentifier"
maybe'string ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'string" a) =>
  Lens.Family2.LensLike' f s a
maybe'string = Data.ProtoLens.Field.field @"maybe'string"
maybe'structured ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'structured" a) =>
  Lens.Family2.LensLike' f s a
maybe'structured = Data.ProtoLens.Field.field @"maybe'structured"
maybe'subscriptionConfirmation ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'subscriptionConfirmation" a) =>
  Lens.Family2.LensLike' f s a
maybe'subscriptionConfirmation
  = Data.ProtoLens.Field.field @"maybe'subscriptionConfirmation"
maybe'uuidOption ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'uuidOption" a) =>
  Lens.Family2.LensLike' f s a
maybe'uuidOption = Data.ProtoLens.Field.field @"maybe'uuidOption"
messageTimeout ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "messageTimeout" a) =>
  Lens.Family2.LensLike' f s a
messageTimeout = Data.ProtoLens.Field.field @"messageTimeout"
metadata ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "metadata" a) =>
  Lens.Family2.LensLike' f s a
metadata = Data.ProtoLens.Field.field @"metadata"
minCheckpointCount ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "minCheckpointCount" a) =>
  Lens.Family2.LensLike' f s a
minCheckpointCount
  = Data.ProtoLens.Field.field @"minCheckpointCount"
nack ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "nack" a) =>
  Lens.Family2.LensLike' f s a
nack = Data.ProtoLens.Field.field @"nack"
namedConsumerStrategy ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "namedConsumerStrategy" a) =>
  Lens.Family2.LensLike' f s a
namedConsumerStrategy
  = Data.ProtoLens.Field.field @"namedConsumerStrategy"
noPosition ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "noPosition" a) =>
  Lens.Family2.LensLike' f s a
noPosition = Data.ProtoLens.Field.field @"noPosition"
noRetryCount ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "noRetryCount" a) =>
  Lens.Family2.LensLike' f s a
noRetryCount = Data.ProtoLens.Field.field @"noRetryCount"
options ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "options" a) =>
  Lens.Family2.LensLike' f s a
options = Data.ProtoLens.Field.field @"options"
preparePosition ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "preparePosition" a) =>
  Lens.Family2.LensLike' f s a
preparePosition = Data.ProtoLens.Field.field @"preparePosition"
readBatchSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "readBatchSize" a) =>
  Lens.Family2.LensLike' f s a
readBatchSize = Data.ProtoLens.Field.field @"readBatchSize"
reason ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "reason" a) =>
  Lens.Family2.LensLike' f s a
reason = Data.ProtoLens.Field.field @"reason"
resolveLinks ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "resolveLinks" a) =>
  Lens.Family2.LensLike' f s a
resolveLinks = Data.ProtoLens.Field.field @"resolveLinks"
retryCount ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "retryCount" a) =>
  Lens.Family2.LensLike' f s a
retryCount = Data.ProtoLens.Field.field @"retryCount"
revision ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "revision" a) =>
  Lens.Family2.LensLike' f s a
revision = Data.ProtoLens.Field.field @"revision"
settings ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "settings" a) =>
  Lens.Family2.LensLike' f s a
settings = Data.ProtoLens.Field.field @"settings"
streamIdentifier ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "streamIdentifier" a) =>
  Lens.Family2.LensLike' f s a
streamIdentifier = Data.ProtoLens.Field.field @"streamIdentifier"
streamRevision ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "streamRevision" a) =>
  Lens.Family2.LensLike' f s a
streamRevision = Data.ProtoLens.Field.field @"streamRevision"
string ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "string" a) =>
  Lens.Family2.LensLike' f s a
string = Data.ProtoLens.Field.field @"string"
structured ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "structured" a) =>
  Lens.Family2.LensLike' f s a
structured = Data.ProtoLens.Field.field @"structured"
subscriptionConfirmation ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "subscriptionConfirmation" a) =>
  Lens.Family2.LensLike' f s a
subscriptionConfirmation
  = Data.ProtoLens.Field.field @"subscriptionConfirmation"
subscriptionId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "subscriptionId" a) =>
  Lens.Family2.LensLike' f s a
subscriptionId = Data.ProtoLens.Field.field @"subscriptionId"
uuidOption ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "uuidOption" a) =>
  Lens.Family2.LensLike' f s a
uuidOption = Data.ProtoLens.Field.field @"uuidOption"
value ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "value" a) =>
  Lens.Family2.LensLike' f s a
value = Data.ProtoLens.Field.field @"value"
vec'ids ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "vec'ids" a) =>
  Lens.Family2.LensLike' f s a
vec'ids = Data.ProtoLens.Field.field @"vec'ids"