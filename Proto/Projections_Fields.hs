{- This file was auto-generated from projections.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Projections_Fields where
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
import qualified Proto.Google.Protobuf.Struct
import qualified Proto.Shared
all ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "all" a) =>
  Lens.Family2.LensLike' f s a
all = Data.ProtoLens.Field.field @"all"
bufferedEvents ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "bufferedEvents" a) =>
  Lens.Family2.LensLike' f s a
bufferedEvents = Data.ProtoLens.Field.field @"bufferedEvents"
checkpointStatus ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "checkpointStatus" a) =>
  Lens.Family2.LensLike' f s a
checkpointStatus = Data.ProtoLens.Field.field @"checkpointStatus"
continuous ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "continuous" a) =>
  Lens.Family2.LensLike' f s a
continuous = Data.ProtoLens.Field.field @"continuous"
coreProcessingTime ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "coreProcessingTime" a) =>
  Lens.Family2.LensLike' f s a
coreProcessingTime
  = Data.ProtoLens.Field.field @"coreProcessingTime"
deleteCheckpointStream ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "deleteCheckpointStream" a) =>
  Lens.Family2.LensLike' f s a
deleteCheckpointStream
  = Data.ProtoLens.Field.field @"deleteCheckpointStream"
deleteEmittedStreams ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "deleteEmittedStreams" a) =>
  Lens.Family2.LensLike' f s a
deleteEmittedStreams
  = Data.ProtoLens.Field.field @"deleteEmittedStreams"
deleteStateStream ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "deleteStateStream" a) =>
  Lens.Family2.LensLike' f s a
deleteStateStream = Data.ProtoLens.Field.field @"deleteStateStream"
details ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "details" a) =>
  Lens.Family2.LensLike' f s a
details = Data.ProtoLens.Field.field @"details"
effectiveName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "effectiveName" a) =>
  Lens.Family2.LensLike' f s a
effectiveName = Data.ProtoLens.Field.field @"effectiveName"
emitEnabled ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "emitEnabled" a) =>
  Lens.Family2.LensLike' f s a
emitEnabled = Data.ProtoLens.Field.field @"emitEnabled"
epoch ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "epoch" a) =>
  Lens.Family2.LensLike' f s a
epoch = Data.ProtoLens.Field.field @"epoch"
eventsProcessedAfterRestart ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "eventsProcessedAfterRestart" a) =>
  Lens.Family2.LensLike' f s a
eventsProcessedAfterRestart
  = Data.ProtoLens.Field.field @"eventsProcessedAfterRestart"
lastCheckpoint ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "lastCheckpoint" a) =>
  Lens.Family2.LensLike' f s a
lastCheckpoint = Data.ProtoLens.Field.field @"lastCheckpoint"
maybe'all ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'all" a) =>
  Lens.Family2.LensLike' f s a
maybe'all = Data.ProtoLens.Field.field @"maybe'all"
maybe'continuous ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'continuous" a) =>
  Lens.Family2.LensLike' f s a
maybe'continuous = Data.ProtoLens.Field.field @"maybe'continuous"
maybe'details ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'details" a) =>
  Lens.Family2.LensLike' f s a
maybe'details = Data.ProtoLens.Field.field @"maybe'details"
maybe'emitEnabled ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'emitEnabled" a) =>
  Lens.Family2.LensLike' f s a
maybe'emitEnabled = Data.ProtoLens.Field.field @"maybe'emitEnabled"
maybe'emitOption ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'emitOption" a) =>
  Lens.Family2.LensLike' f s a
maybe'emitOption = Data.ProtoLens.Field.field @"maybe'emitOption"
maybe'mode ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'mode" a) =>
  Lens.Family2.LensLike' f s a
maybe'mode = Data.ProtoLens.Field.field @"maybe'mode"
maybe'name ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'name" a) =>
  Lens.Family2.LensLike' f s a
maybe'name = Data.ProtoLens.Field.field @"maybe'name"
maybe'noEmitOptions ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'noEmitOptions" a) =>
  Lens.Family2.LensLike' f s a
maybe'noEmitOptions
  = Data.ProtoLens.Field.field @"maybe'noEmitOptions"
maybe'oneTime ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'oneTime" a) =>
  Lens.Family2.LensLike' f s a
maybe'oneTime = Data.ProtoLens.Field.field @"maybe'oneTime"
maybe'options ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'options" a) =>
  Lens.Family2.LensLike' f s a
maybe'options = Data.ProtoLens.Field.field @"maybe'options"
maybe'result ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'result" a) =>
  Lens.Family2.LensLike' f s a
maybe'result = Data.ProtoLens.Field.field @"maybe'result"
maybe'state ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'state" a) =>
  Lens.Family2.LensLike' f s a
maybe'state = Data.ProtoLens.Field.field @"maybe'state"
maybe'transient ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'transient" a) =>
  Lens.Family2.LensLike' f s a
maybe'transient = Data.ProtoLens.Field.field @"maybe'transient"
mode ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "mode" a) =>
  Lens.Family2.LensLike' f s a
mode = Data.ProtoLens.Field.field @"mode"
name ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "name" a) =>
  Lens.Family2.LensLike' f s a
name = Data.ProtoLens.Field.field @"name"
noEmitOptions ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "noEmitOptions" a) =>
  Lens.Family2.LensLike' f s a
noEmitOptions = Data.ProtoLens.Field.field @"noEmitOptions"
oneTime ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "oneTime" a) =>
  Lens.Family2.LensLike' f s a
oneTime = Data.ProtoLens.Field.field @"oneTime"
options ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "options" a) =>
  Lens.Family2.LensLike' f s a
options = Data.ProtoLens.Field.field @"options"
partition ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "partition" a) =>
  Lens.Family2.LensLike' f s a
partition = Data.ProtoLens.Field.field @"partition"
partitionsCached ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "partitionsCached" a) =>
  Lens.Family2.LensLike' f s a
partitionsCached = Data.ProtoLens.Field.field @"partitionsCached"
position ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "position" a) =>
  Lens.Family2.LensLike' f s a
position = Data.ProtoLens.Field.field @"position"
progress ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "progress" a) =>
  Lens.Family2.LensLike' f s a
progress = Data.ProtoLens.Field.field @"progress"
query ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "query" a) =>
  Lens.Family2.LensLike' f s a
query = Data.ProtoLens.Field.field @"query"
readsInProgress ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "readsInProgress" a) =>
  Lens.Family2.LensLike' f s a
readsInProgress = Data.ProtoLens.Field.field @"readsInProgress"
result ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "result" a) =>
  Lens.Family2.LensLike' f s a
result = Data.ProtoLens.Field.field @"result"
state ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "state" a) =>
  Lens.Family2.LensLike' f s a
state = Data.ProtoLens.Field.field @"state"
stateReason ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "stateReason" a) =>
  Lens.Family2.LensLike' f s a
stateReason = Data.ProtoLens.Field.field @"stateReason"
status ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "status" a) =>
  Lens.Family2.LensLike' f s a
status = Data.ProtoLens.Field.field @"status"
trackEmittedStreams ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "trackEmittedStreams" a) =>
  Lens.Family2.LensLike' f s a
trackEmittedStreams
  = Data.ProtoLens.Field.field @"trackEmittedStreams"
transient ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "transient" a) =>
  Lens.Family2.LensLike' f s a
transient = Data.ProtoLens.Field.field @"transient"
version ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "version" a) =>
  Lens.Family2.LensLike' f s a
version = Data.ProtoLens.Field.field @"version"
writeCheckpoint ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "writeCheckpoint" a) =>
  Lens.Family2.LensLike' f s a
writeCheckpoint = Data.ProtoLens.Field.field @"writeCheckpoint"
writePendingEventsAfterCheckpoint ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "writePendingEventsAfterCheckpoint" a) =>
  Lens.Family2.LensLike' f s a
writePendingEventsAfterCheckpoint
  = Data.ProtoLens.Field.field @"writePendingEventsAfterCheckpoint"
writePendingEventsBeforeCheckpoint ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "writePendingEventsBeforeCheckpoint" a) =>
  Lens.Family2.LensLike' f s a
writePendingEventsBeforeCheckpoint
  = Data.ProtoLens.Field.field @"writePendingEventsBeforeCheckpoint"
writesInProgress ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "writesInProgress" a) =>
  Lens.Family2.LensLike' f s a
writesInProgress = Data.ProtoLens.Field.field @"writesInProgress"