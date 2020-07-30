{- This file was auto-generated from cluster.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Cluster_Fields where
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
address ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "address" a) =>
  Lens.Family2.LensLike' f s a
address = Data.ProtoLens.Field.field @"address"
attemptedView ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "attemptedView" a) =>
  Lens.Family2.LensLike' f s a
attemptedView = Data.ProtoLens.Field.field @"attemptedView"
chaserCheckpoint ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "chaserCheckpoint" a) =>
  Lens.Family2.LensLike' f s a
chaserCheckpoint = Data.ProtoLens.Field.field @"chaserCheckpoint"
clusterInfo ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "clusterInfo" a) =>
  Lens.Family2.LensLike' f s a
clusterInfo = Data.ProtoLens.Field.field @"clusterInfo"
epochId ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "epochId" a) =>
  Lens.Family2.LensLike' f s a
epochId = Data.ProtoLens.Field.field @"epochId"
epochLeaderInstanceId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "epochLeaderInstanceId" a) =>
  Lens.Family2.LensLike' f s a
epochLeaderInstanceId
  = Data.ProtoLens.Field.field @"epochLeaderInstanceId"
epochNumber ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "epochNumber" a) =>
  Lens.Family2.LensLike' f s a
epochNumber = Data.ProtoLens.Field.field @"epochNumber"
epochPosition ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "epochPosition" a) =>
  Lens.Family2.LensLike' f s a
epochPosition = Data.ProtoLens.Field.field @"epochPosition"
externalTcp ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "externalTcp" a) =>
  Lens.Family2.LensLike' f s a
externalTcp = Data.ProtoLens.Field.field @"externalTcp"
externalTcpUsesTls ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "externalTcpUsesTls" a) =>
  Lens.Family2.LensLike' f s a
externalTcpUsesTls
  = Data.ProtoLens.Field.field @"externalTcpUsesTls"
httpEndPoint ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "httpEndPoint" a) =>
  Lens.Family2.LensLike' f s a
httpEndPoint = Data.ProtoLens.Field.field @"httpEndPoint"
info ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "info" a) =>
  Lens.Family2.LensLike' f s a
info = Data.ProtoLens.Field.field @"info"
installedView ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "installedView" a) =>
  Lens.Family2.LensLike' f s a
installedView = Data.ProtoLens.Field.field @"installedView"
instanceId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "instanceId" a) =>
  Lens.Family2.LensLike' f s a
instanceId = Data.ProtoLens.Field.field @"instanceId"
internalTcp ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "internalTcp" a) =>
  Lens.Family2.LensLike' f s a
internalTcp = Data.ProtoLens.Field.field @"internalTcp"
internalTcpUsesTls ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "internalTcpUsesTls" a) =>
  Lens.Family2.LensLike' f s a
internalTcpUsesTls
  = Data.ProtoLens.Field.field @"internalTcpUsesTls"
isAlive ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "isAlive" a) =>
  Lens.Family2.LensLike' f s a
isAlive = Data.ProtoLens.Field.field @"isAlive"
isReadOnlyReplica ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "isReadOnlyReplica" a) =>
  Lens.Family2.LensLike' f s a
isReadOnlyReplica = Data.ProtoLens.Field.field @"isReadOnlyReplica"
lastCommitPosition ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "lastCommitPosition" a) =>
  Lens.Family2.LensLike' f s a
lastCommitPosition
  = Data.ProtoLens.Field.field @"lastCommitPosition"
leaderHttp ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "leaderHttp" a) =>
  Lens.Family2.LensLike' f s a
leaderHttp = Data.ProtoLens.Field.field @"leaderHttp"
leaderId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "leaderId" a) =>
  Lens.Family2.LensLike' f s a
leaderId = Data.ProtoLens.Field.field @"leaderId"
maybe'clusterInfo ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'clusterInfo" a) =>
  Lens.Family2.LensLike' f s a
maybe'clusterInfo = Data.ProtoLens.Field.field @"maybe'clusterInfo"
maybe'epochId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'epochId" a) =>
  Lens.Family2.LensLike' f s a
maybe'epochId = Data.ProtoLens.Field.field @"maybe'epochId"
maybe'epochLeaderInstanceId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'epochLeaderInstanceId" a) =>
  Lens.Family2.LensLike' f s a
maybe'epochLeaderInstanceId
  = Data.ProtoLens.Field.field @"maybe'epochLeaderInstanceId"
maybe'externalTcp ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'externalTcp" a) =>
  Lens.Family2.LensLike' f s a
maybe'externalTcp = Data.ProtoLens.Field.field @"maybe'externalTcp"
maybe'httpEndPoint ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'httpEndPoint" a) =>
  Lens.Family2.LensLike' f s a
maybe'httpEndPoint
  = Data.ProtoLens.Field.field @"maybe'httpEndPoint"
maybe'info ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'info" a) =>
  Lens.Family2.LensLike' f s a
maybe'info = Data.ProtoLens.Field.field @"maybe'info"
maybe'instanceId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'instanceId" a) =>
  Lens.Family2.LensLike' f s a
maybe'instanceId = Data.ProtoLens.Field.field @"maybe'instanceId"
maybe'internalTcp ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'internalTcp" a) =>
  Lens.Family2.LensLike' f s a
maybe'internalTcp = Data.ProtoLens.Field.field @"maybe'internalTcp"
maybe'leaderHttp ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'leaderHttp" a) =>
  Lens.Family2.LensLike' f s a
maybe'leaderHttp = Data.ProtoLens.Field.field @"maybe'leaderHttp"
maybe'leaderId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'leaderId" a) =>
  Lens.Family2.LensLike' f s a
maybe'leaderId = Data.ProtoLens.Field.field @"maybe'leaderId"
maybe'server ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'server" a) =>
  Lens.Family2.LensLike' f s a
maybe'server = Data.ProtoLens.Field.field @"maybe'server"
maybe'serverHttp ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'serverHttp" a) =>
  Lens.Family2.LensLike' f s a
maybe'serverHttp = Data.ProtoLens.Field.field @"maybe'serverHttp"
maybe'serverId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'serverId" a) =>
  Lens.Family2.LensLike' f s a
maybe'serverId = Data.ProtoLens.Field.field @"maybe'serverId"
members ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "members" a) =>
  Lens.Family2.LensLike' f s a
members = Data.ProtoLens.Field.field @"members"
nodePriority ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "nodePriority" a) =>
  Lens.Family2.LensLike' f s a
nodePriority = Data.ProtoLens.Field.field @"nodePriority"
port ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "port" a) =>
  Lens.Family2.LensLike' f s a
port = Data.ProtoLens.Field.field @"port"
server ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "server" a) =>
  Lens.Family2.LensLike' f s a
server = Data.ProtoLens.Field.field @"server"
serverHttp ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "serverHttp" a) =>
  Lens.Family2.LensLike' f s a
serverHttp = Data.ProtoLens.Field.field @"serverHttp"
serverId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "serverId" a) =>
  Lens.Family2.LensLike' f s a
serverId = Data.ProtoLens.Field.field @"serverId"
state ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "state" a) =>
  Lens.Family2.LensLike' f s a
state = Data.ProtoLens.Field.field @"state"
timeStamp ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "timeStamp" a) =>
  Lens.Family2.LensLike' f s a
timeStamp = Data.ProtoLens.Field.field @"timeStamp"
vec'members ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'members" a) =>
  Lens.Family2.LensLike' f s a
vec'members = Data.ProtoLens.Field.field @"vec'members"
view ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "view" a) =>
  Lens.Family2.LensLike' f s a
view = Data.ProtoLens.Field.field @"view"
writerCheckpoint ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "writerCheckpoint" a) =>
  Lens.Family2.LensLike' f s a
writerCheckpoint = Data.ProtoLens.Field.field @"writerCheckpoint"