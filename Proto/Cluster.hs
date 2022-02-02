{- This file was auto-generated from cluster.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Cluster (
        Gossip(..), Elections(..), AcceptRequest(), ClusterInfo(),
        EndPoint(), GossipRequest(), LeaderIsResigningOkRequest(),
        LeaderIsResigningRequest(), MemberInfo(),
        MemberInfo'VNodeState(..), MemberInfo'VNodeState(),
        MemberInfo'VNodeState'UnrecognizedValue, PrepareOkRequest(),
        PrepareRequest(), ProposalRequest(), ViewChangeProofRequest(),
        ViewChangeRequest()
    ) where
import qualified Data.ProtoLens.Runtime.Control.DeepSeq as Control.DeepSeq
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Prism as Data.ProtoLens.Prism
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
{- | Fields :
     
         * 'Proto.Cluster_Fields.serverId' @:: Lens' AcceptRequest Proto.Shared.UUID@
         * 'Proto.Cluster_Fields.maybe'serverId' @:: Lens' AcceptRequest (Prelude.Maybe Proto.Shared.UUID)@
         * 'Proto.Cluster_Fields.serverHttp' @:: Lens' AcceptRequest EndPoint@
         * 'Proto.Cluster_Fields.maybe'serverHttp' @:: Lens' AcceptRequest (Prelude.Maybe EndPoint)@
         * 'Proto.Cluster_Fields.leaderId' @:: Lens' AcceptRequest Proto.Shared.UUID@
         * 'Proto.Cluster_Fields.maybe'leaderId' @:: Lens' AcceptRequest (Prelude.Maybe Proto.Shared.UUID)@
         * 'Proto.Cluster_Fields.leaderHttp' @:: Lens' AcceptRequest EndPoint@
         * 'Proto.Cluster_Fields.maybe'leaderHttp' @:: Lens' AcceptRequest (Prelude.Maybe EndPoint)@
         * 'Proto.Cluster_Fields.view' @:: Lens' AcceptRequest Data.Int.Int32@ -}
data AcceptRequest
  = AcceptRequest'_constructor {_AcceptRequest'serverId :: !(Prelude.Maybe Proto.Shared.UUID),
                                _AcceptRequest'serverHttp :: !(Prelude.Maybe EndPoint),
                                _AcceptRequest'leaderId :: !(Prelude.Maybe Proto.Shared.UUID),
                                _AcceptRequest'leaderHttp :: !(Prelude.Maybe EndPoint),
                                _AcceptRequest'view :: !Data.Int.Int32,
                                _AcceptRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show AcceptRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField AcceptRequest "serverId" Proto.Shared.UUID where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AcceptRequest'serverId
           (\ x__ y__ -> x__ {_AcceptRequest'serverId = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField AcceptRequest "maybe'serverId" (Prelude.Maybe Proto.Shared.UUID) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AcceptRequest'serverId
           (\ x__ y__ -> x__ {_AcceptRequest'serverId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AcceptRequest "serverHttp" EndPoint where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AcceptRequest'serverHttp
           (\ x__ y__ -> x__ {_AcceptRequest'serverHttp = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField AcceptRequest "maybe'serverHttp" (Prelude.Maybe EndPoint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AcceptRequest'serverHttp
           (\ x__ y__ -> x__ {_AcceptRequest'serverHttp = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AcceptRequest "leaderId" Proto.Shared.UUID where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AcceptRequest'leaderId
           (\ x__ y__ -> x__ {_AcceptRequest'leaderId = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField AcceptRequest "maybe'leaderId" (Prelude.Maybe Proto.Shared.UUID) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AcceptRequest'leaderId
           (\ x__ y__ -> x__ {_AcceptRequest'leaderId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AcceptRequest "leaderHttp" EndPoint where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AcceptRequest'leaderHttp
           (\ x__ y__ -> x__ {_AcceptRequest'leaderHttp = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField AcceptRequest "maybe'leaderHttp" (Prelude.Maybe EndPoint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AcceptRequest'leaderHttp
           (\ x__ y__ -> x__ {_AcceptRequest'leaderHttp = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AcceptRequest "view" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AcceptRequest'view (\ x__ y__ -> x__ {_AcceptRequest'view = y__}))
        Prelude.id
instance Data.ProtoLens.Message AcceptRequest where
  messageName _ = Data.Text.pack "event_store.cluster.AcceptRequest"
  packedMessageDescriptor _
    = "\n\
      \\rAcceptRequest\DC2<\n\
      \\tserver_id\CAN\SOH \SOH(\v2\US.event_store.client.shared.UUIDR\bserverId\DC2>\n\
      \\vserver_http\CAN\STX \SOH(\v2\GS.event_store.cluster.EndPointR\n\
      \serverHttp\DC2<\n\
      \\tleader_id\CAN\ETX \SOH(\v2\US.event_store.client.shared.UUIDR\bleaderId\DC2>\n\
      \\vleader_http\CAN\EOT \SOH(\v2\GS.event_store.cluster.EndPointR\n\
      \leaderHttp\DC2\DC2\n\
      \\EOTview\CAN\ENQ \SOH(\ENQR\EOTview"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        serverId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "server_id"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.UUID)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'serverId")) ::
              Data.ProtoLens.FieldDescriptor AcceptRequest
        serverHttp__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "server_http"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor EndPoint)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'serverHttp")) ::
              Data.ProtoLens.FieldDescriptor AcceptRequest
        leaderId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "leader_id"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.UUID)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'leaderId")) ::
              Data.ProtoLens.FieldDescriptor AcceptRequest
        leaderHttp__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "leader_http"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor EndPoint)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'leaderHttp")) ::
              Data.ProtoLens.FieldDescriptor AcceptRequest
        view__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "view"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"view")) ::
              Data.ProtoLens.FieldDescriptor AcceptRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, serverId__field_descriptor),
           (Data.ProtoLens.Tag 2, serverHttp__field_descriptor),
           (Data.ProtoLens.Tag 3, leaderId__field_descriptor),
           (Data.ProtoLens.Tag 4, leaderHttp__field_descriptor),
           (Data.ProtoLens.Tag 5, view__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _AcceptRequest'_unknownFields
        (\ x__ y__ -> x__ {_AcceptRequest'_unknownFields = y__})
  defMessage
    = AcceptRequest'_constructor
        {_AcceptRequest'serverId = Prelude.Nothing,
         _AcceptRequest'serverHttp = Prelude.Nothing,
         _AcceptRequest'leaderId = Prelude.Nothing,
         _AcceptRequest'leaderHttp = Prelude.Nothing,
         _AcceptRequest'view = Data.ProtoLens.fieldDefault,
         _AcceptRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          AcceptRequest -> Data.ProtoLens.Encoding.Bytes.Parser AcceptRequest
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "server_id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"serverId") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "server_http"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"serverHttp") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "leader_id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"leaderId") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "leader_http"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"leaderHttp") y x)
                        40
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "view"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"view") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "AcceptRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'serverId") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage
                          _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'serverHttp") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             _v))
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'leaderId") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just _v)
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage
                                _v))
                   ((Data.Monoid.<>)
                      (case
                           Lens.Family2.view
                             (Data.ProtoLens.Field.field @"maybe'leaderHttp") _x
                       of
                         Prelude.Nothing -> Data.Monoid.mempty
                         (Prelude.Just _v)
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                ((Prelude..)
                                   (\ bs
                                      -> (Data.Monoid.<>)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                                           (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                   Data.ProtoLens.encodeMessage
                                   _v))
                      ((Data.Monoid.<>)
                         (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"view") _x
                          in
                            if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                Data.Monoid.mempty
                            else
                                (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 40)
                                  ((Prelude..)
                                     Data.ProtoLens.Encoding.Bytes.putVarInt
                                     Prelude.fromIntegral
                                     _v))
                         (Data.ProtoLens.Encoding.Wire.buildFieldSet
                            (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))
instance Control.DeepSeq.NFData AcceptRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_AcceptRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_AcceptRequest'serverId x__)
                (Control.DeepSeq.deepseq
                   (_AcceptRequest'serverHttp x__)
                   (Control.DeepSeq.deepseq
                      (_AcceptRequest'leaderId x__)
                      (Control.DeepSeq.deepseq
                         (_AcceptRequest'leaderHttp x__)
                         (Control.DeepSeq.deepseq (_AcceptRequest'view x__) ())))))
{- | Fields :
     
         * 'Proto.Cluster_Fields.members' @:: Lens' ClusterInfo [MemberInfo]@
         * 'Proto.Cluster_Fields.vec'members' @:: Lens' ClusterInfo (Data.Vector.Vector MemberInfo)@ -}
data ClusterInfo
  = ClusterInfo'_constructor {_ClusterInfo'members :: !(Data.Vector.Vector MemberInfo),
                              _ClusterInfo'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ClusterInfo where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ClusterInfo "members" [MemberInfo] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ClusterInfo'members
           (\ x__ y__ -> x__ {_ClusterInfo'members = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ClusterInfo "vec'members" (Data.Vector.Vector MemberInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ClusterInfo'members
           (\ x__ y__ -> x__ {_ClusterInfo'members = y__}))
        Prelude.id
instance Data.ProtoLens.Message ClusterInfo where
  messageName _ = Data.Text.pack "event_store.cluster.ClusterInfo"
  packedMessageDescriptor _
    = "\n\
      \\vClusterInfo\DC29\n\
      \\amembers\CAN\SOH \ETX(\v2\US.event_store.cluster.MemberInfoR\amembers"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        members__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "members"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor MemberInfo)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"members")) ::
              Data.ProtoLens.FieldDescriptor ClusterInfo
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, members__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ClusterInfo'_unknownFields
        (\ x__ y__ -> x__ {_ClusterInfo'_unknownFields = y__})
  defMessage
    = ClusterInfo'_constructor
        {_ClusterInfo'members = Data.Vector.Generic.empty,
         _ClusterInfo'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ClusterInfo
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld MemberInfo
             -> Data.ProtoLens.Encoding.Bytes.Parser ClusterInfo
        loop x mutable'members
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'members <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                          (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                             mutable'members)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields
                           (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'members") frozen'members x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "members"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'members y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'members
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'members <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                   Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'members)
          "ClusterInfo"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.ProtoLens.encodeMessage
                           _v))
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'members") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ClusterInfo where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ClusterInfo'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ClusterInfo'members x__) ())
{- | Fields :
     
         * 'Proto.Cluster_Fields.address' @:: Lens' EndPoint Data.Text.Text@
         * 'Proto.Cluster_Fields.port' @:: Lens' EndPoint Data.Word.Word32@ -}
data EndPoint
  = EndPoint'_constructor {_EndPoint'address :: !Data.Text.Text,
                           _EndPoint'port :: !Data.Word.Word32,
                           _EndPoint'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show EndPoint where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField EndPoint "address" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _EndPoint'address (\ x__ y__ -> x__ {_EndPoint'address = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField EndPoint "port" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _EndPoint'port (\ x__ y__ -> x__ {_EndPoint'port = y__}))
        Prelude.id
instance Data.ProtoLens.Message EndPoint where
  messageName _ = Data.Text.pack "event_store.cluster.EndPoint"
  packedMessageDescriptor _
    = "\n\
      \\bEndPoint\DC2\CAN\n\
      \\aaddress\CAN\SOH \SOH(\tR\aaddress\DC2\DC2\n\
      \\EOTport\CAN\STX \SOH(\rR\EOTport"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        address__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "address"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"address")) ::
              Data.ProtoLens.FieldDescriptor EndPoint
        port__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "port"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"port")) ::
              Data.ProtoLens.FieldDescriptor EndPoint
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, address__field_descriptor),
           (Data.ProtoLens.Tag 2, port__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _EndPoint'_unknownFields
        (\ x__ y__ -> x__ {_EndPoint'_unknownFields = y__})
  defMessage
    = EndPoint'_constructor
        {_EndPoint'address = Data.ProtoLens.fieldDefault,
         _EndPoint'port = Data.ProtoLens.fieldDefault,
         _EndPoint'_unknownFields = []}
  parseMessage
    = let
        loop :: EndPoint -> Data.ProtoLens.Encoding.Bytes.Parser EndPoint
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "address"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"address") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "port"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"port") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "EndPoint"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"address") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8
                         _v))
             ((Data.Monoid.<>)
                (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"port") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData EndPoint where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_EndPoint'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_EndPoint'address x__)
                (Control.DeepSeq.deepseq (_EndPoint'port x__) ()))
{- | Fields :
     
         * 'Proto.Cluster_Fields.info' @:: Lens' GossipRequest ClusterInfo@
         * 'Proto.Cluster_Fields.maybe'info' @:: Lens' GossipRequest (Prelude.Maybe ClusterInfo)@
         * 'Proto.Cluster_Fields.server' @:: Lens' GossipRequest EndPoint@
         * 'Proto.Cluster_Fields.maybe'server' @:: Lens' GossipRequest (Prelude.Maybe EndPoint)@ -}
data GossipRequest
  = GossipRequest'_constructor {_GossipRequest'info :: !(Prelude.Maybe ClusterInfo),
                                _GossipRequest'server :: !(Prelude.Maybe EndPoint),
                                _GossipRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show GossipRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField GossipRequest "info" ClusterInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _GossipRequest'info (\ x__ y__ -> x__ {_GossipRequest'info = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField GossipRequest "maybe'info" (Prelude.Maybe ClusterInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _GossipRequest'info (\ x__ y__ -> x__ {_GossipRequest'info = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField GossipRequest "server" EndPoint where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _GossipRequest'server
           (\ x__ y__ -> x__ {_GossipRequest'server = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField GossipRequest "maybe'server" (Prelude.Maybe EndPoint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _GossipRequest'server
           (\ x__ y__ -> x__ {_GossipRequest'server = y__}))
        Prelude.id
instance Data.ProtoLens.Message GossipRequest where
  messageName _ = Data.Text.pack "event_store.cluster.GossipRequest"
  packedMessageDescriptor _
    = "\n\
      \\rGossipRequest\DC24\n\
      \\EOTinfo\CAN\SOH \SOH(\v2 .event_store.cluster.ClusterInfoR\EOTinfo\DC25\n\
      \\ACKserver\CAN\STX \SOH(\v2\GS.event_store.cluster.EndPointR\ACKserver"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        info__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ClusterInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'info")) ::
              Data.ProtoLens.FieldDescriptor GossipRequest
        server__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "server"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor EndPoint)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'server")) ::
              Data.ProtoLens.FieldDescriptor GossipRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, info__field_descriptor),
           (Data.ProtoLens.Tag 2, server__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _GossipRequest'_unknownFields
        (\ x__ y__ -> x__ {_GossipRequest'_unknownFields = y__})
  defMessage
    = GossipRequest'_constructor
        {_GossipRequest'info = Prelude.Nothing,
         _GossipRequest'server = Prelude.Nothing,
         _GossipRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          GossipRequest -> Data.ProtoLens.Encoding.Bytes.Parser GossipRequest
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "info"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"info") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "server"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"server") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "GossipRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'info") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage
                          _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'server") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData GossipRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_GossipRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_GossipRequest'info x__)
                (Control.DeepSeq.deepseq (_GossipRequest'server x__) ()))
{- | Fields :
     
         * 'Proto.Cluster_Fields.leaderId' @:: Lens' LeaderIsResigningOkRequest Proto.Shared.UUID@
         * 'Proto.Cluster_Fields.maybe'leaderId' @:: Lens' LeaderIsResigningOkRequest (Prelude.Maybe Proto.Shared.UUID)@
         * 'Proto.Cluster_Fields.leaderHttp' @:: Lens' LeaderIsResigningOkRequest EndPoint@
         * 'Proto.Cluster_Fields.maybe'leaderHttp' @:: Lens' LeaderIsResigningOkRequest (Prelude.Maybe EndPoint)@
         * 'Proto.Cluster_Fields.serverId' @:: Lens' LeaderIsResigningOkRequest Proto.Shared.UUID@
         * 'Proto.Cluster_Fields.maybe'serverId' @:: Lens' LeaderIsResigningOkRequest (Prelude.Maybe Proto.Shared.UUID)@
         * 'Proto.Cluster_Fields.serverHttp' @:: Lens' LeaderIsResigningOkRequest EndPoint@
         * 'Proto.Cluster_Fields.maybe'serverHttp' @:: Lens' LeaderIsResigningOkRequest (Prelude.Maybe EndPoint)@ -}
data LeaderIsResigningOkRequest
  = LeaderIsResigningOkRequest'_constructor {_LeaderIsResigningOkRequest'leaderId :: !(Prelude.Maybe Proto.Shared.UUID),
                                             _LeaderIsResigningOkRequest'leaderHttp :: !(Prelude.Maybe EndPoint),
                                             _LeaderIsResigningOkRequest'serverId :: !(Prelude.Maybe Proto.Shared.UUID),
                                             _LeaderIsResigningOkRequest'serverHttp :: !(Prelude.Maybe EndPoint),
                                             _LeaderIsResigningOkRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LeaderIsResigningOkRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField LeaderIsResigningOkRequest "leaderId" Proto.Shared.UUID where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LeaderIsResigningOkRequest'leaderId
           (\ x__ y__ -> x__ {_LeaderIsResigningOkRequest'leaderId = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField LeaderIsResigningOkRequest "maybe'leaderId" (Prelude.Maybe Proto.Shared.UUID) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LeaderIsResigningOkRequest'leaderId
           (\ x__ y__ -> x__ {_LeaderIsResigningOkRequest'leaderId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LeaderIsResigningOkRequest "leaderHttp" EndPoint where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LeaderIsResigningOkRequest'leaderHttp
           (\ x__ y__ -> x__ {_LeaderIsResigningOkRequest'leaderHttp = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField LeaderIsResigningOkRequest "maybe'leaderHttp" (Prelude.Maybe EndPoint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LeaderIsResigningOkRequest'leaderHttp
           (\ x__ y__ -> x__ {_LeaderIsResigningOkRequest'leaderHttp = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LeaderIsResigningOkRequest "serverId" Proto.Shared.UUID where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LeaderIsResigningOkRequest'serverId
           (\ x__ y__ -> x__ {_LeaderIsResigningOkRequest'serverId = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField LeaderIsResigningOkRequest "maybe'serverId" (Prelude.Maybe Proto.Shared.UUID) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LeaderIsResigningOkRequest'serverId
           (\ x__ y__ -> x__ {_LeaderIsResigningOkRequest'serverId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LeaderIsResigningOkRequest "serverHttp" EndPoint where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LeaderIsResigningOkRequest'serverHttp
           (\ x__ y__ -> x__ {_LeaderIsResigningOkRequest'serverHttp = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField LeaderIsResigningOkRequest "maybe'serverHttp" (Prelude.Maybe EndPoint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LeaderIsResigningOkRequest'serverHttp
           (\ x__ y__ -> x__ {_LeaderIsResigningOkRequest'serverHttp = y__}))
        Prelude.id
instance Data.ProtoLens.Message LeaderIsResigningOkRequest where
  messageName _
    = Data.Text.pack "event_store.cluster.LeaderIsResigningOkRequest"
  packedMessageDescriptor _
    = "\n\
      \\SUBLeaderIsResigningOkRequest\DC2<\n\
      \\tleader_id\CAN\SOH \SOH(\v2\US.event_store.client.shared.UUIDR\bleaderId\DC2>\n\
      \\vleader_http\CAN\STX \SOH(\v2\GS.event_store.cluster.EndPointR\n\
      \leaderHttp\DC2<\n\
      \\tserver_id\CAN\ETX \SOH(\v2\US.event_store.client.shared.UUIDR\bserverId\DC2>\n\
      \\vserver_http\CAN\EOT \SOH(\v2\GS.event_store.cluster.EndPointR\n\
      \serverHttp"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        leaderId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "leader_id"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.UUID)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'leaderId")) ::
              Data.ProtoLens.FieldDescriptor LeaderIsResigningOkRequest
        leaderHttp__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "leader_http"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor EndPoint)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'leaderHttp")) ::
              Data.ProtoLens.FieldDescriptor LeaderIsResigningOkRequest
        serverId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "server_id"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.UUID)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'serverId")) ::
              Data.ProtoLens.FieldDescriptor LeaderIsResigningOkRequest
        serverHttp__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "server_http"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor EndPoint)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'serverHttp")) ::
              Data.ProtoLens.FieldDescriptor LeaderIsResigningOkRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, leaderId__field_descriptor),
           (Data.ProtoLens.Tag 2, leaderHttp__field_descriptor),
           (Data.ProtoLens.Tag 3, serverId__field_descriptor),
           (Data.ProtoLens.Tag 4, serverHttp__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LeaderIsResigningOkRequest'_unknownFields
        (\ x__ y__
           -> x__ {_LeaderIsResigningOkRequest'_unknownFields = y__})
  defMessage
    = LeaderIsResigningOkRequest'_constructor
        {_LeaderIsResigningOkRequest'leaderId = Prelude.Nothing,
         _LeaderIsResigningOkRequest'leaderHttp = Prelude.Nothing,
         _LeaderIsResigningOkRequest'serverId = Prelude.Nothing,
         _LeaderIsResigningOkRequest'serverHttp = Prelude.Nothing,
         _LeaderIsResigningOkRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LeaderIsResigningOkRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser LeaderIsResigningOkRequest
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "leader_id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"leaderId") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "leader_http"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"leaderHttp") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "server_id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"serverId") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "server_http"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"serverHttp") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "LeaderIsResigningOkRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'leaderId") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage
                          _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'leaderHttp") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             _v))
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'serverId") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just _v)
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage
                                _v))
                   ((Data.Monoid.<>)
                      (case
                           Lens.Family2.view
                             (Data.ProtoLens.Field.field @"maybe'serverHttp") _x
                       of
                         Prelude.Nothing -> Data.Monoid.mempty
                         (Prelude.Just _v)
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                ((Prelude..)
                                   (\ bs
                                      -> (Data.Monoid.<>)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                                           (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                   Data.ProtoLens.encodeMessage
                                   _v))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData LeaderIsResigningOkRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LeaderIsResigningOkRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_LeaderIsResigningOkRequest'leaderId x__)
                (Control.DeepSeq.deepseq
                   (_LeaderIsResigningOkRequest'leaderHttp x__)
                   (Control.DeepSeq.deepseq
                      (_LeaderIsResigningOkRequest'serverId x__)
                      (Control.DeepSeq.deepseq
                         (_LeaderIsResigningOkRequest'serverHttp x__) ()))))
{- | Fields :
     
         * 'Proto.Cluster_Fields.leaderId' @:: Lens' LeaderIsResigningRequest Proto.Shared.UUID@
         * 'Proto.Cluster_Fields.maybe'leaderId' @:: Lens' LeaderIsResigningRequest (Prelude.Maybe Proto.Shared.UUID)@
         * 'Proto.Cluster_Fields.leaderHttp' @:: Lens' LeaderIsResigningRequest EndPoint@
         * 'Proto.Cluster_Fields.maybe'leaderHttp' @:: Lens' LeaderIsResigningRequest (Prelude.Maybe EndPoint)@ -}
data LeaderIsResigningRequest
  = LeaderIsResigningRequest'_constructor {_LeaderIsResigningRequest'leaderId :: !(Prelude.Maybe Proto.Shared.UUID),
                                           _LeaderIsResigningRequest'leaderHttp :: !(Prelude.Maybe EndPoint),
                                           _LeaderIsResigningRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LeaderIsResigningRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField LeaderIsResigningRequest "leaderId" Proto.Shared.UUID where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LeaderIsResigningRequest'leaderId
           (\ x__ y__ -> x__ {_LeaderIsResigningRequest'leaderId = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField LeaderIsResigningRequest "maybe'leaderId" (Prelude.Maybe Proto.Shared.UUID) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LeaderIsResigningRequest'leaderId
           (\ x__ y__ -> x__ {_LeaderIsResigningRequest'leaderId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LeaderIsResigningRequest "leaderHttp" EndPoint where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LeaderIsResigningRequest'leaderHttp
           (\ x__ y__ -> x__ {_LeaderIsResigningRequest'leaderHttp = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField LeaderIsResigningRequest "maybe'leaderHttp" (Prelude.Maybe EndPoint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LeaderIsResigningRequest'leaderHttp
           (\ x__ y__ -> x__ {_LeaderIsResigningRequest'leaderHttp = y__}))
        Prelude.id
instance Data.ProtoLens.Message LeaderIsResigningRequest where
  messageName _
    = Data.Text.pack "event_store.cluster.LeaderIsResigningRequest"
  packedMessageDescriptor _
    = "\n\
      \\CANLeaderIsResigningRequest\DC2<\n\
      \\tleader_id\CAN\SOH \SOH(\v2\US.event_store.client.shared.UUIDR\bleaderId\DC2>\n\
      \\vleader_http\CAN\STX \SOH(\v2\GS.event_store.cluster.EndPointR\n\
      \leaderHttp"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        leaderId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "leader_id"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.UUID)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'leaderId")) ::
              Data.ProtoLens.FieldDescriptor LeaderIsResigningRequest
        leaderHttp__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "leader_http"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor EndPoint)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'leaderHttp")) ::
              Data.ProtoLens.FieldDescriptor LeaderIsResigningRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, leaderId__field_descriptor),
           (Data.ProtoLens.Tag 2, leaderHttp__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LeaderIsResigningRequest'_unknownFields
        (\ x__ y__ -> x__ {_LeaderIsResigningRequest'_unknownFields = y__})
  defMessage
    = LeaderIsResigningRequest'_constructor
        {_LeaderIsResigningRequest'leaderId = Prelude.Nothing,
         _LeaderIsResigningRequest'leaderHttp = Prelude.Nothing,
         _LeaderIsResigningRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LeaderIsResigningRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser LeaderIsResigningRequest
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "leader_id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"leaderId") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "leader_http"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"leaderHttp") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "LeaderIsResigningRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'leaderId") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage
                          _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'leaderHttp") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData LeaderIsResigningRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LeaderIsResigningRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_LeaderIsResigningRequest'leaderId x__)
                (Control.DeepSeq.deepseq
                   (_LeaderIsResigningRequest'leaderHttp x__) ()))
{- | Fields :
     
         * 'Proto.Cluster_Fields.instanceId' @:: Lens' MemberInfo Proto.Shared.UUID@
         * 'Proto.Cluster_Fields.maybe'instanceId' @:: Lens' MemberInfo (Prelude.Maybe Proto.Shared.UUID)@
         * 'Proto.Cluster_Fields.timeStamp' @:: Lens' MemberInfo Data.Int.Int64@
         * 'Proto.Cluster_Fields.state' @:: Lens' MemberInfo MemberInfo'VNodeState@
         * 'Proto.Cluster_Fields.isAlive' @:: Lens' MemberInfo Prelude.Bool@
         * 'Proto.Cluster_Fields.httpEndPoint' @:: Lens' MemberInfo EndPoint@
         * 'Proto.Cluster_Fields.maybe'httpEndPoint' @:: Lens' MemberInfo (Prelude.Maybe EndPoint)@
         * 'Proto.Cluster_Fields.internalTcp' @:: Lens' MemberInfo EndPoint@
         * 'Proto.Cluster_Fields.maybe'internalTcp' @:: Lens' MemberInfo (Prelude.Maybe EndPoint)@
         * 'Proto.Cluster_Fields.externalTcp' @:: Lens' MemberInfo EndPoint@
         * 'Proto.Cluster_Fields.maybe'externalTcp' @:: Lens' MemberInfo (Prelude.Maybe EndPoint)@
         * 'Proto.Cluster_Fields.internalTcpUsesTls' @:: Lens' MemberInfo Prelude.Bool@
         * 'Proto.Cluster_Fields.externalTcpUsesTls' @:: Lens' MemberInfo Prelude.Bool@
         * 'Proto.Cluster_Fields.lastCommitPosition' @:: Lens' MemberInfo Data.Int.Int64@
         * 'Proto.Cluster_Fields.writerCheckpoint' @:: Lens' MemberInfo Data.Int.Int64@
         * 'Proto.Cluster_Fields.chaserCheckpoint' @:: Lens' MemberInfo Data.Int.Int64@
         * 'Proto.Cluster_Fields.epochPosition' @:: Lens' MemberInfo Data.Int.Int64@
         * 'Proto.Cluster_Fields.epochNumber' @:: Lens' MemberInfo Data.Int.Int32@
         * 'Proto.Cluster_Fields.epochId' @:: Lens' MemberInfo Proto.Shared.UUID@
         * 'Proto.Cluster_Fields.maybe'epochId' @:: Lens' MemberInfo (Prelude.Maybe Proto.Shared.UUID)@
         * 'Proto.Cluster_Fields.nodePriority' @:: Lens' MemberInfo Data.Int.Int32@
         * 'Proto.Cluster_Fields.isReadOnlyReplica' @:: Lens' MemberInfo Prelude.Bool@ -}
data MemberInfo
  = MemberInfo'_constructor {_MemberInfo'instanceId :: !(Prelude.Maybe Proto.Shared.UUID),
                             _MemberInfo'timeStamp :: !Data.Int.Int64,
                             _MemberInfo'state :: !MemberInfo'VNodeState,
                             _MemberInfo'isAlive :: !Prelude.Bool,
                             _MemberInfo'httpEndPoint :: !(Prelude.Maybe EndPoint),
                             _MemberInfo'internalTcp :: !(Prelude.Maybe EndPoint),
                             _MemberInfo'externalTcp :: !(Prelude.Maybe EndPoint),
                             _MemberInfo'internalTcpUsesTls :: !Prelude.Bool,
                             _MemberInfo'externalTcpUsesTls :: !Prelude.Bool,
                             _MemberInfo'lastCommitPosition :: !Data.Int.Int64,
                             _MemberInfo'writerCheckpoint :: !Data.Int.Int64,
                             _MemberInfo'chaserCheckpoint :: !Data.Int.Int64,
                             _MemberInfo'epochPosition :: !Data.Int.Int64,
                             _MemberInfo'epochNumber :: !Data.Int.Int32,
                             _MemberInfo'epochId :: !(Prelude.Maybe Proto.Shared.UUID),
                             _MemberInfo'nodePriority :: !Data.Int.Int32,
                             _MemberInfo'isReadOnlyReplica :: !Prelude.Bool,
                             _MemberInfo'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show MemberInfo where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField MemberInfo "instanceId" Proto.Shared.UUID where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemberInfo'instanceId
           (\ x__ y__ -> x__ {_MemberInfo'instanceId = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField MemberInfo "maybe'instanceId" (Prelude.Maybe Proto.Shared.UUID) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemberInfo'instanceId
           (\ x__ y__ -> x__ {_MemberInfo'instanceId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField MemberInfo "timeStamp" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemberInfo'timeStamp
           (\ x__ y__ -> x__ {_MemberInfo'timeStamp = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField MemberInfo "state" MemberInfo'VNodeState where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemberInfo'state (\ x__ y__ -> x__ {_MemberInfo'state = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField MemberInfo "isAlive" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemberInfo'isAlive (\ x__ y__ -> x__ {_MemberInfo'isAlive = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField MemberInfo "httpEndPoint" EndPoint where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemberInfo'httpEndPoint
           (\ x__ y__ -> x__ {_MemberInfo'httpEndPoint = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField MemberInfo "maybe'httpEndPoint" (Prelude.Maybe EndPoint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemberInfo'httpEndPoint
           (\ x__ y__ -> x__ {_MemberInfo'httpEndPoint = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField MemberInfo "internalTcp" EndPoint where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemberInfo'internalTcp
           (\ x__ y__ -> x__ {_MemberInfo'internalTcp = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField MemberInfo "maybe'internalTcp" (Prelude.Maybe EndPoint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemberInfo'internalTcp
           (\ x__ y__ -> x__ {_MemberInfo'internalTcp = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField MemberInfo "externalTcp" EndPoint where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemberInfo'externalTcp
           (\ x__ y__ -> x__ {_MemberInfo'externalTcp = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField MemberInfo "maybe'externalTcp" (Prelude.Maybe EndPoint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemberInfo'externalTcp
           (\ x__ y__ -> x__ {_MemberInfo'externalTcp = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField MemberInfo "internalTcpUsesTls" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemberInfo'internalTcpUsesTls
           (\ x__ y__ -> x__ {_MemberInfo'internalTcpUsesTls = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField MemberInfo "externalTcpUsesTls" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemberInfo'externalTcpUsesTls
           (\ x__ y__ -> x__ {_MemberInfo'externalTcpUsesTls = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField MemberInfo "lastCommitPosition" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemberInfo'lastCommitPosition
           (\ x__ y__ -> x__ {_MemberInfo'lastCommitPosition = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField MemberInfo "writerCheckpoint" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemberInfo'writerCheckpoint
           (\ x__ y__ -> x__ {_MemberInfo'writerCheckpoint = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField MemberInfo "chaserCheckpoint" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemberInfo'chaserCheckpoint
           (\ x__ y__ -> x__ {_MemberInfo'chaserCheckpoint = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField MemberInfo "epochPosition" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemberInfo'epochPosition
           (\ x__ y__ -> x__ {_MemberInfo'epochPosition = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField MemberInfo "epochNumber" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemberInfo'epochNumber
           (\ x__ y__ -> x__ {_MemberInfo'epochNumber = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField MemberInfo "epochId" Proto.Shared.UUID where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemberInfo'epochId (\ x__ y__ -> x__ {_MemberInfo'epochId = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField MemberInfo "maybe'epochId" (Prelude.Maybe Proto.Shared.UUID) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemberInfo'epochId (\ x__ y__ -> x__ {_MemberInfo'epochId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField MemberInfo "nodePriority" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemberInfo'nodePriority
           (\ x__ y__ -> x__ {_MemberInfo'nodePriority = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField MemberInfo "isReadOnlyReplica" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemberInfo'isReadOnlyReplica
           (\ x__ y__ -> x__ {_MemberInfo'isReadOnlyReplica = y__}))
        Prelude.id
instance Data.ProtoLens.Message MemberInfo where
  messageName _ = Data.Text.pack "event_store.cluster.MemberInfo"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \MemberInfo\DC2@\n\
      \\vinstance_id\CAN\SOH \SOH(\v2\US.event_store.client.shared.UUIDR\n\
      \instanceId\DC2\GS\n\
      \\n\
      \time_stamp\CAN\STX \SOH(\ETXR\ttimeStamp\DC2@\n\
      \\ENQstate\CAN\ETX \SOH(\SO2*.event_store.cluster.MemberInfo.VNodeStateR\ENQstate\DC2\EM\n\
      \\bis_alive\CAN\EOT \SOH(\bR\aisAlive\DC2C\n\
      \\SOhttp_end_point\CAN\ENQ \SOH(\v2\GS.event_store.cluster.EndPointR\fhttpEndPoint\DC2@\n\
      \\finternal_tcp\CAN\ACK \SOH(\v2\GS.event_store.cluster.EndPointR\vinternalTcp\DC2@\n\
      \\fexternal_tcp\CAN\a \SOH(\v2\GS.event_store.cluster.EndPointR\vexternalTcp\DC21\n\
      \\NAKinternal_tcp_uses_tls\CAN\b \SOH(\bR\DC2internalTcpUsesTls\DC21\n\
      \\NAKexternal_tcp_uses_tls\CAN\t \SOH(\bR\DC2externalTcpUsesTls\DC20\n\
      \\DC4last_commit_position\CAN\n\
      \ \SOH(\ETXR\DC2lastCommitPosition\DC2+\n\
      \\DC1writer_checkpoint\CAN\v \SOH(\ETXR\DLEwriterCheckpoint\DC2+\n\
      \\DC1chaser_checkpoint\CAN\f \SOH(\ETXR\DLEchaserCheckpoint\DC2%\n\
      \\SOepoch_position\CAN\r \SOH(\ETXR\repochPosition\DC2!\n\
      \\fepoch_number\CAN\SO \SOH(\ENQR\vepochNumber\DC2:\n\
      \\bepoch_id\CAN\SI \SOH(\v2\US.event_store.client.shared.UUIDR\aepochId\DC2#\n\
      \\rnode_priority\CAN\DLE \SOH(\ENQR\fnodePriority\DC2/\n\
      \\DC4is_read_only_replica\CAN\DC1 \SOH(\bR\DC1isReadOnlyReplica\"\154\STX\n\
      \\n\
      \VNodeState\DC2\DLE\n\
      \\fInitializing\DLE\NUL\DC2\DC2\n\
      \\SODiscoverLeader\DLE\SOH\DC2\v\n\
      \\aUnknown\DLE\STX\DC2\SO\n\
      \\n\
      \PreReplica\DLE\ETX\DC2\SO\n\
      \\n\
      \CatchingUp\DLE\EOT\DC2\t\n\
      \\ENQClone\DLE\ENQ\DC2\f\n\
      \\bFollower\DLE\ACK\DC2\r\n\
      \\tPreLeader\DLE\a\DC2\n\
      \\n\
      \\ACKLeader\DLE\b\DC2\v\n\
      \\aManager\DLE\t\DC2\DLE\n\
      \\fShuttingDown\DLE\n\
      \\DC2\f\n\
      \\bShutdown\DLE\v\DC2\SYN\n\
      \\DC2ReadOnlyLeaderless\DLE\f\DC2\SYN\n\
      \\DC2PreReadOnlyReplica\DLE\r\DC2\DC3\n\
      \\SIReadOnlyReplica\DLE\SO\DC2\DC3\n\
      \\SIResigningLeader\DLE\SI"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        instanceId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "instance_id"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.UUID)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'instanceId")) ::
              Data.ProtoLens.FieldDescriptor MemberInfo
        timeStamp__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "time_stamp"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"timeStamp")) ::
              Data.ProtoLens.FieldDescriptor MemberInfo
        state__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "state"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor MemberInfo'VNodeState)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"state")) ::
              Data.ProtoLens.FieldDescriptor MemberInfo
        isAlive__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "is_alive"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"isAlive")) ::
              Data.ProtoLens.FieldDescriptor MemberInfo
        httpEndPoint__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "http_end_point"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor EndPoint)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'httpEndPoint")) ::
              Data.ProtoLens.FieldDescriptor MemberInfo
        internalTcp__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "internal_tcp"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor EndPoint)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'internalTcp")) ::
              Data.ProtoLens.FieldDescriptor MemberInfo
        externalTcp__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "external_tcp"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor EndPoint)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'externalTcp")) ::
              Data.ProtoLens.FieldDescriptor MemberInfo
        internalTcpUsesTls__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "internal_tcp_uses_tls"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"internalTcpUsesTls")) ::
              Data.ProtoLens.FieldDescriptor MemberInfo
        externalTcpUsesTls__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "external_tcp_uses_tls"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"externalTcpUsesTls")) ::
              Data.ProtoLens.FieldDescriptor MemberInfo
        lastCommitPosition__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "last_commit_position"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"lastCommitPosition")) ::
              Data.ProtoLens.FieldDescriptor MemberInfo
        writerCheckpoint__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "writer_checkpoint"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"writerCheckpoint")) ::
              Data.ProtoLens.FieldDescriptor MemberInfo
        chaserCheckpoint__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "chaser_checkpoint"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"chaserCheckpoint")) ::
              Data.ProtoLens.FieldDescriptor MemberInfo
        epochPosition__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "epoch_position"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"epochPosition")) ::
              Data.ProtoLens.FieldDescriptor MemberInfo
        epochNumber__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "epoch_number"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"epochNumber")) ::
              Data.ProtoLens.FieldDescriptor MemberInfo
        epochId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "epoch_id"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.UUID)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'epochId")) ::
              Data.ProtoLens.FieldDescriptor MemberInfo
        nodePriority__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "node_priority"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"nodePriority")) ::
              Data.ProtoLens.FieldDescriptor MemberInfo
        isReadOnlyReplica__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "is_read_only_replica"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"isReadOnlyReplica")) ::
              Data.ProtoLens.FieldDescriptor MemberInfo
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, instanceId__field_descriptor),
           (Data.ProtoLens.Tag 2, timeStamp__field_descriptor),
           (Data.ProtoLens.Tag 3, state__field_descriptor),
           (Data.ProtoLens.Tag 4, isAlive__field_descriptor),
           (Data.ProtoLens.Tag 5, httpEndPoint__field_descriptor),
           (Data.ProtoLens.Tag 6, internalTcp__field_descriptor),
           (Data.ProtoLens.Tag 7, externalTcp__field_descriptor),
           (Data.ProtoLens.Tag 8, internalTcpUsesTls__field_descriptor),
           (Data.ProtoLens.Tag 9, externalTcpUsesTls__field_descriptor),
           (Data.ProtoLens.Tag 10, lastCommitPosition__field_descriptor),
           (Data.ProtoLens.Tag 11, writerCheckpoint__field_descriptor),
           (Data.ProtoLens.Tag 12, chaserCheckpoint__field_descriptor),
           (Data.ProtoLens.Tag 13, epochPosition__field_descriptor),
           (Data.ProtoLens.Tag 14, epochNumber__field_descriptor),
           (Data.ProtoLens.Tag 15, epochId__field_descriptor),
           (Data.ProtoLens.Tag 16, nodePriority__field_descriptor),
           (Data.ProtoLens.Tag 17, isReadOnlyReplica__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _MemberInfo'_unknownFields
        (\ x__ y__ -> x__ {_MemberInfo'_unknownFields = y__})
  defMessage
    = MemberInfo'_constructor
        {_MemberInfo'instanceId = Prelude.Nothing,
         _MemberInfo'timeStamp = Data.ProtoLens.fieldDefault,
         _MemberInfo'state = Data.ProtoLens.fieldDefault,
         _MemberInfo'isAlive = Data.ProtoLens.fieldDefault,
         _MemberInfo'httpEndPoint = Prelude.Nothing,
         _MemberInfo'internalTcp = Prelude.Nothing,
         _MemberInfo'externalTcp = Prelude.Nothing,
         _MemberInfo'internalTcpUsesTls = Data.ProtoLens.fieldDefault,
         _MemberInfo'externalTcpUsesTls = Data.ProtoLens.fieldDefault,
         _MemberInfo'lastCommitPosition = Data.ProtoLens.fieldDefault,
         _MemberInfo'writerCheckpoint = Data.ProtoLens.fieldDefault,
         _MemberInfo'chaserCheckpoint = Data.ProtoLens.fieldDefault,
         _MemberInfo'epochPosition = Data.ProtoLens.fieldDefault,
         _MemberInfo'epochNumber = Data.ProtoLens.fieldDefault,
         _MemberInfo'epochId = Prelude.Nothing,
         _MemberInfo'nodePriority = Data.ProtoLens.fieldDefault,
         _MemberInfo'isReadOnlyReplica = Data.ProtoLens.fieldDefault,
         _MemberInfo'_unknownFields = []}
  parseMessage
    = let
        loop ::
          MemberInfo -> Data.ProtoLens.Encoding.Bytes.Parser MemberInfo
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "instance_id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"instanceId") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "time_stamp"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"timeStamp") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "state"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"state") y x)
                        32
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "is_alive"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"isAlive") y x)
                        42
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "http_end_point"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"httpEndPoint") y x)
                        50
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "internal_tcp"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"internalTcp") y x)
                        58
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "external_tcp"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"externalTcp") y x)
                        64
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "internal_tcp_uses_tls"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"internalTcpUsesTls") y x)
                        72
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "external_tcp_uses_tls"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"externalTcpUsesTls") y x)
                        80
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "last_commit_position"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"lastCommitPosition") y x)
                        88
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "writer_checkpoint"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"writerCheckpoint") y x)
                        96
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "chaser_checkpoint"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"chaserCheckpoint") y x)
                        104
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "epoch_position"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"epochPosition") y x)
                        112
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "epoch_number"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"epochNumber") y x)
                        122
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "epoch_id"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"epochId") y x)
                        128
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "node_priority"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"nodePriority") y x)
                        136
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "is_read_only_replica"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"isReadOnlyReplica") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MemberInfo"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'instanceId") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage
                          _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"timeStamp") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                ((Data.Monoid.<>)
                   (let
                      _v = Lens.Family2.view (Data.ProtoLens.Field.field @"state") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            ((Prelude..)
                               ((Prelude..)
                                  Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                               Prelude.fromEnum
                               _v))
                   ((Data.Monoid.<>)
                      (let
                         _v = Lens.Family2.view (Data.ProtoLens.Field.field @"isAlive") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 32)
                               ((Prelude..)
                                  Data.ProtoLens.Encoding.Bytes.putVarInt
                                  (\ b -> if b then 1 else 0)
                                  _v))
                      ((Data.Monoid.<>)
                         (case
                              Lens.Family2.view
                                (Data.ProtoLens.Field.field @"maybe'httpEndPoint") _x
                          of
                            Prelude.Nothing -> Data.Monoid.mempty
                            (Prelude.Just _v)
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                                   ((Prelude..)
                                      (\ bs
                                         -> (Data.Monoid.<>)
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                 (Prelude.fromIntegral (Data.ByteString.length bs)))
                                              (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                      Data.ProtoLens.encodeMessage
                                      _v))
                         ((Data.Monoid.<>)
                            (case
                                 Lens.Family2.view
                                   (Data.ProtoLens.Field.field @"maybe'internalTcp") _x
                             of
                               Prelude.Nothing -> Data.Monoid.mempty
                               (Prelude.Just _v)
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt 50)
                                      ((Prelude..)
                                         (\ bs
                                            -> (Data.Monoid.<>)
                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                    (Prelude.fromIntegral
                                                       (Data.ByteString.length bs)))
                                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                         Data.ProtoLens.encodeMessage
                                         _v))
                            ((Data.Monoid.<>)
                               (case
                                    Lens.Family2.view
                                      (Data.ProtoLens.Field.field @"maybe'externalTcp") _x
                                of
                                  Prelude.Nothing -> Data.Monoid.mempty
                                  (Prelude.Just _v)
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt 58)
                                         ((Prelude..)
                                            (\ bs
                                               -> (Data.Monoid.<>)
                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                       (Prelude.fromIntegral
                                                          (Data.ByteString.length bs)))
                                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                            Data.ProtoLens.encodeMessage
                                            _v))
                               ((Data.Monoid.<>)
                                  (let
                                     _v
                                       = Lens.Family2.view
                                           (Data.ProtoLens.Field.field @"internalTcpUsesTls") _x
                                   in
                                     if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                         Data.Monoid.mempty
                                     else
                                         (Data.Monoid.<>)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt 64)
                                           ((Prelude..)
                                              Data.ProtoLens.Encoding.Bytes.putVarInt
                                              (\ b -> if b then 1 else 0)
                                              _v))
                                  ((Data.Monoid.<>)
                                     (let
                                        _v
                                          = Lens.Family2.view
                                              (Data.ProtoLens.Field.field @"externalTcpUsesTls") _x
                                      in
                                        if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                            Data.Monoid.mempty
                                        else
                                            (Data.Monoid.<>)
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt 72)
                                              ((Prelude..)
                                                 Data.ProtoLens.Encoding.Bytes.putVarInt
                                                 (\ b -> if b then 1 else 0)
                                                 _v))
                                     ((Data.Monoid.<>)
                                        (let
                                           _v
                                             = Lens.Family2.view
                                                 (Data.ProtoLens.Field.field @"lastCommitPosition")
                                                 _x
                                         in
                                           if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                               Data.Monoid.mempty
                                           else
                                               (Data.Monoid.<>)
                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 80)
                                                 ((Prelude..)
                                                    Data.ProtoLens.Encoding.Bytes.putVarInt
                                                    Prelude.fromIntegral
                                                    _v))
                                        ((Data.Monoid.<>)
                                           (let
                                              _v
                                                = Lens.Family2.view
                                                    (Data.ProtoLens.Field.field @"writerCheckpoint")
                                                    _x
                                            in
                                              if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                                  Data.Monoid.mempty
                                              else
                                                  (Data.Monoid.<>)
                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt 88)
                                                    ((Prelude..)
                                                       Data.ProtoLens.Encoding.Bytes.putVarInt
                                                       Prelude.fromIntegral
                                                       _v))
                                           ((Data.Monoid.<>)
                                              (let
                                                 _v
                                                   = Lens.Family2.view
                                                       (Data.ProtoLens.Field.field
                                                          @"chaserCheckpoint")
                                                       _x
                                               in
                                                 if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                                     Data.Monoid.mempty
                                                 else
                                                     (Data.Monoid.<>)
                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt 96)
                                                       ((Prelude..)
                                                          Data.ProtoLens.Encoding.Bytes.putVarInt
                                                          Prelude.fromIntegral
                                                          _v))
                                              ((Data.Monoid.<>)
                                                 (let
                                                    _v
                                                      = Lens.Family2.view
                                                          (Data.ProtoLens.Field.field
                                                             @"epochPosition")
                                                          _x
                                                  in
                                                    if (Prelude.==)
                                                         _v Data.ProtoLens.fieldDefault then
                                                        Data.Monoid.mempty
                                                    else
                                                        (Data.Monoid.<>)
                                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                             104)
                                                          ((Prelude..)
                                                             Data.ProtoLens.Encoding.Bytes.putVarInt
                                                             Prelude.fromIntegral
                                                             _v))
                                                 ((Data.Monoid.<>)
                                                    (let
                                                       _v
                                                         = Lens.Family2.view
                                                             (Data.ProtoLens.Field.field
                                                                @"epochNumber")
                                                             _x
                                                     in
                                                       if (Prelude.==)
                                                            _v Data.ProtoLens.fieldDefault then
                                                           Data.Monoid.mempty
                                                       else
                                                           (Data.Monoid.<>)
                                                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                112)
                                                             ((Prelude..)
                                                                Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                Prelude.fromIntegral
                                                                _v))
                                                    ((Data.Monoid.<>)
                                                       (case
                                                            Lens.Family2.view
                                                              (Data.ProtoLens.Field.field
                                                                 @"maybe'epochId")
                                                              _x
                                                        of
                                                          Prelude.Nothing -> Data.Monoid.mempty
                                                          (Prelude.Just _v)
                                                            -> (Data.Monoid.<>)
                                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                    122)
                                                                 ((Prelude..)
                                                                    (\ bs
                                                                       -> (Data.Monoid.<>)
                                                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                               (Prelude.fromIntegral
                                                                                  (Data.ByteString.length
                                                                                     bs)))
                                                                            (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                               bs))
                                                                    Data.ProtoLens.encodeMessage
                                                                    _v))
                                                       ((Data.Monoid.<>)
                                                          (let
                                                             _v
                                                               = Lens.Family2.view
                                                                   (Data.ProtoLens.Field.field
                                                                      @"nodePriority")
                                                                   _x
                                                           in
                                                             if (Prelude.==)
                                                                  _v
                                                                  Data.ProtoLens.fieldDefault then
                                                                 Data.Monoid.mempty
                                                             else
                                                                 (Data.Monoid.<>)
                                                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                      128)
                                                                   ((Prelude..)
                                                                      Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                      Prelude.fromIntegral
                                                                      _v))
                                                          ((Data.Monoid.<>)
                                                             (let
                                                                _v
                                                                  = Lens.Family2.view
                                                                      (Data.ProtoLens.Field.field
                                                                         @"isReadOnlyReplica")
                                                                      _x
                                                              in
                                                                if (Prelude.==)
                                                                     _v
                                                                     Data.ProtoLens.fieldDefault then
                                                                    Data.Monoid.mempty
                                                                else
                                                                    (Data.Monoid.<>)
                                                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                         136)
                                                                      ((Prelude..)
                                                                         Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                         (\ b -> if b then 1 else 0)
                                                                         _v))
                                                             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                                                                (Lens.Family2.view
                                                                   Data.ProtoLens.unknownFields
                                                                   _x))))))))))))))))))
instance Control.DeepSeq.NFData MemberInfo where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_MemberInfo'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_MemberInfo'instanceId x__)
                (Control.DeepSeq.deepseq
                   (_MemberInfo'timeStamp x__)
                   (Control.DeepSeq.deepseq
                      (_MemberInfo'state x__)
                      (Control.DeepSeq.deepseq
                         (_MemberInfo'isAlive x__)
                         (Control.DeepSeq.deepseq
                            (_MemberInfo'httpEndPoint x__)
                            (Control.DeepSeq.deepseq
                               (_MemberInfo'internalTcp x__)
                               (Control.DeepSeq.deepseq
                                  (_MemberInfo'externalTcp x__)
                                  (Control.DeepSeq.deepseq
                                     (_MemberInfo'internalTcpUsesTls x__)
                                     (Control.DeepSeq.deepseq
                                        (_MemberInfo'externalTcpUsesTls x__)
                                        (Control.DeepSeq.deepseq
                                           (_MemberInfo'lastCommitPosition x__)
                                           (Control.DeepSeq.deepseq
                                              (_MemberInfo'writerCheckpoint x__)
                                              (Control.DeepSeq.deepseq
                                                 (_MemberInfo'chaserCheckpoint x__)
                                                 (Control.DeepSeq.deepseq
                                                    (_MemberInfo'epochPosition x__)
                                                    (Control.DeepSeq.deepseq
                                                       (_MemberInfo'epochNumber x__)
                                                       (Control.DeepSeq.deepseq
                                                          (_MemberInfo'epochId x__)
                                                          (Control.DeepSeq.deepseq
                                                             (_MemberInfo'nodePriority x__)
                                                             (Control.DeepSeq.deepseq
                                                                (_MemberInfo'isReadOnlyReplica x__)
                                                                ())))))))))))))))))
newtype MemberInfo'VNodeState'UnrecognizedValue
  = MemberInfo'VNodeState'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data MemberInfo'VNodeState
  = MemberInfo'Initializing |
    MemberInfo'DiscoverLeader |
    MemberInfo'Unknown |
    MemberInfo'PreReplica |
    MemberInfo'CatchingUp |
    MemberInfo'Clone |
    MemberInfo'Follower |
    MemberInfo'PreLeader |
    MemberInfo'Leader |
    MemberInfo'Manager |
    MemberInfo'ShuttingDown |
    MemberInfo'Shutdown |
    MemberInfo'ReadOnlyLeaderless |
    MemberInfo'PreReadOnlyReplica |
    MemberInfo'ReadOnlyReplica |
    MemberInfo'ResigningLeader |
    MemberInfo'VNodeState'Unrecognized !MemberInfo'VNodeState'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum MemberInfo'VNodeState where
  maybeToEnum 0 = Prelude.Just MemberInfo'Initializing
  maybeToEnum 1 = Prelude.Just MemberInfo'DiscoverLeader
  maybeToEnum 2 = Prelude.Just MemberInfo'Unknown
  maybeToEnum 3 = Prelude.Just MemberInfo'PreReplica
  maybeToEnum 4 = Prelude.Just MemberInfo'CatchingUp
  maybeToEnum 5 = Prelude.Just MemberInfo'Clone
  maybeToEnum 6 = Prelude.Just MemberInfo'Follower
  maybeToEnum 7 = Prelude.Just MemberInfo'PreLeader
  maybeToEnum 8 = Prelude.Just MemberInfo'Leader
  maybeToEnum 9 = Prelude.Just MemberInfo'Manager
  maybeToEnum 10 = Prelude.Just MemberInfo'ShuttingDown
  maybeToEnum 11 = Prelude.Just MemberInfo'Shutdown
  maybeToEnum 12 = Prelude.Just MemberInfo'ReadOnlyLeaderless
  maybeToEnum 13 = Prelude.Just MemberInfo'PreReadOnlyReplica
  maybeToEnum 14 = Prelude.Just MemberInfo'ReadOnlyReplica
  maybeToEnum 15 = Prelude.Just MemberInfo'ResigningLeader
  maybeToEnum k
    = Prelude.Just
        (MemberInfo'VNodeState'Unrecognized
           (MemberInfo'VNodeState'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum MemberInfo'Initializing = "Initializing"
  showEnum MemberInfo'DiscoverLeader = "DiscoverLeader"
  showEnum MemberInfo'Unknown = "Unknown"
  showEnum MemberInfo'PreReplica = "PreReplica"
  showEnum MemberInfo'CatchingUp = "CatchingUp"
  showEnum MemberInfo'Clone = "Clone"
  showEnum MemberInfo'Follower = "Follower"
  showEnum MemberInfo'PreLeader = "PreLeader"
  showEnum MemberInfo'Leader = "Leader"
  showEnum MemberInfo'Manager = "Manager"
  showEnum MemberInfo'ShuttingDown = "ShuttingDown"
  showEnum MemberInfo'Shutdown = "Shutdown"
  showEnum MemberInfo'ReadOnlyLeaderless = "ReadOnlyLeaderless"
  showEnum MemberInfo'PreReadOnlyReplica = "PreReadOnlyReplica"
  showEnum MemberInfo'ReadOnlyReplica = "ReadOnlyReplica"
  showEnum MemberInfo'ResigningLeader = "ResigningLeader"
  showEnum
    (MemberInfo'VNodeState'Unrecognized (MemberInfo'VNodeState'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "Initializing"
    = Prelude.Just MemberInfo'Initializing
    | (Prelude.==) k "DiscoverLeader"
    = Prelude.Just MemberInfo'DiscoverLeader
    | (Prelude.==) k "Unknown" = Prelude.Just MemberInfo'Unknown
    | (Prelude.==) k "PreReplica" = Prelude.Just MemberInfo'PreReplica
    | (Prelude.==) k "CatchingUp" = Prelude.Just MemberInfo'CatchingUp
    | (Prelude.==) k "Clone" = Prelude.Just MemberInfo'Clone
    | (Prelude.==) k "Follower" = Prelude.Just MemberInfo'Follower
    | (Prelude.==) k "PreLeader" = Prelude.Just MemberInfo'PreLeader
    | (Prelude.==) k "Leader" = Prelude.Just MemberInfo'Leader
    | (Prelude.==) k "Manager" = Prelude.Just MemberInfo'Manager
    | (Prelude.==) k "ShuttingDown"
    = Prelude.Just MemberInfo'ShuttingDown
    | (Prelude.==) k "Shutdown" = Prelude.Just MemberInfo'Shutdown
    | (Prelude.==) k "ReadOnlyLeaderless"
    = Prelude.Just MemberInfo'ReadOnlyLeaderless
    | (Prelude.==) k "PreReadOnlyReplica"
    = Prelude.Just MemberInfo'PreReadOnlyReplica
    | (Prelude.==) k "ReadOnlyReplica"
    = Prelude.Just MemberInfo'ReadOnlyReplica
    | (Prelude.==) k "ResigningLeader"
    = Prelude.Just MemberInfo'ResigningLeader
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded MemberInfo'VNodeState where
  minBound = MemberInfo'Initializing
  maxBound = MemberInfo'ResigningLeader
instance Prelude.Enum MemberInfo'VNodeState where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum VNodeState: " (Prelude.show k__)))
        Prelude.id
        (Data.ProtoLens.maybeToEnum k__)
  fromEnum MemberInfo'Initializing = 0
  fromEnum MemberInfo'DiscoverLeader = 1
  fromEnum MemberInfo'Unknown = 2
  fromEnum MemberInfo'PreReplica = 3
  fromEnum MemberInfo'CatchingUp = 4
  fromEnum MemberInfo'Clone = 5
  fromEnum MemberInfo'Follower = 6
  fromEnum MemberInfo'PreLeader = 7
  fromEnum MemberInfo'Leader = 8
  fromEnum MemberInfo'Manager = 9
  fromEnum MemberInfo'ShuttingDown = 10
  fromEnum MemberInfo'Shutdown = 11
  fromEnum MemberInfo'ReadOnlyLeaderless = 12
  fromEnum MemberInfo'PreReadOnlyReplica = 13
  fromEnum MemberInfo'ReadOnlyReplica = 14
  fromEnum MemberInfo'ResigningLeader = 15
  fromEnum
    (MemberInfo'VNodeState'Unrecognized (MemberInfo'VNodeState'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ MemberInfo'ResigningLeader
    = Prelude.error
        "MemberInfo'VNodeState.succ: bad argument MemberInfo'ResigningLeader. This value would be out of bounds."
  succ MemberInfo'Initializing = MemberInfo'DiscoverLeader
  succ MemberInfo'DiscoverLeader = MemberInfo'Unknown
  succ MemberInfo'Unknown = MemberInfo'PreReplica
  succ MemberInfo'PreReplica = MemberInfo'CatchingUp
  succ MemberInfo'CatchingUp = MemberInfo'Clone
  succ MemberInfo'Clone = MemberInfo'Follower
  succ MemberInfo'Follower = MemberInfo'PreLeader
  succ MemberInfo'PreLeader = MemberInfo'Leader
  succ MemberInfo'Leader = MemberInfo'Manager
  succ MemberInfo'Manager = MemberInfo'ShuttingDown
  succ MemberInfo'ShuttingDown = MemberInfo'Shutdown
  succ MemberInfo'Shutdown = MemberInfo'ReadOnlyLeaderless
  succ MemberInfo'ReadOnlyLeaderless = MemberInfo'PreReadOnlyReplica
  succ MemberInfo'PreReadOnlyReplica = MemberInfo'ReadOnlyReplica
  succ MemberInfo'ReadOnlyReplica = MemberInfo'ResigningLeader
  succ (MemberInfo'VNodeState'Unrecognized _)
    = Prelude.error
        "MemberInfo'VNodeState.succ: bad argument: unrecognized value"
  pred MemberInfo'Initializing
    = Prelude.error
        "MemberInfo'VNodeState.pred: bad argument MemberInfo'Initializing. This value would be out of bounds."
  pred MemberInfo'DiscoverLeader = MemberInfo'Initializing
  pred MemberInfo'Unknown = MemberInfo'DiscoverLeader
  pred MemberInfo'PreReplica = MemberInfo'Unknown
  pred MemberInfo'CatchingUp = MemberInfo'PreReplica
  pred MemberInfo'Clone = MemberInfo'CatchingUp
  pred MemberInfo'Follower = MemberInfo'Clone
  pred MemberInfo'PreLeader = MemberInfo'Follower
  pred MemberInfo'Leader = MemberInfo'PreLeader
  pred MemberInfo'Manager = MemberInfo'Leader
  pred MemberInfo'ShuttingDown = MemberInfo'Manager
  pred MemberInfo'Shutdown = MemberInfo'ShuttingDown
  pred MemberInfo'ReadOnlyLeaderless = MemberInfo'Shutdown
  pred MemberInfo'PreReadOnlyReplica = MemberInfo'ReadOnlyLeaderless
  pred MemberInfo'ReadOnlyReplica = MemberInfo'PreReadOnlyReplica
  pred MemberInfo'ResigningLeader = MemberInfo'ReadOnlyReplica
  pred (MemberInfo'VNodeState'Unrecognized _)
    = Prelude.error
        "MemberInfo'VNodeState.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault MemberInfo'VNodeState where
  fieldDefault = MemberInfo'Initializing
instance Control.DeepSeq.NFData MemberInfo'VNodeState where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :
     
         * 'Proto.Cluster_Fields.view' @:: Lens' PrepareOkRequest Data.Int.Int32@
         * 'Proto.Cluster_Fields.serverId' @:: Lens' PrepareOkRequest Proto.Shared.UUID@
         * 'Proto.Cluster_Fields.maybe'serverId' @:: Lens' PrepareOkRequest (Prelude.Maybe Proto.Shared.UUID)@
         * 'Proto.Cluster_Fields.serverHttp' @:: Lens' PrepareOkRequest EndPoint@
         * 'Proto.Cluster_Fields.maybe'serverHttp' @:: Lens' PrepareOkRequest (Prelude.Maybe EndPoint)@
         * 'Proto.Cluster_Fields.epochNumber' @:: Lens' PrepareOkRequest Data.Int.Int32@
         * 'Proto.Cluster_Fields.epochPosition' @:: Lens' PrepareOkRequest Data.Int.Int64@
         * 'Proto.Cluster_Fields.epochId' @:: Lens' PrepareOkRequest Proto.Shared.UUID@
         * 'Proto.Cluster_Fields.maybe'epochId' @:: Lens' PrepareOkRequest (Prelude.Maybe Proto.Shared.UUID)@
         * 'Proto.Cluster_Fields.epochLeaderInstanceId' @:: Lens' PrepareOkRequest Proto.Shared.UUID@
         * 'Proto.Cluster_Fields.maybe'epochLeaderInstanceId' @:: Lens' PrepareOkRequest (Prelude.Maybe Proto.Shared.UUID)@
         * 'Proto.Cluster_Fields.lastCommitPosition' @:: Lens' PrepareOkRequest Data.Int.Int64@
         * 'Proto.Cluster_Fields.writerCheckpoint' @:: Lens' PrepareOkRequest Data.Int.Int64@
         * 'Proto.Cluster_Fields.chaserCheckpoint' @:: Lens' PrepareOkRequest Data.Int.Int64@
         * 'Proto.Cluster_Fields.nodePriority' @:: Lens' PrepareOkRequest Data.Int.Int32@
         * 'Proto.Cluster_Fields.clusterInfo' @:: Lens' PrepareOkRequest ClusterInfo@
         * 'Proto.Cluster_Fields.maybe'clusterInfo' @:: Lens' PrepareOkRequest (Prelude.Maybe ClusterInfo)@ -}
data PrepareOkRequest
  = PrepareOkRequest'_constructor {_PrepareOkRequest'view :: !Data.Int.Int32,
                                   _PrepareOkRequest'serverId :: !(Prelude.Maybe Proto.Shared.UUID),
                                   _PrepareOkRequest'serverHttp :: !(Prelude.Maybe EndPoint),
                                   _PrepareOkRequest'epochNumber :: !Data.Int.Int32,
                                   _PrepareOkRequest'epochPosition :: !Data.Int.Int64,
                                   _PrepareOkRequest'epochId :: !(Prelude.Maybe Proto.Shared.UUID),
                                   _PrepareOkRequest'epochLeaderInstanceId :: !(Prelude.Maybe Proto.Shared.UUID),
                                   _PrepareOkRequest'lastCommitPosition :: !Data.Int.Int64,
                                   _PrepareOkRequest'writerCheckpoint :: !Data.Int.Int64,
                                   _PrepareOkRequest'chaserCheckpoint :: !Data.Int.Int64,
                                   _PrepareOkRequest'nodePriority :: !Data.Int.Int32,
                                   _PrepareOkRequest'clusterInfo :: !(Prelude.Maybe ClusterInfo),
                                   _PrepareOkRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show PrepareOkRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField PrepareOkRequest "view" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrepareOkRequest'view
           (\ x__ y__ -> x__ {_PrepareOkRequest'view = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PrepareOkRequest "serverId" Proto.Shared.UUID where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrepareOkRequest'serverId
           (\ x__ y__ -> x__ {_PrepareOkRequest'serverId = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField PrepareOkRequest "maybe'serverId" (Prelude.Maybe Proto.Shared.UUID) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrepareOkRequest'serverId
           (\ x__ y__ -> x__ {_PrepareOkRequest'serverId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PrepareOkRequest "serverHttp" EndPoint where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrepareOkRequest'serverHttp
           (\ x__ y__ -> x__ {_PrepareOkRequest'serverHttp = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField PrepareOkRequest "maybe'serverHttp" (Prelude.Maybe EndPoint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrepareOkRequest'serverHttp
           (\ x__ y__ -> x__ {_PrepareOkRequest'serverHttp = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PrepareOkRequest "epochNumber" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrepareOkRequest'epochNumber
           (\ x__ y__ -> x__ {_PrepareOkRequest'epochNumber = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PrepareOkRequest "epochPosition" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrepareOkRequest'epochPosition
           (\ x__ y__ -> x__ {_PrepareOkRequest'epochPosition = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PrepareOkRequest "epochId" Proto.Shared.UUID where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrepareOkRequest'epochId
           (\ x__ y__ -> x__ {_PrepareOkRequest'epochId = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField PrepareOkRequest "maybe'epochId" (Prelude.Maybe Proto.Shared.UUID) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrepareOkRequest'epochId
           (\ x__ y__ -> x__ {_PrepareOkRequest'epochId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PrepareOkRequest "epochLeaderInstanceId" Proto.Shared.UUID where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrepareOkRequest'epochLeaderInstanceId
           (\ x__ y__ -> x__ {_PrepareOkRequest'epochLeaderInstanceId = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField PrepareOkRequest "maybe'epochLeaderInstanceId" (Prelude.Maybe Proto.Shared.UUID) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrepareOkRequest'epochLeaderInstanceId
           (\ x__ y__ -> x__ {_PrepareOkRequest'epochLeaderInstanceId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PrepareOkRequest "lastCommitPosition" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrepareOkRequest'lastCommitPosition
           (\ x__ y__ -> x__ {_PrepareOkRequest'lastCommitPosition = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PrepareOkRequest "writerCheckpoint" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrepareOkRequest'writerCheckpoint
           (\ x__ y__ -> x__ {_PrepareOkRequest'writerCheckpoint = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PrepareOkRequest "chaserCheckpoint" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrepareOkRequest'chaserCheckpoint
           (\ x__ y__ -> x__ {_PrepareOkRequest'chaserCheckpoint = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PrepareOkRequest "nodePriority" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrepareOkRequest'nodePriority
           (\ x__ y__ -> x__ {_PrepareOkRequest'nodePriority = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PrepareOkRequest "clusterInfo" ClusterInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrepareOkRequest'clusterInfo
           (\ x__ y__ -> x__ {_PrepareOkRequest'clusterInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField PrepareOkRequest "maybe'clusterInfo" (Prelude.Maybe ClusterInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrepareOkRequest'clusterInfo
           (\ x__ y__ -> x__ {_PrepareOkRequest'clusterInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message PrepareOkRequest where
  messageName _
    = Data.Text.pack "event_store.cluster.PrepareOkRequest"
  packedMessageDescriptor _
    = "\n\
      \\DLEPrepareOkRequest\DC2\DC2\n\
      \\EOTview\CAN\SOH \SOH(\ENQR\EOTview\DC2<\n\
      \\tserver_id\CAN\STX \SOH(\v2\US.event_store.client.shared.UUIDR\bserverId\DC2>\n\
      \\vserver_http\CAN\ETX \SOH(\v2\GS.event_store.cluster.EndPointR\n\
      \serverHttp\DC2!\n\
      \\fepoch_number\CAN\EOT \SOH(\ENQR\vepochNumber\DC2%\n\
      \\SOepoch_position\CAN\ENQ \SOH(\ETXR\repochPosition\DC2:\n\
      \\bepoch_id\CAN\ACK \SOH(\v2\US.event_store.client.shared.UUIDR\aepochId\DC2X\n\
      \\CANepoch_leader_instance_id\CAN\a \SOH(\v2\US.event_store.client.shared.UUIDR\NAKepochLeaderInstanceId\DC20\n\
      \\DC4last_commit_position\CAN\b \SOH(\ETXR\DC2lastCommitPosition\DC2+\n\
      \\DC1writer_checkpoint\CAN\t \SOH(\ETXR\DLEwriterCheckpoint\DC2+\n\
      \\DC1chaser_checkpoint\CAN\n\
      \ \SOH(\ETXR\DLEchaserCheckpoint\DC2#\n\
      \\rnode_priority\CAN\v \SOH(\ENQR\fnodePriority\DC2C\n\
      \\fcluster_info\CAN\f \SOH(\v2 .event_store.cluster.ClusterInfoR\vclusterInfo"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        view__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "view"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"view")) ::
              Data.ProtoLens.FieldDescriptor PrepareOkRequest
        serverId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "server_id"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.UUID)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'serverId")) ::
              Data.ProtoLens.FieldDescriptor PrepareOkRequest
        serverHttp__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "server_http"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor EndPoint)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'serverHttp")) ::
              Data.ProtoLens.FieldDescriptor PrepareOkRequest
        epochNumber__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "epoch_number"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"epochNumber")) ::
              Data.ProtoLens.FieldDescriptor PrepareOkRequest
        epochPosition__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "epoch_position"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"epochPosition")) ::
              Data.ProtoLens.FieldDescriptor PrepareOkRequest
        epochId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "epoch_id"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.UUID)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'epochId")) ::
              Data.ProtoLens.FieldDescriptor PrepareOkRequest
        epochLeaderInstanceId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "epoch_leader_instance_id"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.UUID)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'epochLeaderInstanceId")) ::
              Data.ProtoLens.FieldDescriptor PrepareOkRequest
        lastCommitPosition__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "last_commit_position"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"lastCommitPosition")) ::
              Data.ProtoLens.FieldDescriptor PrepareOkRequest
        writerCheckpoint__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "writer_checkpoint"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"writerCheckpoint")) ::
              Data.ProtoLens.FieldDescriptor PrepareOkRequest
        chaserCheckpoint__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "chaser_checkpoint"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"chaserCheckpoint")) ::
              Data.ProtoLens.FieldDescriptor PrepareOkRequest
        nodePriority__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "node_priority"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"nodePriority")) ::
              Data.ProtoLens.FieldDescriptor PrepareOkRequest
        clusterInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "cluster_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ClusterInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'clusterInfo")) ::
              Data.ProtoLens.FieldDescriptor PrepareOkRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, view__field_descriptor),
           (Data.ProtoLens.Tag 2, serverId__field_descriptor),
           (Data.ProtoLens.Tag 3, serverHttp__field_descriptor),
           (Data.ProtoLens.Tag 4, epochNumber__field_descriptor),
           (Data.ProtoLens.Tag 5, epochPosition__field_descriptor),
           (Data.ProtoLens.Tag 6, epochId__field_descriptor),
           (Data.ProtoLens.Tag 7, epochLeaderInstanceId__field_descriptor),
           (Data.ProtoLens.Tag 8, lastCommitPosition__field_descriptor),
           (Data.ProtoLens.Tag 9, writerCheckpoint__field_descriptor),
           (Data.ProtoLens.Tag 10, chaserCheckpoint__field_descriptor),
           (Data.ProtoLens.Tag 11, nodePriority__field_descriptor),
           (Data.ProtoLens.Tag 12, clusterInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _PrepareOkRequest'_unknownFields
        (\ x__ y__ -> x__ {_PrepareOkRequest'_unknownFields = y__})
  defMessage
    = PrepareOkRequest'_constructor
        {_PrepareOkRequest'view = Data.ProtoLens.fieldDefault,
         _PrepareOkRequest'serverId = Prelude.Nothing,
         _PrepareOkRequest'serverHttp = Prelude.Nothing,
         _PrepareOkRequest'epochNumber = Data.ProtoLens.fieldDefault,
         _PrepareOkRequest'epochPosition = Data.ProtoLens.fieldDefault,
         _PrepareOkRequest'epochId = Prelude.Nothing,
         _PrepareOkRequest'epochLeaderInstanceId = Prelude.Nothing,
         _PrepareOkRequest'lastCommitPosition = Data.ProtoLens.fieldDefault,
         _PrepareOkRequest'writerCheckpoint = Data.ProtoLens.fieldDefault,
         _PrepareOkRequest'chaserCheckpoint = Data.ProtoLens.fieldDefault,
         _PrepareOkRequest'nodePriority = Data.ProtoLens.fieldDefault,
         _PrepareOkRequest'clusterInfo = Prelude.Nothing,
         _PrepareOkRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          PrepareOkRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser PrepareOkRequest
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "view"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"view") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "server_id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"serverId") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "server_http"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"serverHttp") y x)
                        32
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "epoch_number"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"epochNumber") y x)
                        40
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "epoch_position"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"epochPosition") y x)
                        50
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "epoch_id"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"epochId") y x)
                        58
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "epoch_leader_instance_id"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"epochLeaderInstanceId") y x)
                        64
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "last_commit_position"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"lastCommitPosition") y x)
                        72
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "writer_checkpoint"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"writerCheckpoint") y x)
                        80
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "chaser_checkpoint"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"chaserCheckpoint") y x)
                        88
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "node_priority"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"nodePriority") y x)
                        98
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "cluster_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"clusterInfo") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "PrepareOkRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"view") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'serverId") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             _v))
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view
                          (Data.ProtoLens.Field.field @"maybe'serverHttp") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just _v)
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage
                                _v))
                   ((Data.Monoid.<>)
                      (let
                         _v
                           = Lens.Family2.view (Data.ProtoLens.Field.field @"epochNumber") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 32)
                               ((Prelude..)
                                  Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                      ((Data.Monoid.<>)
                         (let
                            _v
                              = Lens.Family2.view
                                  (Data.ProtoLens.Field.field @"epochPosition") _x
                          in
                            if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                Data.Monoid.mempty
                            else
                                (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 40)
                                  ((Prelude..)
                                     Data.ProtoLens.Encoding.Bytes.putVarInt
                                     Prelude.fromIntegral
                                     _v))
                         ((Data.Monoid.<>)
                            (case
                                 Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'epochId") _x
                             of
                               Prelude.Nothing -> Data.Monoid.mempty
                               (Prelude.Just _v)
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt 50)
                                      ((Prelude..)
                                         (\ bs
                                            -> (Data.Monoid.<>)
                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                    (Prelude.fromIntegral
                                                       (Data.ByteString.length bs)))
                                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                         Data.ProtoLens.encodeMessage
                                         _v))
                            ((Data.Monoid.<>)
                               (case
                                    Lens.Family2.view
                                      (Data.ProtoLens.Field.field @"maybe'epochLeaderInstanceId") _x
                                of
                                  Prelude.Nothing -> Data.Monoid.mempty
                                  (Prelude.Just _v)
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt 58)
                                         ((Prelude..)
                                            (\ bs
                                               -> (Data.Monoid.<>)
                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                       (Prelude.fromIntegral
                                                          (Data.ByteString.length bs)))
                                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                            Data.ProtoLens.encodeMessage
                                            _v))
                               ((Data.Monoid.<>)
                                  (let
                                     _v
                                       = Lens.Family2.view
                                           (Data.ProtoLens.Field.field @"lastCommitPosition") _x
                                   in
                                     if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                         Data.Monoid.mempty
                                     else
                                         (Data.Monoid.<>)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt 64)
                                           ((Prelude..)
                                              Data.ProtoLens.Encoding.Bytes.putVarInt
                                              Prelude.fromIntegral
                                              _v))
                                  ((Data.Monoid.<>)
                                     (let
                                        _v
                                          = Lens.Family2.view
                                              (Data.ProtoLens.Field.field @"writerCheckpoint") _x
                                      in
                                        if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                            Data.Monoid.mempty
                                        else
                                            (Data.Monoid.<>)
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt 72)
                                              ((Prelude..)
                                                 Data.ProtoLens.Encoding.Bytes.putVarInt
                                                 Prelude.fromIntegral
                                                 _v))
                                     ((Data.Monoid.<>)
                                        (let
                                           _v
                                             = Lens.Family2.view
                                                 (Data.ProtoLens.Field.field @"chaserCheckpoint") _x
                                         in
                                           if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                               Data.Monoid.mempty
                                           else
                                               (Data.Monoid.<>)
                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 80)
                                                 ((Prelude..)
                                                    Data.ProtoLens.Encoding.Bytes.putVarInt
                                                    Prelude.fromIntegral
                                                    _v))
                                        ((Data.Monoid.<>)
                                           (let
                                              _v
                                                = Lens.Family2.view
                                                    (Data.ProtoLens.Field.field @"nodePriority") _x
                                            in
                                              if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                                  Data.Monoid.mempty
                                              else
                                                  (Data.Monoid.<>)
                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt 88)
                                                    ((Prelude..)
                                                       Data.ProtoLens.Encoding.Bytes.putVarInt
                                                       Prelude.fromIntegral
                                                       _v))
                                           ((Data.Monoid.<>)
                                              (case
                                                   Lens.Family2.view
                                                     (Data.ProtoLens.Field.field
                                                        @"maybe'clusterInfo")
                                                     _x
                                               of
                                                 Prelude.Nothing -> Data.Monoid.mempty
                                                 (Prelude.Just _v)
                                                   -> (Data.Monoid.<>)
                                                        (Data.ProtoLens.Encoding.Bytes.putVarInt 98)
                                                        ((Prelude..)
                                                           (\ bs
                                                              -> (Data.Monoid.<>)
                                                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                      (Prelude.fromIntegral
                                                                         (Data.ByteString.length
                                                                            bs)))
                                                                   (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                      bs))
                                                           Data.ProtoLens.encodeMessage
                                                           _v))
                                              (Data.ProtoLens.Encoding.Wire.buildFieldSet
                                                 (Lens.Family2.view
                                                    Data.ProtoLens.unknownFields _x)))))))))))))
instance Control.DeepSeq.NFData PrepareOkRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_PrepareOkRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_PrepareOkRequest'view x__)
                (Control.DeepSeq.deepseq
                   (_PrepareOkRequest'serverId x__)
                   (Control.DeepSeq.deepseq
                      (_PrepareOkRequest'serverHttp x__)
                      (Control.DeepSeq.deepseq
                         (_PrepareOkRequest'epochNumber x__)
                         (Control.DeepSeq.deepseq
                            (_PrepareOkRequest'epochPosition x__)
                            (Control.DeepSeq.deepseq
                               (_PrepareOkRequest'epochId x__)
                               (Control.DeepSeq.deepseq
                                  (_PrepareOkRequest'epochLeaderInstanceId x__)
                                  (Control.DeepSeq.deepseq
                                     (_PrepareOkRequest'lastCommitPosition x__)
                                     (Control.DeepSeq.deepseq
                                        (_PrepareOkRequest'writerCheckpoint x__)
                                        (Control.DeepSeq.deepseq
                                           (_PrepareOkRequest'chaserCheckpoint x__)
                                           (Control.DeepSeq.deepseq
                                              (_PrepareOkRequest'nodePriority x__)
                                              (Control.DeepSeq.deepseq
                                                 (_PrepareOkRequest'clusterInfo x__) ()))))))))))))
{- | Fields :
     
         * 'Proto.Cluster_Fields.serverId' @:: Lens' PrepareRequest Proto.Shared.UUID@
         * 'Proto.Cluster_Fields.maybe'serverId' @:: Lens' PrepareRequest (Prelude.Maybe Proto.Shared.UUID)@
         * 'Proto.Cluster_Fields.serverHttp' @:: Lens' PrepareRequest EndPoint@
         * 'Proto.Cluster_Fields.maybe'serverHttp' @:: Lens' PrepareRequest (Prelude.Maybe EndPoint)@
         * 'Proto.Cluster_Fields.view' @:: Lens' PrepareRequest Data.Int.Int32@ -}
data PrepareRequest
  = PrepareRequest'_constructor {_PrepareRequest'serverId :: !(Prelude.Maybe Proto.Shared.UUID),
                                 _PrepareRequest'serverHttp :: !(Prelude.Maybe EndPoint),
                                 _PrepareRequest'view :: !Data.Int.Int32,
                                 _PrepareRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show PrepareRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField PrepareRequest "serverId" Proto.Shared.UUID where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrepareRequest'serverId
           (\ x__ y__ -> x__ {_PrepareRequest'serverId = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField PrepareRequest "maybe'serverId" (Prelude.Maybe Proto.Shared.UUID) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrepareRequest'serverId
           (\ x__ y__ -> x__ {_PrepareRequest'serverId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PrepareRequest "serverHttp" EndPoint where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrepareRequest'serverHttp
           (\ x__ y__ -> x__ {_PrepareRequest'serverHttp = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField PrepareRequest "maybe'serverHttp" (Prelude.Maybe EndPoint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrepareRequest'serverHttp
           (\ x__ y__ -> x__ {_PrepareRequest'serverHttp = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PrepareRequest "view" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrepareRequest'view
           (\ x__ y__ -> x__ {_PrepareRequest'view = y__}))
        Prelude.id
instance Data.ProtoLens.Message PrepareRequest where
  messageName _ = Data.Text.pack "event_store.cluster.PrepareRequest"
  packedMessageDescriptor _
    = "\n\
      \\SOPrepareRequest\DC2<\n\
      \\tserver_id\CAN\SOH \SOH(\v2\US.event_store.client.shared.UUIDR\bserverId\DC2>\n\
      \\vserver_http\CAN\STX \SOH(\v2\GS.event_store.cluster.EndPointR\n\
      \serverHttp\DC2\DC2\n\
      \\EOTview\CAN\ETX \SOH(\ENQR\EOTview"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        serverId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "server_id"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.UUID)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'serverId")) ::
              Data.ProtoLens.FieldDescriptor PrepareRequest
        serverHttp__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "server_http"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor EndPoint)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'serverHttp")) ::
              Data.ProtoLens.FieldDescriptor PrepareRequest
        view__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "view"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"view")) ::
              Data.ProtoLens.FieldDescriptor PrepareRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, serverId__field_descriptor),
           (Data.ProtoLens.Tag 2, serverHttp__field_descriptor),
           (Data.ProtoLens.Tag 3, view__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _PrepareRequest'_unknownFields
        (\ x__ y__ -> x__ {_PrepareRequest'_unknownFields = y__})
  defMessage
    = PrepareRequest'_constructor
        {_PrepareRequest'serverId = Prelude.Nothing,
         _PrepareRequest'serverHttp = Prelude.Nothing,
         _PrepareRequest'view = Data.ProtoLens.fieldDefault,
         _PrepareRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          PrepareRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser PrepareRequest
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "server_id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"serverId") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "server_http"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"serverHttp") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "view"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"view") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "PrepareRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'serverId") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage
                          _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'serverHttp") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             _v))
                ((Data.Monoid.<>)
                   (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"view") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            ((Prelude..)
                               Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData PrepareRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_PrepareRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_PrepareRequest'serverId x__)
                (Control.DeepSeq.deepseq
                   (_PrepareRequest'serverHttp x__)
                   (Control.DeepSeq.deepseq (_PrepareRequest'view x__) ())))
{- | Fields :
     
         * 'Proto.Cluster_Fields.serverId' @:: Lens' ProposalRequest Proto.Shared.UUID@
         * 'Proto.Cluster_Fields.maybe'serverId' @:: Lens' ProposalRequest (Prelude.Maybe Proto.Shared.UUID)@
         * 'Proto.Cluster_Fields.serverHttp' @:: Lens' ProposalRequest EndPoint@
         * 'Proto.Cluster_Fields.maybe'serverHttp' @:: Lens' ProposalRequest (Prelude.Maybe EndPoint)@
         * 'Proto.Cluster_Fields.leaderId' @:: Lens' ProposalRequest Proto.Shared.UUID@
         * 'Proto.Cluster_Fields.maybe'leaderId' @:: Lens' ProposalRequest (Prelude.Maybe Proto.Shared.UUID)@
         * 'Proto.Cluster_Fields.leaderHttp' @:: Lens' ProposalRequest EndPoint@
         * 'Proto.Cluster_Fields.maybe'leaderHttp' @:: Lens' ProposalRequest (Prelude.Maybe EndPoint)@
         * 'Proto.Cluster_Fields.view' @:: Lens' ProposalRequest Data.Int.Int32@
         * 'Proto.Cluster_Fields.epochNumber' @:: Lens' ProposalRequest Data.Int.Int32@
         * 'Proto.Cluster_Fields.epochPosition' @:: Lens' ProposalRequest Data.Int.Int64@
         * 'Proto.Cluster_Fields.epochId' @:: Lens' ProposalRequest Proto.Shared.UUID@
         * 'Proto.Cluster_Fields.maybe'epochId' @:: Lens' ProposalRequest (Prelude.Maybe Proto.Shared.UUID)@
         * 'Proto.Cluster_Fields.epochLeaderInstanceId' @:: Lens' ProposalRequest Proto.Shared.UUID@
         * 'Proto.Cluster_Fields.maybe'epochLeaderInstanceId' @:: Lens' ProposalRequest (Prelude.Maybe Proto.Shared.UUID)@
         * 'Proto.Cluster_Fields.lastCommitPosition' @:: Lens' ProposalRequest Data.Int.Int64@
         * 'Proto.Cluster_Fields.writerCheckpoint' @:: Lens' ProposalRequest Data.Int.Int64@
         * 'Proto.Cluster_Fields.chaserCheckpoint' @:: Lens' ProposalRequest Data.Int.Int64@
         * 'Proto.Cluster_Fields.nodePriority' @:: Lens' ProposalRequest Data.Int.Int32@ -}
data ProposalRequest
  = ProposalRequest'_constructor {_ProposalRequest'serverId :: !(Prelude.Maybe Proto.Shared.UUID),
                                  _ProposalRequest'serverHttp :: !(Prelude.Maybe EndPoint),
                                  _ProposalRequest'leaderId :: !(Prelude.Maybe Proto.Shared.UUID),
                                  _ProposalRequest'leaderHttp :: !(Prelude.Maybe EndPoint),
                                  _ProposalRequest'view :: !Data.Int.Int32,
                                  _ProposalRequest'epochNumber :: !Data.Int.Int32,
                                  _ProposalRequest'epochPosition :: !Data.Int.Int64,
                                  _ProposalRequest'epochId :: !(Prelude.Maybe Proto.Shared.UUID),
                                  _ProposalRequest'epochLeaderInstanceId :: !(Prelude.Maybe Proto.Shared.UUID),
                                  _ProposalRequest'lastCommitPosition :: !Data.Int.Int64,
                                  _ProposalRequest'writerCheckpoint :: !Data.Int.Int64,
                                  _ProposalRequest'chaserCheckpoint :: !Data.Int.Int64,
                                  _ProposalRequest'nodePriority :: !Data.Int.Int32,
                                  _ProposalRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ProposalRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ProposalRequest "serverId" Proto.Shared.UUID where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProposalRequest'serverId
           (\ x__ y__ -> x__ {_ProposalRequest'serverId = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ProposalRequest "maybe'serverId" (Prelude.Maybe Proto.Shared.UUID) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProposalRequest'serverId
           (\ x__ y__ -> x__ {_ProposalRequest'serverId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ProposalRequest "serverHttp" EndPoint where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProposalRequest'serverHttp
           (\ x__ y__ -> x__ {_ProposalRequest'serverHttp = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ProposalRequest "maybe'serverHttp" (Prelude.Maybe EndPoint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProposalRequest'serverHttp
           (\ x__ y__ -> x__ {_ProposalRequest'serverHttp = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ProposalRequest "leaderId" Proto.Shared.UUID where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProposalRequest'leaderId
           (\ x__ y__ -> x__ {_ProposalRequest'leaderId = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ProposalRequest "maybe'leaderId" (Prelude.Maybe Proto.Shared.UUID) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProposalRequest'leaderId
           (\ x__ y__ -> x__ {_ProposalRequest'leaderId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ProposalRequest "leaderHttp" EndPoint where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProposalRequest'leaderHttp
           (\ x__ y__ -> x__ {_ProposalRequest'leaderHttp = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ProposalRequest "maybe'leaderHttp" (Prelude.Maybe EndPoint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProposalRequest'leaderHttp
           (\ x__ y__ -> x__ {_ProposalRequest'leaderHttp = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ProposalRequest "view" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProposalRequest'view
           (\ x__ y__ -> x__ {_ProposalRequest'view = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ProposalRequest "epochNumber" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProposalRequest'epochNumber
           (\ x__ y__ -> x__ {_ProposalRequest'epochNumber = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ProposalRequest "epochPosition" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProposalRequest'epochPosition
           (\ x__ y__ -> x__ {_ProposalRequest'epochPosition = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ProposalRequest "epochId" Proto.Shared.UUID where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProposalRequest'epochId
           (\ x__ y__ -> x__ {_ProposalRequest'epochId = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ProposalRequest "maybe'epochId" (Prelude.Maybe Proto.Shared.UUID) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProposalRequest'epochId
           (\ x__ y__ -> x__ {_ProposalRequest'epochId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ProposalRequest "epochLeaderInstanceId" Proto.Shared.UUID where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProposalRequest'epochLeaderInstanceId
           (\ x__ y__ -> x__ {_ProposalRequest'epochLeaderInstanceId = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ProposalRequest "maybe'epochLeaderInstanceId" (Prelude.Maybe Proto.Shared.UUID) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProposalRequest'epochLeaderInstanceId
           (\ x__ y__ -> x__ {_ProposalRequest'epochLeaderInstanceId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ProposalRequest "lastCommitPosition" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProposalRequest'lastCommitPosition
           (\ x__ y__ -> x__ {_ProposalRequest'lastCommitPosition = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ProposalRequest "writerCheckpoint" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProposalRequest'writerCheckpoint
           (\ x__ y__ -> x__ {_ProposalRequest'writerCheckpoint = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ProposalRequest "chaserCheckpoint" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProposalRequest'chaserCheckpoint
           (\ x__ y__ -> x__ {_ProposalRequest'chaserCheckpoint = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ProposalRequest "nodePriority" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProposalRequest'nodePriority
           (\ x__ y__ -> x__ {_ProposalRequest'nodePriority = y__}))
        Prelude.id
instance Data.ProtoLens.Message ProposalRequest where
  messageName _
    = Data.Text.pack "event_store.cluster.ProposalRequest"
  packedMessageDescriptor _
    = "\n\
      \\SIProposalRequest\DC2<\n\
      \\tserver_id\CAN\SOH \SOH(\v2\US.event_store.client.shared.UUIDR\bserverId\DC2>\n\
      \\vserver_http\CAN\STX \SOH(\v2\GS.event_store.cluster.EndPointR\n\
      \serverHttp\DC2<\n\
      \\tleader_id\CAN\ETX \SOH(\v2\US.event_store.client.shared.UUIDR\bleaderId\DC2>\n\
      \\vleader_http\CAN\EOT \SOH(\v2\GS.event_store.cluster.EndPointR\n\
      \leaderHttp\DC2\DC2\n\
      \\EOTview\CAN\ENQ \SOH(\ENQR\EOTview\DC2!\n\
      \\fepoch_number\CAN\ACK \SOH(\ENQR\vepochNumber\DC2%\n\
      \\SOepoch_position\CAN\a \SOH(\ETXR\repochPosition\DC2:\n\
      \\bepoch_id\CAN\b \SOH(\v2\US.event_store.client.shared.UUIDR\aepochId\DC2X\n\
      \\CANepoch_leader_instance_id\CAN\t \SOH(\v2\US.event_store.client.shared.UUIDR\NAKepochLeaderInstanceId\DC20\n\
      \\DC4last_commit_position\CAN\n\
      \ \SOH(\ETXR\DC2lastCommitPosition\DC2+\n\
      \\DC1writer_checkpoint\CAN\v \SOH(\ETXR\DLEwriterCheckpoint\DC2+\n\
      \\DC1chaser_checkpoint\CAN\f \SOH(\ETXR\DLEchaserCheckpoint\DC2#\n\
      \\rnode_priority\CAN\r \SOH(\ENQR\fnodePriority"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        serverId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "server_id"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.UUID)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'serverId")) ::
              Data.ProtoLens.FieldDescriptor ProposalRequest
        serverHttp__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "server_http"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor EndPoint)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'serverHttp")) ::
              Data.ProtoLens.FieldDescriptor ProposalRequest
        leaderId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "leader_id"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.UUID)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'leaderId")) ::
              Data.ProtoLens.FieldDescriptor ProposalRequest
        leaderHttp__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "leader_http"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor EndPoint)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'leaderHttp")) ::
              Data.ProtoLens.FieldDescriptor ProposalRequest
        view__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "view"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"view")) ::
              Data.ProtoLens.FieldDescriptor ProposalRequest
        epochNumber__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "epoch_number"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"epochNumber")) ::
              Data.ProtoLens.FieldDescriptor ProposalRequest
        epochPosition__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "epoch_position"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"epochPosition")) ::
              Data.ProtoLens.FieldDescriptor ProposalRequest
        epochId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "epoch_id"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.UUID)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'epochId")) ::
              Data.ProtoLens.FieldDescriptor ProposalRequest
        epochLeaderInstanceId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "epoch_leader_instance_id"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.UUID)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'epochLeaderInstanceId")) ::
              Data.ProtoLens.FieldDescriptor ProposalRequest
        lastCommitPosition__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "last_commit_position"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"lastCommitPosition")) ::
              Data.ProtoLens.FieldDescriptor ProposalRequest
        writerCheckpoint__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "writer_checkpoint"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"writerCheckpoint")) ::
              Data.ProtoLens.FieldDescriptor ProposalRequest
        chaserCheckpoint__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "chaser_checkpoint"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"chaserCheckpoint")) ::
              Data.ProtoLens.FieldDescriptor ProposalRequest
        nodePriority__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "node_priority"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"nodePriority")) ::
              Data.ProtoLens.FieldDescriptor ProposalRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, serverId__field_descriptor),
           (Data.ProtoLens.Tag 2, serverHttp__field_descriptor),
           (Data.ProtoLens.Tag 3, leaderId__field_descriptor),
           (Data.ProtoLens.Tag 4, leaderHttp__field_descriptor),
           (Data.ProtoLens.Tag 5, view__field_descriptor),
           (Data.ProtoLens.Tag 6, epochNumber__field_descriptor),
           (Data.ProtoLens.Tag 7, epochPosition__field_descriptor),
           (Data.ProtoLens.Tag 8, epochId__field_descriptor),
           (Data.ProtoLens.Tag 9, epochLeaderInstanceId__field_descriptor),
           (Data.ProtoLens.Tag 10, lastCommitPosition__field_descriptor),
           (Data.ProtoLens.Tag 11, writerCheckpoint__field_descriptor),
           (Data.ProtoLens.Tag 12, chaserCheckpoint__field_descriptor),
           (Data.ProtoLens.Tag 13, nodePriority__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ProposalRequest'_unknownFields
        (\ x__ y__ -> x__ {_ProposalRequest'_unknownFields = y__})
  defMessage
    = ProposalRequest'_constructor
        {_ProposalRequest'serverId = Prelude.Nothing,
         _ProposalRequest'serverHttp = Prelude.Nothing,
         _ProposalRequest'leaderId = Prelude.Nothing,
         _ProposalRequest'leaderHttp = Prelude.Nothing,
         _ProposalRequest'view = Data.ProtoLens.fieldDefault,
         _ProposalRequest'epochNumber = Data.ProtoLens.fieldDefault,
         _ProposalRequest'epochPosition = Data.ProtoLens.fieldDefault,
         _ProposalRequest'epochId = Prelude.Nothing,
         _ProposalRequest'epochLeaderInstanceId = Prelude.Nothing,
         _ProposalRequest'lastCommitPosition = Data.ProtoLens.fieldDefault,
         _ProposalRequest'writerCheckpoint = Data.ProtoLens.fieldDefault,
         _ProposalRequest'chaserCheckpoint = Data.ProtoLens.fieldDefault,
         _ProposalRequest'nodePriority = Data.ProtoLens.fieldDefault,
         _ProposalRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ProposalRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser ProposalRequest
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "server_id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"serverId") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "server_http"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"serverHttp") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "leader_id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"leaderId") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "leader_http"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"leaderHttp") y x)
                        40
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "view"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"view") y x)
                        48
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "epoch_number"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"epochNumber") y x)
                        56
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "epoch_position"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"epochPosition") y x)
                        66
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "epoch_id"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"epochId") y x)
                        74
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "epoch_leader_instance_id"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"epochLeaderInstanceId") y x)
                        80
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "last_commit_position"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"lastCommitPosition") y x)
                        88
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "writer_checkpoint"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"writerCheckpoint") y x)
                        96
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "chaser_checkpoint"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"chaserCheckpoint") y x)
                        104
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "node_priority"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"nodePriority") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ProposalRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'serverId") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage
                          _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'serverHttp") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             _v))
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'leaderId") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just _v)
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage
                                _v))
                   ((Data.Monoid.<>)
                      (case
                           Lens.Family2.view
                             (Data.ProtoLens.Field.field @"maybe'leaderHttp") _x
                       of
                         Prelude.Nothing -> Data.Monoid.mempty
                         (Prelude.Just _v)
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                ((Prelude..)
                                   (\ bs
                                      -> (Data.Monoid.<>)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                                           (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                   Data.ProtoLens.encodeMessage
                                   _v))
                      ((Data.Monoid.<>)
                         (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"view") _x
                          in
                            if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                Data.Monoid.mempty
                            else
                                (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 40)
                                  ((Prelude..)
                                     Data.ProtoLens.Encoding.Bytes.putVarInt
                                     Prelude.fromIntegral
                                     _v))
                         ((Data.Monoid.<>)
                            (let
                               _v
                                 = Lens.Family2.view (Data.ProtoLens.Field.field @"epochNumber") _x
                             in
                               if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                   Data.Monoid.mempty
                               else
                                   (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt 48)
                                     ((Prelude..)
                                        Data.ProtoLens.Encoding.Bytes.putVarInt
                                        Prelude.fromIntegral
                                        _v))
                            ((Data.Monoid.<>)
                               (let
                                  _v
                                    = Lens.Family2.view
                                        (Data.ProtoLens.Field.field @"epochPosition") _x
                                in
                                  if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                      Data.Monoid.mempty
                                  else
                                      (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt 56)
                                        ((Prelude..)
                                           Data.ProtoLens.Encoding.Bytes.putVarInt
                                           Prelude.fromIntegral
                                           _v))
                               ((Data.Monoid.<>)
                                  (case
                                       Lens.Family2.view
                                         (Data.ProtoLens.Field.field @"maybe'epochId") _x
                                   of
                                     Prelude.Nothing -> Data.Monoid.mempty
                                     (Prelude.Just _v)
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt 66)
                                            ((Prelude..)
                                               (\ bs
                                                  -> (Data.Monoid.<>)
                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                          (Prelude.fromIntegral
                                                             (Data.ByteString.length bs)))
                                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                               Data.ProtoLens.encodeMessage
                                               _v))
                                  ((Data.Monoid.<>)
                                     (case
                                          Lens.Family2.view
                                            (Data.ProtoLens.Field.field
                                               @"maybe'epochLeaderInstanceId")
                                            _x
                                      of
                                        Prelude.Nothing -> Data.Monoid.mempty
                                        (Prelude.Just _v)
                                          -> (Data.Monoid.<>)
                                               (Data.ProtoLens.Encoding.Bytes.putVarInt 74)
                                               ((Prelude..)
                                                  (\ bs
                                                     -> (Data.Monoid.<>)
                                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                             (Prelude.fromIntegral
                                                                (Data.ByteString.length bs)))
                                                          (Data.ProtoLens.Encoding.Bytes.putBytes
                                                             bs))
                                                  Data.ProtoLens.encodeMessage
                                                  _v))
                                     ((Data.Monoid.<>)
                                        (let
                                           _v
                                             = Lens.Family2.view
                                                 (Data.ProtoLens.Field.field @"lastCommitPosition")
                                                 _x
                                         in
                                           if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                               Data.Monoid.mempty
                                           else
                                               (Data.Monoid.<>)
                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 80)
                                                 ((Prelude..)
                                                    Data.ProtoLens.Encoding.Bytes.putVarInt
                                                    Prelude.fromIntegral
                                                    _v))
                                        ((Data.Monoid.<>)
                                           (let
                                              _v
                                                = Lens.Family2.view
                                                    (Data.ProtoLens.Field.field @"writerCheckpoint")
                                                    _x
                                            in
                                              if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                                  Data.Monoid.mempty
                                              else
                                                  (Data.Monoid.<>)
                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt 88)
                                                    ((Prelude..)
                                                       Data.ProtoLens.Encoding.Bytes.putVarInt
                                                       Prelude.fromIntegral
                                                       _v))
                                           ((Data.Monoid.<>)
                                              (let
                                                 _v
                                                   = Lens.Family2.view
                                                       (Data.ProtoLens.Field.field
                                                          @"chaserCheckpoint")
                                                       _x
                                               in
                                                 if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                                     Data.Monoid.mempty
                                                 else
                                                     (Data.Monoid.<>)
                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt 96)
                                                       ((Prelude..)
                                                          Data.ProtoLens.Encoding.Bytes.putVarInt
                                                          Prelude.fromIntegral
                                                          _v))
                                              ((Data.Monoid.<>)
                                                 (let
                                                    _v
                                                      = Lens.Family2.view
                                                          (Data.ProtoLens.Field.field
                                                             @"nodePriority")
                                                          _x
                                                  in
                                                    if (Prelude.==)
                                                         _v Data.ProtoLens.fieldDefault then
                                                        Data.Monoid.mempty
                                                    else
                                                        (Data.Monoid.<>)
                                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                             104)
                                                          ((Prelude..)
                                                             Data.ProtoLens.Encoding.Bytes.putVarInt
                                                             Prelude.fromIntegral
                                                             _v))
                                                 (Data.ProtoLens.Encoding.Wire.buildFieldSet
                                                    (Lens.Family2.view
                                                       Data.ProtoLens.unknownFields _x))))))))))))))
instance Control.DeepSeq.NFData ProposalRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ProposalRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ProposalRequest'serverId x__)
                (Control.DeepSeq.deepseq
                   (_ProposalRequest'serverHttp x__)
                   (Control.DeepSeq.deepseq
                      (_ProposalRequest'leaderId x__)
                      (Control.DeepSeq.deepseq
                         (_ProposalRequest'leaderHttp x__)
                         (Control.DeepSeq.deepseq
                            (_ProposalRequest'view x__)
                            (Control.DeepSeq.deepseq
                               (_ProposalRequest'epochNumber x__)
                               (Control.DeepSeq.deepseq
                                  (_ProposalRequest'epochPosition x__)
                                  (Control.DeepSeq.deepseq
                                     (_ProposalRequest'epochId x__)
                                     (Control.DeepSeq.deepseq
                                        (_ProposalRequest'epochLeaderInstanceId x__)
                                        (Control.DeepSeq.deepseq
                                           (_ProposalRequest'lastCommitPosition x__)
                                           (Control.DeepSeq.deepseq
                                              (_ProposalRequest'writerCheckpoint x__)
                                              (Control.DeepSeq.deepseq
                                                 (_ProposalRequest'chaserCheckpoint x__)
                                                 (Control.DeepSeq.deepseq
                                                    (_ProposalRequest'nodePriority x__)
                                                    ())))))))))))))
{- | Fields :
     
         * 'Proto.Cluster_Fields.serverId' @:: Lens' ViewChangeProofRequest Proto.Shared.UUID@
         * 'Proto.Cluster_Fields.maybe'serverId' @:: Lens' ViewChangeProofRequest (Prelude.Maybe Proto.Shared.UUID)@
         * 'Proto.Cluster_Fields.serverHttp' @:: Lens' ViewChangeProofRequest EndPoint@
         * 'Proto.Cluster_Fields.maybe'serverHttp' @:: Lens' ViewChangeProofRequest (Prelude.Maybe EndPoint)@
         * 'Proto.Cluster_Fields.installedView' @:: Lens' ViewChangeProofRequest Data.Int.Int32@ -}
data ViewChangeProofRequest
  = ViewChangeProofRequest'_constructor {_ViewChangeProofRequest'serverId :: !(Prelude.Maybe Proto.Shared.UUID),
                                         _ViewChangeProofRequest'serverHttp :: !(Prelude.Maybe EndPoint),
                                         _ViewChangeProofRequest'installedView :: !Data.Int.Int32,
                                         _ViewChangeProofRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ViewChangeProofRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ViewChangeProofRequest "serverId" Proto.Shared.UUID where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ViewChangeProofRequest'serverId
           (\ x__ y__ -> x__ {_ViewChangeProofRequest'serverId = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ViewChangeProofRequest "maybe'serverId" (Prelude.Maybe Proto.Shared.UUID) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ViewChangeProofRequest'serverId
           (\ x__ y__ -> x__ {_ViewChangeProofRequest'serverId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ViewChangeProofRequest "serverHttp" EndPoint where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ViewChangeProofRequest'serverHttp
           (\ x__ y__ -> x__ {_ViewChangeProofRequest'serverHttp = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ViewChangeProofRequest "maybe'serverHttp" (Prelude.Maybe EndPoint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ViewChangeProofRequest'serverHttp
           (\ x__ y__ -> x__ {_ViewChangeProofRequest'serverHttp = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ViewChangeProofRequest "installedView" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ViewChangeProofRequest'installedView
           (\ x__ y__ -> x__ {_ViewChangeProofRequest'installedView = y__}))
        Prelude.id
instance Data.ProtoLens.Message ViewChangeProofRequest where
  messageName _
    = Data.Text.pack "event_store.cluster.ViewChangeProofRequest"
  packedMessageDescriptor _
    = "\n\
      \\SYNViewChangeProofRequest\DC2<\n\
      \\tserver_id\CAN\SOH \SOH(\v2\US.event_store.client.shared.UUIDR\bserverId\DC2>\n\
      \\vserver_http\CAN\STX \SOH(\v2\GS.event_store.cluster.EndPointR\n\
      \serverHttp\DC2%\n\
      \\SOinstalled_view\CAN\ETX \SOH(\ENQR\rinstalledView"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        serverId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "server_id"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.UUID)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'serverId")) ::
              Data.ProtoLens.FieldDescriptor ViewChangeProofRequest
        serverHttp__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "server_http"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor EndPoint)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'serverHttp")) ::
              Data.ProtoLens.FieldDescriptor ViewChangeProofRequest
        installedView__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "installed_view"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"installedView")) ::
              Data.ProtoLens.FieldDescriptor ViewChangeProofRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, serverId__field_descriptor),
           (Data.ProtoLens.Tag 2, serverHttp__field_descriptor),
           (Data.ProtoLens.Tag 3, installedView__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ViewChangeProofRequest'_unknownFields
        (\ x__ y__ -> x__ {_ViewChangeProofRequest'_unknownFields = y__})
  defMessage
    = ViewChangeProofRequest'_constructor
        {_ViewChangeProofRequest'serverId = Prelude.Nothing,
         _ViewChangeProofRequest'serverHttp = Prelude.Nothing,
         _ViewChangeProofRequest'installedView = Data.ProtoLens.fieldDefault,
         _ViewChangeProofRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ViewChangeProofRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser ViewChangeProofRequest
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "server_id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"serverId") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "server_http"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"serverHttp") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "installed_view"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"installedView") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ViewChangeProofRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'serverId") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage
                          _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'serverHttp") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             _v))
                ((Data.Monoid.<>)
                   (let
                      _v
                        = Lens.Family2.view
                            (Data.ProtoLens.Field.field @"installedView") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            ((Prelude..)
                               Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData ViewChangeProofRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ViewChangeProofRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ViewChangeProofRequest'serverId x__)
                (Control.DeepSeq.deepseq
                   (_ViewChangeProofRequest'serverHttp x__)
                   (Control.DeepSeq.deepseq
                      (_ViewChangeProofRequest'installedView x__) ())))
{- | Fields :
     
         * 'Proto.Cluster_Fields.serverId' @:: Lens' ViewChangeRequest Proto.Shared.UUID@
         * 'Proto.Cluster_Fields.maybe'serverId' @:: Lens' ViewChangeRequest (Prelude.Maybe Proto.Shared.UUID)@
         * 'Proto.Cluster_Fields.serverHttp' @:: Lens' ViewChangeRequest EndPoint@
         * 'Proto.Cluster_Fields.maybe'serverHttp' @:: Lens' ViewChangeRequest (Prelude.Maybe EndPoint)@
         * 'Proto.Cluster_Fields.attemptedView' @:: Lens' ViewChangeRequest Data.Int.Int32@ -}
data ViewChangeRequest
  = ViewChangeRequest'_constructor {_ViewChangeRequest'serverId :: !(Prelude.Maybe Proto.Shared.UUID),
                                    _ViewChangeRequest'serverHttp :: !(Prelude.Maybe EndPoint),
                                    _ViewChangeRequest'attemptedView :: !Data.Int.Int32,
                                    _ViewChangeRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ViewChangeRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ViewChangeRequest "serverId" Proto.Shared.UUID where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ViewChangeRequest'serverId
           (\ x__ y__ -> x__ {_ViewChangeRequest'serverId = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ViewChangeRequest "maybe'serverId" (Prelude.Maybe Proto.Shared.UUID) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ViewChangeRequest'serverId
           (\ x__ y__ -> x__ {_ViewChangeRequest'serverId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ViewChangeRequest "serverHttp" EndPoint where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ViewChangeRequest'serverHttp
           (\ x__ y__ -> x__ {_ViewChangeRequest'serverHttp = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ViewChangeRequest "maybe'serverHttp" (Prelude.Maybe EndPoint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ViewChangeRequest'serverHttp
           (\ x__ y__ -> x__ {_ViewChangeRequest'serverHttp = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ViewChangeRequest "attemptedView" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ViewChangeRequest'attemptedView
           (\ x__ y__ -> x__ {_ViewChangeRequest'attemptedView = y__}))
        Prelude.id
instance Data.ProtoLens.Message ViewChangeRequest where
  messageName _
    = Data.Text.pack "event_store.cluster.ViewChangeRequest"
  packedMessageDescriptor _
    = "\n\
      \\DC1ViewChangeRequest\DC2<\n\
      \\tserver_id\CAN\SOH \SOH(\v2\US.event_store.client.shared.UUIDR\bserverId\DC2>\n\
      \\vserver_http\CAN\STX \SOH(\v2\GS.event_store.cluster.EndPointR\n\
      \serverHttp\DC2%\n\
      \\SOattempted_view\CAN\ETX \SOH(\ENQR\rattemptedView"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        serverId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "server_id"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.UUID)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'serverId")) ::
              Data.ProtoLens.FieldDescriptor ViewChangeRequest
        serverHttp__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "server_http"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor EndPoint)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'serverHttp")) ::
              Data.ProtoLens.FieldDescriptor ViewChangeRequest
        attemptedView__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "attempted_view"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"attemptedView")) ::
              Data.ProtoLens.FieldDescriptor ViewChangeRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, serverId__field_descriptor),
           (Data.ProtoLens.Tag 2, serverHttp__field_descriptor),
           (Data.ProtoLens.Tag 3, attemptedView__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ViewChangeRequest'_unknownFields
        (\ x__ y__ -> x__ {_ViewChangeRequest'_unknownFields = y__})
  defMessage
    = ViewChangeRequest'_constructor
        {_ViewChangeRequest'serverId = Prelude.Nothing,
         _ViewChangeRequest'serverHttp = Prelude.Nothing,
         _ViewChangeRequest'attemptedView = Data.ProtoLens.fieldDefault,
         _ViewChangeRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ViewChangeRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser ViewChangeRequest
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "server_id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"serverId") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "server_http"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"serverHttp") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "attempted_view"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"attemptedView") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ViewChangeRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'serverId") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage
                          _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'serverHttp") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             _v))
                ((Data.Monoid.<>)
                   (let
                      _v
                        = Lens.Family2.view
                            (Data.ProtoLens.Field.field @"attemptedView") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            ((Prelude..)
                               Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData ViewChangeRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ViewChangeRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ViewChangeRequest'serverId x__)
                (Control.DeepSeq.deepseq
                   (_ViewChangeRequest'serverHttp x__)
                   (Control.DeepSeq.deepseq
                      (_ViewChangeRequest'attemptedView x__) ())))
data Gossip = Gossip {}
instance Data.ProtoLens.Service.Types.Service Gossip where
  type ServiceName Gossip = "Gossip"
  type ServicePackage Gossip = "event_store.cluster"
  type ServiceMethods Gossip = '["read", "update"]
instance Data.ProtoLens.Service.Types.HasMethodImpl Gossip "update" where
  type MethodName Gossip "update" = "Update"
  type MethodInput Gossip "update" = GossipRequest
  type MethodOutput Gossip "update" = ClusterInfo
  type MethodStreamingType Gossip "update" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Gossip "read" where
  type MethodName Gossip "read" = "Read"
  type MethodInput Gossip "read" = Proto.Shared.Empty
  type MethodOutput Gossip "read" = ClusterInfo
  type MethodStreamingType Gossip "read" = 'Data.ProtoLens.Service.Types.NonStreaming
data Elections = Elections {}
instance Data.ProtoLens.Service.Types.Service Elections where
  type ServiceName Elections = "Elections"
  type ServicePackage Elections = "event_store.cluster"
  type ServiceMethods Elections = '["accept",
                                    "leaderIsResigning",
                                    "leaderIsResigningOk",
                                    "prepare",
                                    "prepareOk",
                                    "proposal",
                                    "viewChange",
                                    "viewChangeProof"]
instance Data.ProtoLens.Service.Types.HasMethodImpl Elections "viewChange" where
  type MethodName Elections "viewChange" = "ViewChange"
  type MethodInput Elections "viewChange" = ViewChangeRequest
  type MethodOutput Elections "viewChange" = Proto.Shared.Empty
  type MethodStreamingType Elections "viewChange" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Elections "viewChangeProof" where
  type MethodName Elections "viewChangeProof" = "ViewChangeProof"
  type MethodInput Elections "viewChangeProof" = ViewChangeProofRequest
  type MethodOutput Elections "viewChangeProof" = Proto.Shared.Empty
  type MethodStreamingType Elections "viewChangeProof" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Elections "prepare" where
  type MethodName Elections "prepare" = "Prepare"
  type MethodInput Elections "prepare" = PrepareRequest
  type MethodOutput Elections "prepare" = Proto.Shared.Empty
  type MethodStreamingType Elections "prepare" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Elections "prepareOk" where
  type MethodName Elections "prepareOk" = "PrepareOk"
  type MethodInput Elections "prepareOk" = PrepareOkRequest
  type MethodOutput Elections "prepareOk" = Proto.Shared.Empty
  type MethodStreamingType Elections "prepareOk" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Elections "proposal" where
  type MethodName Elections "proposal" = "Proposal"
  type MethodInput Elections "proposal" = ProposalRequest
  type MethodOutput Elections "proposal" = Proto.Shared.Empty
  type MethodStreamingType Elections "proposal" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Elections "accept" where
  type MethodName Elections "accept" = "Accept"
  type MethodInput Elections "accept" = AcceptRequest
  type MethodOutput Elections "accept" = Proto.Shared.Empty
  type MethodStreamingType Elections "accept" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Elections "leaderIsResigning" where
  type MethodName Elections "leaderIsResigning" = "LeaderIsResigning"
  type MethodInput Elections "leaderIsResigning" = LeaderIsResigningRequest
  type MethodOutput Elections "leaderIsResigning" = Proto.Shared.Empty
  type MethodStreamingType Elections "leaderIsResigning" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Elections "leaderIsResigningOk" where
  type MethodName Elections "leaderIsResigningOk" = "LeaderIsResigningOk"
  type MethodInput Elections "leaderIsResigningOk" = LeaderIsResigningOkRequest
  type MethodOutput Elections "leaderIsResigningOk" = Proto.Shared.Empty
  type MethodStreamingType Elections "leaderIsResigningOk" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\rcluster.proto\DC2\DC3event_store.cluster\SUB\fshared.proto\"|\n\
    \\rGossipRequest\DC24\n\
    \\EOTinfo\CAN\SOH \SOH(\v2 .event_store.cluster.ClusterInfoR\EOTinfo\DC25\n\
    \\ACKserver\CAN\STX \SOH(\v2\GS.event_store.cluster.EndPointR\ACKserver\"\184\SOH\n\
    \\DC1ViewChangeRequest\DC2<\n\
    \\tserver_id\CAN\SOH \SOH(\v2\US.event_store.client.shared.UUIDR\bserverId\DC2>\n\
    \\vserver_http\CAN\STX \SOH(\v2\GS.event_store.cluster.EndPointR\n\
    \serverHttp\DC2%\n\
    \\SOattempted_view\CAN\ETX \SOH(\ENQR\rattemptedView\"\189\SOH\n\
    \\SYNViewChangeProofRequest\DC2<\n\
    \\tserver_id\CAN\SOH \SOH(\v2\US.event_store.client.shared.UUIDR\bserverId\DC2>\n\
    \\vserver_http\CAN\STX \SOH(\v2\GS.event_store.cluster.EndPointR\n\
    \serverHttp\DC2%\n\
    \\SOinstalled_view\CAN\ETX \SOH(\ENQR\rinstalledView\"\162\SOH\n\
    \\SOPrepareRequest\DC2<\n\
    \\tserver_id\CAN\SOH \SOH(\v2\US.event_store.client.shared.UUIDR\bserverId\DC2>\n\
    \\vserver_http\CAN\STX \SOH(\v2\GS.event_store.cluster.EndPointR\n\
    \serverHttp\DC2\DC2\n\
    \\EOTview\CAN\ETX \SOH(\ENQR\EOTview\"\250\EOT\n\
    \\DLEPrepareOkRequest\DC2\DC2\n\
    \\EOTview\CAN\SOH \SOH(\ENQR\EOTview\DC2<\n\
    \\tserver_id\CAN\STX \SOH(\v2\US.event_store.client.shared.UUIDR\bserverId\DC2>\n\
    \\vserver_http\CAN\ETX \SOH(\v2\GS.event_store.cluster.EndPointR\n\
    \serverHttp\DC2!\n\
    \\fepoch_number\CAN\EOT \SOH(\ENQR\vepochNumber\DC2%\n\
    \\SOepoch_position\CAN\ENQ \SOH(\ETXR\repochPosition\DC2:\n\
    \\bepoch_id\CAN\ACK \SOH(\v2\US.event_store.client.shared.UUIDR\aepochId\DC2X\n\
    \\CANepoch_leader_instance_id\CAN\a \SOH(\v2\US.event_store.client.shared.UUIDR\NAKepochLeaderInstanceId\DC20\n\
    \\DC4last_commit_position\CAN\b \SOH(\ETXR\DC2lastCommitPosition\DC2+\n\
    \\DC1writer_checkpoint\CAN\t \SOH(\ETXR\DLEwriterCheckpoint\DC2+\n\
    \\DC1chaser_checkpoint\CAN\n\
    \ \SOH(\ETXR\DLEchaserCheckpoint\DC2#\n\
    \\rnode_priority\CAN\v \SOH(\ENQR\fnodePriority\DC2C\n\
    \\fcluster_info\CAN\f \SOH(\v2 .event_store.cluster.ClusterInfoR\vclusterInfo\"\178\ENQ\n\
    \\SIProposalRequest\DC2<\n\
    \\tserver_id\CAN\SOH \SOH(\v2\US.event_store.client.shared.UUIDR\bserverId\DC2>\n\
    \\vserver_http\CAN\STX \SOH(\v2\GS.event_store.cluster.EndPointR\n\
    \serverHttp\DC2<\n\
    \\tleader_id\CAN\ETX \SOH(\v2\US.event_store.client.shared.UUIDR\bleaderId\DC2>\n\
    \\vleader_http\CAN\EOT \SOH(\v2\GS.event_store.cluster.EndPointR\n\
    \leaderHttp\DC2\DC2\n\
    \\EOTview\CAN\ENQ \SOH(\ENQR\EOTview\DC2!\n\
    \\fepoch_number\CAN\ACK \SOH(\ENQR\vepochNumber\DC2%\n\
    \\SOepoch_position\CAN\a \SOH(\ETXR\repochPosition\DC2:\n\
    \\bepoch_id\CAN\b \SOH(\v2\US.event_store.client.shared.UUIDR\aepochId\DC2X\n\
    \\CANepoch_leader_instance_id\CAN\t \SOH(\v2\US.event_store.client.shared.UUIDR\NAKepochLeaderInstanceId\DC20\n\
    \\DC4last_commit_position\CAN\n\
    \ \SOH(\ETXR\DC2lastCommitPosition\DC2+\n\
    \\DC1writer_checkpoint\CAN\v \SOH(\ETXR\DLEwriterCheckpoint\DC2+\n\
    \\DC1chaser_checkpoint\CAN\f \SOH(\ETXR\DLEchaserCheckpoint\DC2#\n\
    \\rnode_priority\CAN\r \SOH(\ENQR\fnodePriority\"\159\STX\n\
    \\rAcceptRequest\DC2<\n\
    \\tserver_id\CAN\SOH \SOH(\v2\US.event_store.client.shared.UUIDR\bserverId\DC2>\n\
    \\vserver_http\CAN\STX \SOH(\v2\GS.event_store.cluster.EndPointR\n\
    \serverHttp\DC2<\n\
    \\tleader_id\CAN\ETX \SOH(\v2\US.event_store.client.shared.UUIDR\bleaderId\DC2>\n\
    \\vleader_http\CAN\EOT \SOH(\v2\GS.event_store.cluster.EndPointR\n\
    \leaderHttp\DC2\DC2\n\
    \\EOTview\CAN\ENQ \SOH(\ENQR\EOTview\"\152\SOH\n\
    \\CANLeaderIsResigningRequest\DC2<\n\
    \\tleader_id\CAN\SOH \SOH(\v2\US.event_store.client.shared.UUIDR\bleaderId\DC2>\n\
    \\vleader_http\CAN\STX \SOH(\v2\GS.event_store.cluster.EndPointR\n\
    \leaderHttp\"\152\STX\n\
    \\SUBLeaderIsResigningOkRequest\DC2<\n\
    \\tleader_id\CAN\SOH \SOH(\v2\US.event_store.client.shared.UUIDR\bleaderId\DC2>\n\
    \\vleader_http\CAN\STX \SOH(\v2\GS.event_store.cluster.EndPointR\n\
    \leaderHttp\DC2<\n\
    \\tserver_id\CAN\ETX \SOH(\v2\US.event_store.client.shared.UUIDR\bserverId\DC2>\n\
    \\vserver_http\CAN\EOT \SOH(\v2\GS.event_store.cluster.EndPointR\n\
    \serverHttp\"H\n\
    \\vClusterInfo\DC29\n\
    \\amembers\CAN\SOH \ETX(\v2\US.event_store.cluster.MemberInfoR\amembers\"8\n\
    \\bEndPoint\DC2\CAN\n\
    \\aaddress\CAN\SOH \SOH(\tR\aaddress\DC2\DC2\n\
    \\EOTport\CAN\STX \SOH(\rR\EOTport\"\254\b\n\
    \\n\
    \MemberInfo\DC2@\n\
    \\vinstance_id\CAN\SOH \SOH(\v2\US.event_store.client.shared.UUIDR\n\
    \instanceId\DC2\GS\n\
    \\n\
    \time_stamp\CAN\STX \SOH(\ETXR\ttimeStamp\DC2@\n\
    \\ENQstate\CAN\ETX \SOH(\SO2*.event_store.cluster.MemberInfo.VNodeStateR\ENQstate\DC2\EM\n\
    \\bis_alive\CAN\EOT \SOH(\bR\aisAlive\DC2C\n\
    \\SOhttp_end_point\CAN\ENQ \SOH(\v2\GS.event_store.cluster.EndPointR\fhttpEndPoint\DC2@\n\
    \\finternal_tcp\CAN\ACK \SOH(\v2\GS.event_store.cluster.EndPointR\vinternalTcp\DC2@\n\
    \\fexternal_tcp\CAN\a \SOH(\v2\GS.event_store.cluster.EndPointR\vexternalTcp\DC21\n\
    \\NAKinternal_tcp_uses_tls\CAN\b \SOH(\bR\DC2internalTcpUsesTls\DC21\n\
    \\NAKexternal_tcp_uses_tls\CAN\t \SOH(\bR\DC2externalTcpUsesTls\DC20\n\
    \\DC4last_commit_position\CAN\n\
    \ \SOH(\ETXR\DC2lastCommitPosition\DC2+\n\
    \\DC1writer_checkpoint\CAN\v \SOH(\ETXR\DLEwriterCheckpoint\DC2+\n\
    \\DC1chaser_checkpoint\CAN\f \SOH(\ETXR\DLEchaserCheckpoint\DC2%\n\
    \\SOepoch_position\CAN\r \SOH(\ETXR\repochPosition\DC2!\n\
    \\fepoch_number\CAN\SO \SOH(\ENQR\vepochNumber\DC2:\n\
    \\bepoch_id\CAN\SI \SOH(\v2\US.event_store.client.shared.UUIDR\aepochId\DC2#\n\
    \\rnode_priority\CAN\DLE \SOH(\ENQR\fnodePriority\DC2/\n\
    \\DC4is_read_only_replica\CAN\DC1 \SOH(\bR\DC1isReadOnlyReplica\"\154\STX\n\
    \\n\
    \VNodeState\DC2\DLE\n\
    \\fInitializing\DLE\NUL\DC2\DC2\n\
    \\SODiscoverLeader\DLE\SOH\DC2\v\n\
    \\aUnknown\DLE\STX\DC2\SO\n\
    \\n\
    \PreReplica\DLE\ETX\DC2\SO\n\
    \\n\
    \CatchingUp\DLE\EOT\DC2\t\n\
    \\ENQClone\DLE\ENQ\DC2\f\n\
    \\bFollower\DLE\ACK\DC2\r\n\
    \\tPreLeader\DLE\a\DC2\n\
    \\n\
    \\ACKLeader\DLE\b\DC2\v\n\
    \\aManager\DLE\t\DC2\DLE\n\
    \\fShuttingDown\DLE\n\
    \\DC2\f\n\
    \\bShutdown\DLE\v\DC2\SYN\n\
    \\DC2ReadOnlyLeaderless\DLE\f\DC2\SYN\n\
    \\DC2PreReadOnlyReplica\DLE\r\DC2\DC3\n\
    \\SIReadOnlyReplica\DLE\SO\DC2\DC3\n\
    \\SIResigningLeader\DLE\SI2\164\SOH\n\
    \\ACKGossip\DC2N\n\
    \\ACKUpdate\DC2\".event_store.cluster.GossipRequest\SUB .event_store.cluster.ClusterInfo\DC2J\n\
    \\EOTRead\DC2 .event_store.client.shared.Empty\SUB .event_store.cluster.ClusterInfo2\225\ENQ\n\
    \\tElections\DC2V\n\
    \\n\
    \ViewChange\DC2&.event_store.cluster.ViewChangeRequest\SUB .event_store.client.shared.Empty\DC2`\n\
    \\SIViewChangeProof\DC2+.event_store.cluster.ViewChangeProofRequest\SUB .event_store.client.shared.Empty\DC2P\n\
    \\aPrepare\DC2#.event_store.cluster.PrepareRequest\SUB .event_store.client.shared.Empty\DC2T\n\
    \\tPrepareOk\DC2%.event_store.cluster.PrepareOkRequest\SUB .event_store.client.shared.Empty\DC2R\n\
    \\bProposal\DC2$.event_store.cluster.ProposalRequest\SUB .event_store.client.shared.Empty\DC2N\n\
    \\ACKAccept\DC2\".event_store.cluster.AcceptRequest\SUB .event_store.client.shared.Empty\DC2d\n\
    \\DC1LeaderIsResigning\DC2-.event_store.cluster.LeaderIsResigningRequest\SUB .event_store.client.shared.Empty\DC2h\n\
    \\DC3LeaderIsResigningOk\DC2/.event_store.cluster.LeaderIsResigningOkRequest\SUB .event_store.client.shared.EmptyB'\n\
    \%com.eventstore.dbclient.proto.clusterJ\235*\n\
    \\a\DC2\ENQ\NUL\NUL\144\SOH\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\SOH\NUL\FS\n\
    \\b\n\
    \\SOH\b\DC2\ETX\STX\NUL>\n\
    \\t\n\
    \\STX\b\SOH\DC2\ETX\STX\NUL>\n\
    \\t\n\
    \\STX\ETX\NUL\DC2\ETX\EOT\NUL\SYN\n\
    \\n\
    \\n\
    \\STX\ACK\NUL\DC2\EOT\ACK\NUL\t\SOH\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX\ACK\b\SO\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETX\a\b9\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX\a\f\DC2\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX\a\DC4!\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX\a,7\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\SOH\DC2\ETX\b\bI\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\SOH\DC2\ETX\b\f\DLE\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\STX\DC2\ETX\b\DC21\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\ETX\DC2\ETX\b<G\n\
    \\n\
    \\n\
    \\STX\ACK\SOH\DC2\EOT\v\NUL\NAK\SOH\n\
    \\n\
    \\n\
    \\ETX\ACK\SOH\SOH\DC2\ETX\v\b\DC1\n\
    \\v\n\
    \\EOT\ACK\SOH\STX\NUL\DC2\ETX\f\bU\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\NUL\SOH\DC2\ETX\f\f\SYN\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\NUL\STX\DC2\ETX\f\CAN)\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\NUL\ETX\DC2\ETX\f4S\n\
    \\v\n\
    \\EOT\ACK\SOH\STX\SOH\DC2\ETX\r\b_\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\SOH\SOH\DC2\ETX\r\f\ESC\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\SOH\STX\DC2\ETX\r\GS3\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\SOH\ETX\DC2\ETX\r>]\n\
    \\v\n\
    \\EOT\ACK\SOH\STX\STX\DC2\ETX\SO\bO\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\STX\SOH\DC2\ETX\SO\f\DC3\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\STX\STX\DC2\ETX\SO\NAK#\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\STX\ETX\DC2\ETX\SO.M\n\
    \\v\n\
    \\EOT\ACK\SOH\STX\ETX\DC2\ETX\SI\bS\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\ETX\SOH\DC2\ETX\SI\f\NAK\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\ETX\STX\DC2\ETX\SI\ETB'\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\ETX\ETX\DC2\ETX\SI2Q\n\
    \\v\n\
    \\EOT\ACK\SOH\STX\EOT\DC2\ETX\DLE\bQ\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\EOT\SOH\DC2\ETX\DLE\f\DC4\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\EOT\STX\DC2\ETX\DLE\SYN%\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\EOT\ETX\DC2\ETX\DLE0O\n\
    \\v\n\
    \\EOT\ACK\SOH\STX\ENQ\DC2\ETX\DC1\bM\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\ENQ\SOH\DC2\ETX\DC1\f\DC2\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\ENQ\STX\DC2\ETX\DC1\DC4!\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\ENQ\ETX\DC2\ETX\DC1,K\n\
    \\v\n\
    \\EOT\ACK\SOH\STX\ACK\DC2\ETX\DC3\bc\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\ACK\SOH\DC2\ETX\DC3\f\GS\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\ACK\STX\DC2\ETX\DC3\US7\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\ACK\ETX\DC2\ETX\DC3Ba\n\
    \\v\n\
    \\EOT\ACK\SOH\STX\a\DC2\ETX\DC4\bg\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\a\SOH\DC2\ETX\DC4\f\US\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\a\STX\DC2\ETX\DC4!;\n\
    \\f\n\
    \\ENQ\ACK\SOH\STX\a\ETX\DC2\ETX\DC4Fe\n\
    \\n\
    \\n\
    \\STX\EOT\NUL\DC2\EOT\ETB\NUL\SUB\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\ETB\b\NAK\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\CAN\b\GS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ACK\DC2\ETX\CAN\b\DC3\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\CAN\DC4\CAN\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\CAN\ESC\FS\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\SOH\DC2\ETX\EM\b\FS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ACK\DC2\ETX\EM\b\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\SOH\DC2\ETX\EM\DC1\ETB\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ETX\DC2\ETX\EM\SUB\ESC\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT\FS\NUL \SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\FS\b\EM\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\GS\b5\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ACK\DC2\ETX\GS\b&\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\GS'0\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\GS34\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\SOH\DC2\ETX\RS\b!\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ACK\DC2\ETX\RS\b\DLE\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\SOH\DC2\ETX\RS\DC1\FS\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ETX\DC2\ETX\RS\US \n\
    \\v\n\
    \\EOT\EOT\SOH\STX\STX\DC2\ETX\US\b!\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\ENQ\DC2\ETX\US\b\r\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\SOH\DC2\ETX\US\SO\FS\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\ETX\DC2\ETX\US\US \n\
    \\n\
    \\n\
    \\STX\EOT\STX\DC2\EOT\"\NUL&\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX\"\b\RS\n\
    \\v\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX#\b5\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ACK\DC2\ETX#\b&\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX#'0\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX#34\n\
    \\v\n\
    \\EOT\EOT\STX\STX\SOH\DC2\ETX$\b!\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ACK\DC2\ETX$\b\DLE\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\SOH\DC2\ETX$\DC1\FS\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ETX\DC2\ETX$\US \n\
    \\v\n\
    \\EOT\EOT\STX\STX\STX\DC2\ETX%\b!\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\ENQ\DC2\ETX%\b\r\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\SOH\DC2\ETX%\SO\FS\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\ETX\DC2\ETX%\US \n\
    \\n\
    \\n\
    \\STX\EOT\ETX\DC2\EOT(\NUL,\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETX(\b\SYN\n\
    \\v\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\ETX)\b5\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ACK\DC2\ETX)\b&\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\ETX)'0\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\ETX)34\n\
    \\v\n\
    \\EOT\EOT\ETX\STX\SOH\DC2\ETX*\b!\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\ACK\DC2\ETX*\b\DLE\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\SOH\DC2\ETX*\DC1\FS\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\ETX\DC2\ETX*\US \n\
    \\v\n\
    \\EOT\EOT\ETX\STX\STX\DC2\ETX+\b\ETB\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\ENQ\DC2\ETX+\b\r\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\SOH\DC2\ETX+\SO\DC2\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\ETX\DC2\ETX+\NAK\SYN\n\
    \\n\
    \\n\
    \\STX\EOT\EOT\DC2\EOT.\NUL;\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\EOT\SOH\DC2\ETX.\b\CAN\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\NUL\DC2\ETX/\b\ETB\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ENQ\DC2\ETX/\b\r\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\SOH\DC2\ETX/\SO\DC2\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ETX\DC2\ETX/\NAK\SYN\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\SOH\DC2\ETX0\b5\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ACK\DC2\ETX0\b&\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\SOH\DC2\ETX0'0\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ETX\DC2\ETX034\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\STX\DC2\ETX1\b!\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\ACK\DC2\ETX1\b\DLE\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\SOH\DC2\ETX1\DC1\FS\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\ETX\DC2\ETX1\US \n\
    \\v\n\
    \\EOT\EOT\EOT\STX\ETX\DC2\ETX2\b\US\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ETX\ENQ\DC2\ETX2\b\r\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ETX\SOH\DC2\ETX2\SO\SUB\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ETX\ETX\DC2\ETX2\GS\RS\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\EOT\DC2\ETX3\b!\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\EOT\ENQ\DC2\ETX3\b\r\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\EOT\SOH\DC2\ETX3\SO\FS\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\EOT\ETX\DC2\ETX3\US \n\
    \\v\n\
    \\EOT\EOT\EOT\STX\ENQ\DC2\ETX4\b4\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ENQ\ACK\DC2\ETX4\b&\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ENQ\SOH\DC2\ETX4'/\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ENQ\ETX\DC2\ETX423\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\ACK\DC2\ETX5\bD\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ACK\ACK\DC2\ETX5\b&\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ACK\SOH\DC2\ETX5'?\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ACK\ETX\DC2\ETX5BC\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\a\DC2\ETX6\b'\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\a\ENQ\DC2\ETX6\b\r\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\a\SOH\DC2\ETX6\SO\"\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\a\ETX\DC2\ETX6%&\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\b\DC2\ETX7\b$\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\b\ENQ\DC2\ETX7\b\r\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\b\SOH\DC2\ETX7\SO\US\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\b\ETX\DC2\ETX7\"#\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\t\DC2\ETX8\b%\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\t\ENQ\DC2\ETX8\b\r\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\t\SOH\DC2\ETX8\SO\US\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\t\ETX\DC2\ETX8\"$\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\n\
    \\DC2\ETX9\b!\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\n\
    \\ENQ\DC2\ETX9\b\r\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\n\
    \\SOH\DC2\ETX9\SO\ESC\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\n\
    \\ETX\DC2\ETX9\RS \n\
    \\v\n\
    \\EOT\EOT\EOT\STX\v\DC2\ETX:\b&\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\v\ACK\DC2\ETX:\b\DC3\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\v\SOH\DC2\ETX:\DC4 \n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\v\ETX\DC2\ETX:#%\n\
    \\n\
    \\n\
    \\STX\EOT\ENQ\DC2\EOT=\NULK\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ENQ\SOH\DC2\ETX=\b\ETB\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\NUL\DC2\ETX>\b5\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ACK\DC2\ETX>\b&\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\SOH\DC2\ETX>'0\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ETX\DC2\ETX>34\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\SOH\DC2\ETX?\b!\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\ACK\DC2\ETX?\b\DLE\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\SOH\DC2\ETX?\DC1\FS\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\ETX\DC2\ETX?\US \n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\STX\DC2\ETX@\b5\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\STX\ACK\DC2\ETX@\b&\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\STX\SOH\DC2\ETX@'0\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\STX\ETX\DC2\ETX@34\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\ETX\DC2\ETXA\b!\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ETX\ACK\DC2\ETXA\b\DLE\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ETX\SOH\DC2\ETXA\DC1\FS\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ETX\ETX\DC2\ETXA\US \n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\EOT\DC2\ETXB\b\ETB\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\EOT\ENQ\DC2\ETXB\b\r\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\EOT\SOH\DC2\ETXB\SO\DC2\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\EOT\ETX\DC2\ETXB\NAK\SYN\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\ENQ\DC2\ETXC\b\US\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ENQ\ENQ\DC2\ETXC\b\r\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ENQ\SOH\DC2\ETXC\SO\SUB\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ENQ\ETX\DC2\ETXC\GS\RS\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\ACK\DC2\ETXD\b!\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ACK\ENQ\DC2\ETXD\b\r\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ACK\SOH\DC2\ETXD\SO\FS\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ACK\ETX\DC2\ETXD\US \n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\a\DC2\ETXE\b4\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\a\ACK\DC2\ETXE\b&\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\a\SOH\DC2\ETXE'/\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\a\ETX\DC2\ETXE23\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\b\DC2\ETXF\bD\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\b\ACK\DC2\ETXF\b&\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\b\SOH\DC2\ETXF'?\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\b\ETX\DC2\ETXFBC\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\t\DC2\ETXG\b(\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\t\ENQ\DC2\ETXG\b\r\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\t\SOH\DC2\ETXG\SO\"\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\t\ETX\DC2\ETXG%'\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\n\
    \\DC2\ETXH\b%\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\n\
    \\ENQ\DC2\ETXH\b\r\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\n\
    \\SOH\DC2\ETXH\SO\US\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\n\
    \\ETX\DC2\ETXH\"$\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\v\DC2\ETXI\b%\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\v\ENQ\DC2\ETXI\b\r\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\v\SOH\DC2\ETXI\SO\US\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\v\ETX\DC2\ETXI\"$\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\f\DC2\ETXJ\b!\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\f\ENQ\DC2\ETXJ\b\r\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\f\SOH\DC2\ETXJ\SO\ESC\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\f\ETX\DC2\ETXJ\RS \n\
    \\n\
    \\n\
    \\STX\EOT\ACK\DC2\EOTM\NULS\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ACK\SOH\DC2\ETXM\b\NAK\n\
    \\v\n\
    \\EOT\EOT\ACK\STX\NUL\DC2\ETXN\b5\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ACK\DC2\ETXN\b&\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\SOH\DC2\ETXN'0\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ETX\DC2\ETXN34\n\
    \\v\n\
    \\EOT\EOT\ACK\STX\SOH\DC2\ETXO\b!\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\ACK\DC2\ETXO\b\DLE\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\SOH\DC2\ETXO\DC1\FS\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\ETX\DC2\ETXO\US \n\
    \\v\n\
    \\EOT\EOT\ACK\STX\STX\DC2\ETXP\b5\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\STX\ACK\DC2\ETXP\b&\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\STX\SOH\DC2\ETXP'0\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\STX\ETX\DC2\ETXP34\n\
    \\v\n\
    \\EOT\EOT\ACK\STX\ETX\DC2\ETXQ\b!\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\ETX\ACK\DC2\ETXQ\b\DLE\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\ETX\SOH\DC2\ETXQ\DC1\FS\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\ETX\ETX\DC2\ETXQ\US \n\
    \\v\n\
    \\EOT\EOT\ACK\STX\EOT\DC2\ETXR\b\ETB\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\EOT\ENQ\DC2\ETXR\b\r\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\EOT\SOH\DC2\ETXR\SO\DC2\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\EOT\ETX\DC2\ETXR\NAK\SYN\n\
    \\n\
    \\n\
    \\STX\EOT\a\DC2\EOTU\NULX\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\a\SOH\DC2\ETXU\b \n\
    \\v\n\
    \\EOT\EOT\a\STX\NUL\DC2\ETXV\b5\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\ACK\DC2\ETXV\b&\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\SOH\DC2\ETXV'0\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\ETX\DC2\ETXV34\n\
    \\v\n\
    \\EOT\EOT\a\STX\SOH\DC2\ETXW\b!\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\ACK\DC2\ETXW\b\DLE\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\SOH\DC2\ETXW\DC1\FS\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\ETX\DC2\ETXW\US \n\
    \\n\
    \\n\
    \\STX\EOT\b\DC2\EOTZ\NUL_\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\b\SOH\DC2\ETXZ\b\"\n\
    \\v\n\
    \\EOT\EOT\b\STX\NUL\DC2\ETX[\b5\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\ACK\DC2\ETX[\b&\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\SOH\DC2\ETX['0\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\ETX\DC2\ETX[34\n\
    \\v\n\
    \\EOT\EOT\b\STX\SOH\DC2\ETX\\\b!\n\
    \\f\n\
    \\ENQ\EOT\b\STX\SOH\ACK\DC2\ETX\\\b\DLE\n\
    \\f\n\
    \\ENQ\EOT\b\STX\SOH\SOH\DC2\ETX\\\DC1\FS\n\
    \\f\n\
    \\ENQ\EOT\b\STX\SOH\ETX\DC2\ETX\\\US \n\
    \\v\n\
    \\EOT\EOT\b\STX\STX\DC2\ETX]\b5\n\
    \\f\n\
    \\ENQ\EOT\b\STX\STX\ACK\DC2\ETX]\b&\n\
    \\f\n\
    \\ENQ\EOT\b\STX\STX\SOH\DC2\ETX]'0\n\
    \\f\n\
    \\ENQ\EOT\b\STX\STX\ETX\DC2\ETX]34\n\
    \\v\n\
    \\EOT\EOT\b\STX\ETX\DC2\ETX^\b!\n\
    \\f\n\
    \\ENQ\EOT\b\STX\ETX\ACK\DC2\ETX^\b\DLE\n\
    \\f\n\
    \\ENQ\EOT\b\STX\ETX\SOH\DC2\ETX^\DC1\FS\n\
    \\f\n\
    \\ENQ\EOT\b\STX\ETX\ETX\DC2\ETX^\US \n\
    \\n\
    \\n\
    \\STX\EOT\t\DC2\EOTa\NULc\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\t\SOH\DC2\ETXa\b\DC3\n\
    \\v\n\
    \\EOT\EOT\t\STX\NUL\DC2\ETXb\b(\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\EOT\DC2\ETXb\b\DLE\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\ACK\DC2\ETXb\DC1\ESC\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\SOH\DC2\ETXb\FS#\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\ETX\DC2\ETXb&'\n\
    \\n\
    \\n\
    \\STX\EOT\n\
    \\DC2\EOTe\NULh\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\n\
    \\SOH\DC2\ETXe\b\DLE\n\
    \\v\n\
    \\EOT\EOT\n\
    \\STX\NUL\DC2\ETXf\b\ESC\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ENQ\DC2\ETXf\b\SO\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\SOH\DC2\ETXf\SI\SYN\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ETX\DC2\ETXf\EM\SUB\n\
    \\v\n\
    \\EOT\EOT\n\
    \\STX\SOH\DC2\ETXg\b\CAN\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\SOH\ENQ\DC2\ETXg\b\SO\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\SOH\SOH\DC2\ETXg\SI\DC3\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\SOH\ETX\DC2\ETXg\SYN\ETB\n\
    \\v\n\
    \\STX\EOT\v\DC2\ENQj\NUL\144\SOH\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\v\SOH\DC2\ETXj\b\DC2\n\
    \\f\n\
    \\EOT\EOT\v\EOT\NUL\DC2\EOTk\b|\t\n\
    \\f\n\
    \\ENQ\EOT\v\EOT\NUL\SOH\DC2\ETXk\r\ETB\n\
    \\r\n\
    \\ACK\EOT\v\EOT\NUL\STX\NUL\DC2\ETXl\DLE!\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\NUL\SOH\DC2\ETXl\DLE\FS\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\NUL\STX\DC2\ETXl\US \n\
    \\r\n\
    \\ACK\EOT\v\EOT\NUL\STX\SOH\DC2\ETXm\DLE#\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\SOH\SOH\DC2\ETXm\DLE\RS\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\SOH\STX\DC2\ETXm!\"\n\
    \\r\n\
    \\ACK\EOT\v\EOT\NUL\STX\STX\DC2\ETXn\DLE\FS\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\STX\SOH\DC2\ETXn\DLE\ETB\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\STX\STX\DC2\ETXn\SUB\ESC\n\
    \\r\n\
    \\ACK\EOT\v\EOT\NUL\STX\ETX\DC2\ETXo\DLE\US\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\ETX\SOH\DC2\ETXo\DLE\SUB\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\ETX\STX\DC2\ETXo\GS\RS\n\
    \\r\n\
    \\ACK\EOT\v\EOT\NUL\STX\EOT\DC2\ETXp\DLE\US\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\EOT\SOH\DC2\ETXp\DLE\SUB\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\EOT\STX\DC2\ETXp\GS\RS\n\
    \\r\n\
    \\ACK\EOT\v\EOT\NUL\STX\ENQ\DC2\ETXq\DLE\SUB\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\ENQ\SOH\DC2\ETXq\DLE\NAK\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\ENQ\STX\DC2\ETXq\CAN\EM\n\
    \\r\n\
    \\ACK\EOT\v\EOT\NUL\STX\ACK\DC2\ETXr\DLE\GS\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\ACK\SOH\DC2\ETXr\DLE\CAN\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\ACK\STX\DC2\ETXr\ESC\FS\n\
    \\r\n\
    \\ACK\EOT\v\EOT\NUL\STX\a\DC2\ETXs\DLE\RS\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\a\SOH\DC2\ETXs\DLE\EM\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\a\STX\DC2\ETXs\FS\GS\n\
    \\r\n\
    \\ACK\EOT\v\EOT\NUL\STX\b\DC2\ETXt\DLE\ESC\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\b\SOH\DC2\ETXt\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\b\STX\DC2\ETXt\EM\SUB\n\
    \\r\n\
    \\ACK\EOT\v\EOT\NUL\STX\t\DC2\ETXu\DLE\FS\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\t\SOH\DC2\ETXu\DLE\ETB\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\t\STX\DC2\ETXu\SUB\ESC\n\
    \\r\n\
    \\ACK\EOT\v\EOT\NUL\STX\n\
    \\DC2\ETXv\DLE\"\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\n\
    \\SOH\DC2\ETXv\DLE\FS\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\n\
    \\STX\DC2\ETXv\US!\n\
    \\r\n\
    \\ACK\EOT\v\EOT\NUL\STX\v\DC2\ETXw\DLE\RS\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\v\SOH\DC2\ETXw\DLE\CAN\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\v\STX\DC2\ETXw\ESC\GS\n\
    \\r\n\
    \\ACK\EOT\v\EOT\NUL\STX\f\DC2\ETXx\DLE(\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\f\SOH\DC2\ETXx\DLE\"\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\f\STX\DC2\ETXx%'\n\
    \\r\n\
    \\ACK\EOT\v\EOT\NUL\STX\r\DC2\ETXy\DLE(\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\r\SOH\DC2\ETXy\DLE\"\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\r\STX\DC2\ETXy%'\n\
    \\r\n\
    \\ACK\EOT\v\EOT\NUL\STX\SO\DC2\ETXz\DLE%\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\SO\SOH\DC2\ETXz\DLE\US\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\SO\STX\DC2\ETXz\"$\n\
    \\r\n\
    \\ACK\EOT\v\EOT\NUL\STX\SI\DC2\ETX{\DLE%\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\SI\SOH\DC2\ETX{\DLE\US\n\
    \\SO\n\
    \\a\EOT\v\EOT\NUL\STX\SI\STX\DC2\ETX{\"$\n\
    \\v\n\
    \\EOT\EOT\v\STX\NUL\DC2\ETX}\b7\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\ACK\DC2\ETX}\b&\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\SOH\DC2\ETX}'2\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\ETX\DC2\ETX}56\n\
    \\v\n\
    \\EOT\EOT\v\STX\SOH\DC2\ETX~\b\GS\n\
    \\f\n\
    \\ENQ\EOT\v\STX\SOH\ENQ\DC2\ETX~\b\r\n\
    \\f\n\
    \\ENQ\EOT\v\STX\SOH\SOH\DC2\ETX~\SO\CAN\n\
    \\f\n\
    \\ENQ\EOT\v\STX\SOH\ETX\DC2\ETX~\ESC\FS\n\
    \\v\n\
    \\EOT\EOT\v\STX\STX\DC2\ETX\DEL\b\GS\n\
    \\f\n\
    \\ENQ\EOT\v\STX\STX\ACK\DC2\ETX\DEL\b\DC2\n\
    \\f\n\
    \\ENQ\EOT\v\STX\STX\SOH\DC2\ETX\DEL\DC3\CAN\n\
    \\f\n\
    \\ENQ\EOT\v\STX\STX\ETX\DC2\ETX\DEL\ESC\FS\n\
    \\f\n\
    \\EOT\EOT\v\STX\ETX\DC2\EOT\128\SOH\b\SUB\n\
    \\r\n\
    \\ENQ\EOT\v\STX\ETX\ENQ\DC2\EOT\128\SOH\b\f\n\
    \\r\n\
    \\ENQ\EOT\v\STX\ETX\SOH\DC2\EOT\128\SOH\r\NAK\n\
    \\r\n\
    \\ENQ\EOT\v\STX\ETX\ETX\DC2\EOT\128\SOH\CAN\EM\n\
    \\f\n\
    \\EOT\EOT\v\STX\EOT\DC2\EOT\129\SOH\b$\n\
    \\r\n\
    \\ENQ\EOT\v\STX\EOT\ACK\DC2\EOT\129\SOH\b\DLE\n\
    \\r\n\
    \\ENQ\EOT\v\STX\EOT\SOH\DC2\EOT\129\SOH\DC1\US\n\
    \\r\n\
    \\ENQ\EOT\v\STX\EOT\ETX\DC2\EOT\129\SOH\"#\n\
    \\f\n\
    \\EOT\EOT\v\STX\ENQ\DC2\EOT\130\SOH\b\"\n\
    \\r\n\
    \\ENQ\EOT\v\STX\ENQ\ACK\DC2\EOT\130\SOH\b\DLE\n\
    \\r\n\
    \\ENQ\EOT\v\STX\ENQ\SOH\DC2\EOT\130\SOH\DC1\GS\n\
    \\r\n\
    \\ENQ\EOT\v\STX\ENQ\ETX\DC2\EOT\130\SOH !\n\
    \\f\n\
    \\EOT\EOT\v\STX\ACK\DC2\EOT\131\SOH\b\"\n\
    \\r\n\
    \\ENQ\EOT\v\STX\ACK\ACK\DC2\EOT\131\SOH\b\DLE\n\
    \\r\n\
    \\ENQ\EOT\v\STX\ACK\SOH\DC2\EOT\131\SOH\DC1\GS\n\
    \\r\n\
    \\ENQ\EOT\v\STX\ACK\ETX\DC2\EOT\131\SOH !\n\
    \\f\n\
    \\EOT\EOT\v\STX\a\DC2\EOT\132\SOH\b'\n\
    \\r\n\
    \\ENQ\EOT\v\STX\a\ENQ\DC2\EOT\132\SOH\b\f\n\
    \\r\n\
    \\ENQ\EOT\v\STX\a\SOH\DC2\EOT\132\SOH\r\"\n\
    \\r\n\
    \\ENQ\EOT\v\STX\a\ETX\DC2\EOT\132\SOH%&\n\
    \\f\n\
    \\EOT\EOT\v\STX\b\DC2\EOT\133\SOH\b'\n\
    \\r\n\
    \\ENQ\EOT\v\STX\b\ENQ\DC2\EOT\133\SOH\b\f\n\
    \\r\n\
    \\ENQ\EOT\v\STX\b\SOH\DC2\EOT\133\SOH\r\"\n\
    \\r\n\
    \\ENQ\EOT\v\STX\b\ETX\DC2\EOT\133\SOH%&\n\
    \\f\n\
    \\EOT\EOT\v\STX\t\DC2\EOT\135\SOH\EOT$\n\
    \\r\n\
    \\ENQ\EOT\v\STX\t\ENQ\DC2\EOT\135\SOH\EOT\t\n\
    \\r\n\
    \\ENQ\EOT\v\STX\t\SOH\DC2\EOT\135\SOH\n\
    \\RS\n\
    \\r\n\
    \\ENQ\EOT\v\STX\t\ETX\DC2\EOT\135\SOH!#\n\
    \\f\n\
    \\EOT\EOT\v\STX\n\
    \\DC2\EOT\136\SOH\EOT!\n\
    \\r\n\
    \\ENQ\EOT\v\STX\n\
    \\ENQ\DC2\EOT\136\SOH\EOT\t\n\
    \\r\n\
    \\ENQ\EOT\v\STX\n\
    \\SOH\DC2\EOT\136\SOH\n\
    \\ESC\n\
    \\r\n\
    \\ENQ\EOT\v\STX\n\
    \\ETX\DC2\EOT\136\SOH\RS \n\
    \\f\n\
    \\EOT\EOT\v\STX\v\DC2\EOT\137\SOH\b%\n\
    \\r\n\
    \\ENQ\EOT\v\STX\v\ENQ\DC2\EOT\137\SOH\b\r\n\
    \\r\n\
    \\ENQ\EOT\v\STX\v\SOH\DC2\EOT\137\SOH\SO\US\n\
    \\r\n\
    \\ENQ\EOT\v\STX\v\ETX\DC2\EOT\137\SOH\"$\n\
    \\f\n\
    \\EOT\EOT\v\STX\f\DC2\EOT\138\SOH\EOT\RS\n\
    \\r\n\
    \\ENQ\EOT\v\STX\f\ENQ\DC2\EOT\138\SOH\EOT\t\n\
    \\r\n\
    \\ENQ\EOT\v\STX\f\SOH\DC2\EOT\138\SOH\n\
    \\CAN\n\
    \\r\n\
    \\ENQ\EOT\v\STX\f\ETX\DC2\EOT\138\SOH\ESC\GS\n\
    \\f\n\
    \\EOT\EOT\v\STX\r\DC2\EOT\139\SOH\EOT\FS\n\
    \\r\n\
    \\ENQ\EOT\v\STX\r\ENQ\DC2\EOT\139\SOH\EOT\t\n\
    \\r\n\
    \\ENQ\EOT\v\STX\r\SOH\DC2\EOT\139\SOH\n\
    \\SYN\n\
    \\r\n\
    \\ENQ\EOT\v\STX\r\ETX\DC2\EOT\139\SOH\EM\ESC\n\
    \\f\n\
    \\EOT\EOT\v\STX\SO\DC2\EOT\140\SOH\EOT1\n\
    \\r\n\
    \\ENQ\EOT\v\STX\SO\ACK\DC2\EOT\140\SOH\EOT\"\n\
    \\r\n\
    \\ENQ\EOT\v\STX\SO\SOH\DC2\EOT\140\SOH#+\n\
    \\r\n\
    \\ENQ\EOT\v\STX\SO\ETX\DC2\EOT\140\SOH.0\n\
    \\f\n\
    \\EOT\EOT\v\STX\SI\DC2\EOT\142\SOH\EOT\GS\n\
    \\r\n\
    \\ENQ\EOT\v\STX\SI\ENQ\DC2\EOT\142\SOH\EOT\t\n\
    \\r\n\
    \\ENQ\EOT\v\STX\SI\SOH\DC2\EOT\142\SOH\n\
    \\ETB\n\
    \\r\n\
    \\ENQ\EOT\v\STX\SI\ETX\DC2\EOT\142\SOH\SUB\FS\n\
    \\f\n\
    \\EOT\EOT\v\STX\DLE\DC2\EOT\143\SOH\EOT#\n\
    \\r\n\
    \\ENQ\EOT\v\STX\DLE\ENQ\DC2\EOT\143\SOH\EOT\b\n\
    \\r\n\
    \\ENQ\EOT\v\STX\DLE\SOH\DC2\EOT\143\SOH\t\GS\n\
    \\r\n\
    \\ENQ\EOT\v\STX\DLE\ETX\DC2\EOT\143\SOH \"b\ACKproto3"