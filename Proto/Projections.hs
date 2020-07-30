{- This file was auto-generated from projections.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Projections (
        Projections(..), CreateReq(), CreateReq'Options(),
        CreateReq'Options'Mode(..), _CreateReq'Options'OneTime,
        _CreateReq'Options'Transient', _CreateReq'Options'Continuous',
        CreateReq'Options'Continuous(), CreateReq'Options'Transient(),
        CreateResp(), DeleteReq(), DeleteReq'Options(), DeleteResp(),
        DisableReq(), DisableReq'Options(), DisableResp(), EnableReq(),
        EnableReq'Options(), EnableResp(), ResetReq(), ResetReq'Options(),
        ResetResp(), ResultReq(), ResultReq'Options(), ResultResp(),
        StateReq(), StateReq'Options(), StateResp(), StatisticsReq(),
        StatisticsReq'Options(), StatisticsReq'Options'Mode(..),
        _StatisticsReq'Options'Name, _StatisticsReq'Options'All,
        _StatisticsReq'Options'Transient,
        _StatisticsReq'Options'Continuous, _StatisticsReq'Options'OneTime,
        StatisticsResp(), StatisticsResp'Details(), UpdateReq(),
        UpdateReq'Options(), UpdateReq'Options'EmitOption(..),
        _UpdateReq'Options'EmitEnabled, _UpdateReq'Options'NoEmitOptions,
        UpdateResp()
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
import qualified Proto.Google.Protobuf.Struct
import qualified Proto.Shared
{- | Fields :
     
         * 'Proto.Projections_Fields.options' @:: Lens' CreateReq CreateReq'Options@
         * 'Proto.Projections_Fields.maybe'options' @:: Lens' CreateReq (Prelude.Maybe CreateReq'Options)@ -}
data CreateReq
  = CreateReq'_constructor {_CreateReq'options :: !(Prelude.Maybe CreateReq'Options),
                            _CreateReq'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show CreateReq where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField CreateReq "options" CreateReq'Options where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CreateReq'options (\ x__ y__ -> x__ {_CreateReq'options = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField CreateReq "maybe'options" (Prelude.Maybe CreateReq'Options) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CreateReq'options (\ x__ y__ -> x__ {_CreateReq'options = y__}))
        Prelude.id
instance Data.ProtoLens.Message CreateReq where
  messageName _
    = Data.Text.pack "event_store.client.projections.CreateReq"
  packedMessageDescriptor _
    = "\n\
      \\tCreateReq\DC2K\n\
      \\aoptions\CAN\SOH \SOH(\v21.event_store.client.projections.CreateReq.OptionsR\aoptions\SUB\154\ETX\n\
      \\aOptions\DC2=\n\
      \\bone_time\CAN\SOH \SOH(\v2 .event_store.client.shared.EmptyH\NULR\aoneTime\DC2[\n\
      \\ttransient\CAN\STX \SOH(\v2;.event_store.client.projections.CreateReq.Options.TransientH\NULR\ttransient\DC2^\n\
      \\n\
      \continuous\CAN\ETX \SOH(\v2<.event_store.client.projections.CreateReq.Options.ContinuousH\NULR\n\
      \continuous\DC2\DC4\n\
      \\ENQquery\CAN\EOT \SOH(\tR\ENQquery\SUB\US\n\
      \\tTransient\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\SUBT\n\
      \\n\
      \Continuous\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC22\n\
      \\NAKtrack_emitted_streams\CAN\STX \SOH(\bR\DC3trackEmittedStreamsB\ACK\n\
      \\EOTmode"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        options__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "options"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor CreateReq'Options)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'options")) ::
              Data.ProtoLens.FieldDescriptor CreateReq
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, options__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _CreateReq'_unknownFields
        (\ x__ y__ -> x__ {_CreateReq'_unknownFields = y__})
  defMessage
    = CreateReq'_constructor
        {_CreateReq'options = Prelude.Nothing,
         _CreateReq'_unknownFields = []}
  parseMessage
    = let
        loop :: CreateReq -> Data.ProtoLens.Encoding.Bytes.Parser CreateReq
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
                                       "options"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"options") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "CreateReq"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'options") _x
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
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData CreateReq where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_CreateReq'_unknownFields x__)
             (Control.DeepSeq.deepseq (_CreateReq'options x__) ())
{- | Fields :
     
         * 'Proto.Projections_Fields.query' @:: Lens' CreateReq'Options Data.Text.Text@
         * 'Proto.Projections_Fields.maybe'mode' @:: Lens' CreateReq'Options (Prelude.Maybe CreateReq'Options'Mode)@
         * 'Proto.Projections_Fields.maybe'oneTime' @:: Lens' CreateReq'Options (Prelude.Maybe Proto.Shared.Empty)@
         * 'Proto.Projections_Fields.oneTime' @:: Lens' CreateReq'Options Proto.Shared.Empty@
         * 'Proto.Projections_Fields.maybe'transient' @:: Lens' CreateReq'Options (Prelude.Maybe CreateReq'Options'Transient)@
         * 'Proto.Projections_Fields.transient' @:: Lens' CreateReq'Options CreateReq'Options'Transient@
         * 'Proto.Projections_Fields.maybe'continuous' @:: Lens' CreateReq'Options (Prelude.Maybe CreateReq'Options'Continuous)@
         * 'Proto.Projections_Fields.continuous' @:: Lens' CreateReq'Options CreateReq'Options'Continuous@ -}
data CreateReq'Options
  = CreateReq'Options'_constructor {_CreateReq'Options'query :: !Data.Text.Text,
                                    _CreateReq'Options'mode :: !(Prelude.Maybe CreateReq'Options'Mode),
                                    _CreateReq'Options'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show CreateReq'Options where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data CreateReq'Options'Mode
  = CreateReq'Options'OneTime !Proto.Shared.Empty |
    CreateReq'Options'Transient' !CreateReq'Options'Transient |
    CreateReq'Options'Continuous' !CreateReq'Options'Continuous
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField CreateReq'Options "query" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CreateReq'Options'query
           (\ x__ y__ -> x__ {_CreateReq'Options'query = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField CreateReq'Options "maybe'mode" (Prelude.Maybe CreateReq'Options'Mode) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CreateReq'Options'mode
           (\ x__ y__ -> x__ {_CreateReq'Options'mode = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField CreateReq'Options "maybe'oneTime" (Prelude.Maybe Proto.Shared.Empty) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CreateReq'Options'mode
           (\ x__ y__ -> x__ {_CreateReq'Options'mode = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (CreateReq'Options'OneTime x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap CreateReq'Options'OneTime y__))
instance Data.ProtoLens.Field.HasField CreateReq'Options "oneTime" Proto.Shared.Empty where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CreateReq'Options'mode
           (\ x__ y__ -> x__ {_CreateReq'Options'mode = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (CreateReq'Options'OneTime x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap CreateReq'Options'OneTime y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField CreateReq'Options "maybe'transient" (Prelude.Maybe CreateReq'Options'Transient) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CreateReq'Options'mode
           (\ x__ y__ -> x__ {_CreateReq'Options'mode = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (CreateReq'Options'Transient' x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap CreateReq'Options'Transient' y__))
instance Data.ProtoLens.Field.HasField CreateReq'Options "transient" CreateReq'Options'Transient where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CreateReq'Options'mode
           (\ x__ y__ -> x__ {_CreateReq'Options'mode = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (CreateReq'Options'Transient' x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap CreateReq'Options'Transient' y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField CreateReq'Options "maybe'continuous" (Prelude.Maybe CreateReq'Options'Continuous) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CreateReq'Options'mode
           (\ x__ y__ -> x__ {_CreateReq'Options'mode = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (CreateReq'Options'Continuous' x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap CreateReq'Options'Continuous' y__))
instance Data.ProtoLens.Field.HasField CreateReq'Options "continuous" CreateReq'Options'Continuous where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CreateReq'Options'mode
           (\ x__ y__ -> x__ {_CreateReq'Options'mode = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (CreateReq'Options'Continuous' x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap CreateReq'Options'Continuous' y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message CreateReq'Options where
  messageName _
    = Data.Text.pack "event_store.client.projections.CreateReq.Options"
  packedMessageDescriptor _
    = "\n\
      \\aOptions\DC2=\n\
      \\bone_time\CAN\SOH \SOH(\v2 .event_store.client.shared.EmptyH\NULR\aoneTime\DC2[\n\
      \\ttransient\CAN\STX \SOH(\v2;.event_store.client.projections.CreateReq.Options.TransientH\NULR\ttransient\DC2^\n\
      \\n\
      \continuous\CAN\ETX \SOH(\v2<.event_store.client.projections.CreateReq.Options.ContinuousH\NULR\n\
      \continuous\DC2\DC4\n\
      \\ENQquery\CAN\EOT \SOH(\tR\ENQquery\SUB\US\n\
      \\tTransient\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\SUBT\n\
      \\n\
      \Continuous\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC22\n\
      \\NAKtrack_emitted_streams\CAN\STX \SOH(\bR\DC3trackEmittedStreamsB\ACK\n\
      \\EOTmode"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        query__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "query"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"query")) ::
              Data.ProtoLens.FieldDescriptor CreateReq'Options
        oneTime__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "one_time"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.Empty)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'oneTime")) ::
              Data.ProtoLens.FieldDescriptor CreateReq'Options
        transient__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "transient"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor CreateReq'Options'Transient)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'transient")) ::
              Data.ProtoLens.FieldDescriptor CreateReq'Options
        continuous__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "continuous"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor CreateReq'Options'Continuous)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'continuous")) ::
              Data.ProtoLens.FieldDescriptor CreateReq'Options
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 4, query__field_descriptor),
           (Data.ProtoLens.Tag 1, oneTime__field_descriptor),
           (Data.ProtoLens.Tag 2, transient__field_descriptor),
           (Data.ProtoLens.Tag 3, continuous__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _CreateReq'Options'_unknownFields
        (\ x__ y__ -> x__ {_CreateReq'Options'_unknownFields = y__})
  defMessage
    = CreateReq'Options'_constructor
        {_CreateReq'Options'query = Data.ProtoLens.fieldDefault,
         _CreateReq'Options'mode = Prelude.Nothing,
         _CreateReq'Options'_unknownFields = []}
  parseMessage
    = let
        loop ::
          CreateReq'Options
          -> Data.ProtoLens.Encoding.Bytes.Parser CreateReq'Options
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
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "query"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"query") y x)
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "one_time"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"oneTime") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "transient"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"transient") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "continuous"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"continuous") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Options"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"query") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8
                         _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'mode") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just (CreateReq'Options'OneTime v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             v)
                   (Prelude.Just (CreateReq'Options'Transient' v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             v)
                   (Prelude.Just (CreateReq'Options'Continuous' v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData CreateReq'Options where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_CreateReq'Options'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_CreateReq'Options'query x__)
                (Control.DeepSeq.deepseq (_CreateReq'Options'mode x__) ()))
instance Control.DeepSeq.NFData CreateReq'Options'Mode where
  rnf (CreateReq'Options'OneTime x__) = Control.DeepSeq.rnf x__
  rnf (CreateReq'Options'Transient' x__) = Control.DeepSeq.rnf x__
  rnf (CreateReq'Options'Continuous' x__) = Control.DeepSeq.rnf x__
_CreateReq'Options'OneTime ::
  Data.ProtoLens.Prism.Prism' CreateReq'Options'Mode Proto.Shared.Empty
_CreateReq'Options'OneTime
  = Data.ProtoLens.Prism.prism'
      CreateReq'Options'OneTime
      (\ p__
         -> case p__ of
              (CreateReq'Options'OneTime p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_CreateReq'Options'Transient' ::
  Data.ProtoLens.Prism.Prism' CreateReq'Options'Mode CreateReq'Options'Transient
_CreateReq'Options'Transient'
  = Data.ProtoLens.Prism.prism'
      CreateReq'Options'Transient'
      (\ p__
         -> case p__ of
              (CreateReq'Options'Transient' p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_CreateReq'Options'Continuous' ::
  Data.ProtoLens.Prism.Prism' CreateReq'Options'Mode CreateReq'Options'Continuous
_CreateReq'Options'Continuous'
  = Data.ProtoLens.Prism.prism'
      CreateReq'Options'Continuous'
      (\ p__
         -> case p__ of
              (CreateReq'Options'Continuous' p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
     
         * 'Proto.Projections_Fields.name' @:: Lens' CreateReq'Options'Continuous Data.Text.Text@
         * 'Proto.Projections_Fields.trackEmittedStreams' @:: Lens' CreateReq'Options'Continuous Prelude.Bool@ -}
data CreateReq'Options'Continuous
  = CreateReq'Options'Continuous'_constructor {_CreateReq'Options'Continuous'name :: !Data.Text.Text,
                                               _CreateReq'Options'Continuous'trackEmittedStreams :: !Prelude.Bool,
                                               _CreateReq'Options'Continuous'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show CreateReq'Options'Continuous where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField CreateReq'Options'Continuous "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CreateReq'Options'Continuous'name
           (\ x__ y__ -> x__ {_CreateReq'Options'Continuous'name = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField CreateReq'Options'Continuous "trackEmittedStreams" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CreateReq'Options'Continuous'trackEmittedStreams
           (\ x__ y__
              -> x__ {_CreateReq'Options'Continuous'trackEmittedStreams = y__}))
        Prelude.id
instance Data.ProtoLens.Message CreateReq'Options'Continuous where
  messageName _
    = Data.Text.pack
        "event_store.client.projections.CreateReq.Options.Continuous"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \Continuous\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC22\n\
      \\NAKtrack_emitted_streams\CAN\STX \SOH(\bR\DC3trackEmittedStreams"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor CreateReq'Options'Continuous
        trackEmittedStreams__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "track_emitted_streams"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"trackEmittedStreams")) ::
              Data.ProtoLens.FieldDescriptor CreateReq'Options'Continuous
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, name__field_descriptor),
           (Data.ProtoLens.Tag 2, trackEmittedStreams__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _CreateReq'Options'Continuous'_unknownFields
        (\ x__ y__
           -> x__ {_CreateReq'Options'Continuous'_unknownFields = y__})
  defMessage
    = CreateReq'Options'Continuous'_constructor
        {_CreateReq'Options'Continuous'name = Data.ProtoLens.fieldDefault,
         _CreateReq'Options'Continuous'trackEmittedStreams = Data.ProtoLens.fieldDefault,
         _CreateReq'Options'Continuous'_unknownFields = []}
  parseMessage
    = let
        loop ::
          CreateReq'Options'Continuous
          -> Data.ProtoLens.Encoding.Bytes.Parser CreateReq'Options'Continuous
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
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "track_emitted_streams"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"trackEmittedStreams") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Continuous"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
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
                (let
                   _v
                     = Lens.Family2.view
                         (Data.ProtoLens.Field.field @"trackEmittedStreams") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt
                            (\ b -> if b then 1 else 0)
                            _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData CreateReq'Options'Continuous where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_CreateReq'Options'Continuous'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_CreateReq'Options'Continuous'name x__)
                (Control.DeepSeq.deepseq
                   (_CreateReq'Options'Continuous'trackEmittedStreams x__) ()))
{- | Fields :
     
         * 'Proto.Projections_Fields.name' @:: Lens' CreateReq'Options'Transient Data.Text.Text@ -}
data CreateReq'Options'Transient
  = CreateReq'Options'Transient'_constructor {_CreateReq'Options'Transient'name :: !Data.Text.Text,
                                              _CreateReq'Options'Transient'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show CreateReq'Options'Transient where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField CreateReq'Options'Transient "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CreateReq'Options'Transient'name
           (\ x__ y__ -> x__ {_CreateReq'Options'Transient'name = y__}))
        Prelude.id
instance Data.ProtoLens.Message CreateReq'Options'Transient where
  messageName _
    = Data.Text.pack
        "event_store.client.projections.CreateReq.Options.Transient"
  packedMessageDescriptor _
    = "\n\
      \\tTransient\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor CreateReq'Options'Transient
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, name__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _CreateReq'Options'Transient'_unknownFields
        (\ x__ y__
           -> x__ {_CreateReq'Options'Transient'_unknownFields = y__})
  defMessage
    = CreateReq'Options'Transient'_constructor
        {_CreateReq'Options'Transient'name = Data.ProtoLens.fieldDefault,
         _CreateReq'Options'Transient'_unknownFields = []}
  parseMessage
    = let
        loop ::
          CreateReq'Options'Transient
          -> Data.ProtoLens.Encoding.Bytes.Parser CreateReq'Options'Transient
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
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Transient"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
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
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData CreateReq'Options'Transient where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_CreateReq'Options'Transient'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_CreateReq'Options'Transient'name x__) ())
{- | Fields :
      -}
data CreateResp
  = CreateResp'_constructor {_CreateResp'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show CreateResp where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message CreateResp where
  messageName _
    = Data.Text.pack "event_store.client.projections.CreateResp"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \CreateResp"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _CreateResp'_unknownFields
        (\ x__ y__ -> x__ {_CreateResp'_unknownFields = y__})
  defMessage
    = CreateResp'_constructor {_CreateResp'_unknownFields = []}
  parseMessage
    = let
        loop ::
          CreateResp -> Data.ProtoLens.Encoding.Bytes.Parser CreateResp
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
                      case tag of {
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x) }
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "CreateResp"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData CreateResp where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq (_CreateResp'_unknownFields x__) ()
{- | Fields :
     
         * 'Proto.Projections_Fields.options' @:: Lens' DeleteReq DeleteReq'Options@
         * 'Proto.Projections_Fields.maybe'options' @:: Lens' DeleteReq (Prelude.Maybe DeleteReq'Options)@ -}
data DeleteReq
  = DeleteReq'_constructor {_DeleteReq'options :: !(Prelude.Maybe DeleteReq'Options),
                            _DeleteReq'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DeleteReq where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField DeleteReq "options" DeleteReq'Options where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DeleteReq'options (\ x__ y__ -> x__ {_DeleteReq'options = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField DeleteReq "maybe'options" (Prelude.Maybe DeleteReq'Options) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DeleteReq'options (\ x__ y__ -> x__ {_DeleteReq'options = y__}))
        Prelude.id
instance Data.ProtoLens.Message DeleteReq where
  messageName _
    = Data.Text.pack "event_store.client.projections.DeleteReq"
  packedMessageDescriptor _
    = "\n\
      \\tDeleteReq\DC2K\n\
      \\aoptions\CAN\SOH \SOH(\v21.event_store.client.projections.DeleteReq.OptionsR\aoptions\SUB\189\SOH\n\
      \\aOptions\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC24\n\
      \\SYNdelete_emitted_streams\CAN\STX \SOH(\bR\DC4deleteEmittedStreams\DC2.\n\
      \\DC3delete_state_stream\CAN\ETX \SOH(\bR\DC1deleteStateStream\DC28\n\
      \\CANdelete_checkpoint_stream\CAN\EOT \SOH(\bR\SYNdeleteCheckpointStream"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        options__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "options"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor DeleteReq'Options)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'options")) ::
              Data.ProtoLens.FieldDescriptor DeleteReq
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, options__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DeleteReq'_unknownFields
        (\ x__ y__ -> x__ {_DeleteReq'_unknownFields = y__})
  defMessage
    = DeleteReq'_constructor
        {_DeleteReq'options = Prelude.Nothing,
         _DeleteReq'_unknownFields = []}
  parseMessage
    = let
        loop :: DeleteReq -> Data.ProtoLens.Encoding.Bytes.Parser DeleteReq
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
                                       "options"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"options") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "DeleteReq"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'options") _x
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
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData DeleteReq where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DeleteReq'_unknownFields x__)
             (Control.DeepSeq.deepseq (_DeleteReq'options x__) ())
{- | Fields :
     
         * 'Proto.Projections_Fields.name' @:: Lens' DeleteReq'Options Data.Text.Text@
         * 'Proto.Projections_Fields.deleteEmittedStreams' @:: Lens' DeleteReq'Options Prelude.Bool@
         * 'Proto.Projections_Fields.deleteStateStream' @:: Lens' DeleteReq'Options Prelude.Bool@
         * 'Proto.Projections_Fields.deleteCheckpointStream' @:: Lens' DeleteReq'Options Prelude.Bool@ -}
data DeleteReq'Options
  = DeleteReq'Options'_constructor {_DeleteReq'Options'name :: !Data.Text.Text,
                                    _DeleteReq'Options'deleteEmittedStreams :: !Prelude.Bool,
                                    _DeleteReq'Options'deleteStateStream :: !Prelude.Bool,
                                    _DeleteReq'Options'deleteCheckpointStream :: !Prelude.Bool,
                                    _DeleteReq'Options'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DeleteReq'Options where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField DeleteReq'Options "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DeleteReq'Options'name
           (\ x__ y__ -> x__ {_DeleteReq'Options'name = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField DeleteReq'Options "deleteEmittedStreams" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DeleteReq'Options'deleteEmittedStreams
           (\ x__ y__ -> x__ {_DeleteReq'Options'deleteEmittedStreams = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField DeleteReq'Options "deleteStateStream" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DeleteReq'Options'deleteStateStream
           (\ x__ y__ -> x__ {_DeleteReq'Options'deleteStateStream = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField DeleteReq'Options "deleteCheckpointStream" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DeleteReq'Options'deleteCheckpointStream
           (\ x__ y__
              -> x__ {_DeleteReq'Options'deleteCheckpointStream = y__}))
        Prelude.id
instance Data.ProtoLens.Message DeleteReq'Options where
  messageName _
    = Data.Text.pack "event_store.client.projections.DeleteReq.Options"
  packedMessageDescriptor _
    = "\n\
      \\aOptions\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC24\n\
      \\SYNdelete_emitted_streams\CAN\STX \SOH(\bR\DC4deleteEmittedStreams\DC2.\n\
      \\DC3delete_state_stream\CAN\ETX \SOH(\bR\DC1deleteStateStream\DC28\n\
      \\CANdelete_checkpoint_stream\CAN\EOT \SOH(\bR\SYNdeleteCheckpointStream"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor DeleteReq'Options
        deleteEmittedStreams__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "delete_emitted_streams"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"deleteEmittedStreams")) ::
              Data.ProtoLens.FieldDescriptor DeleteReq'Options
        deleteStateStream__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "delete_state_stream"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"deleteStateStream")) ::
              Data.ProtoLens.FieldDescriptor DeleteReq'Options
        deleteCheckpointStream__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "delete_checkpoint_stream"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"deleteCheckpointStream")) ::
              Data.ProtoLens.FieldDescriptor DeleteReq'Options
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, name__field_descriptor),
           (Data.ProtoLens.Tag 2, deleteEmittedStreams__field_descriptor),
           (Data.ProtoLens.Tag 3, deleteStateStream__field_descriptor),
           (Data.ProtoLens.Tag 4, deleteCheckpointStream__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DeleteReq'Options'_unknownFields
        (\ x__ y__ -> x__ {_DeleteReq'Options'_unknownFields = y__})
  defMessage
    = DeleteReq'Options'_constructor
        {_DeleteReq'Options'name = Data.ProtoLens.fieldDefault,
         _DeleteReq'Options'deleteEmittedStreams = Data.ProtoLens.fieldDefault,
         _DeleteReq'Options'deleteStateStream = Data.ProtoLens.fieldDefault,
         _DeleteReq'Options'deleteCheckpointStream = Data.ProtoLens.fieldDefault,
         _DeleteReq'Options'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DeleteReq'Options
          -> Data.ProtoLens.Encoding.Bytes.Parser DeleteReq'Options
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
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "delete_emitted_streams"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"deleteEmittedStreams") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "delete_state_stream"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"deleteStateStream") y x)
                        32
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "delete_checkpoint_stream"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"deleteCheckpointStream") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Options"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
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
                (let
                   _v
                     = Lens.Family2.view
                         (Data.ProtoLens.Field.field @"deleteEmittedStreams") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt
                            (\ b -> if b then 1 else 0)
                            _v))
                ((Data.Monoid.<>)
                   (let
                      _v
                        = Lens.Family2.view
                            (Data.ProtoLens.Field.field @"deleteStateStream") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            ((Prelude..)
                               Data.ProtoLens.Encoding.Bytes.putVarInt
                               (\ b -> if b then 1 else 0)
                               _v))
                   ((Data.Monoid.<>)
                      (let
                         _v
                           = Lens.Family2.view
                               (Data.ProtoLens.Field.field @"deleteCheckpointStream") _x
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
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData DeleteReq'Options where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DeleteReq'Options'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_DeleteReq'Options'name x__)
                (Control.DeepSeq.deepseq
                   (_DeleteReq'Options'deleteEmittedStreams x__)
                   (Control.DeepSeq.deepseq
                      (_DeleteReq'Options'deleteStateStream x__)
                      (Control.DeepSeq.deepseq
                         (_DeleteReq'Options'deleteCheckpointStream x__) ()))))
{- | Fields :
      -}
data DeleteResp
  = DeleteResp'_constructor {_DeleteResp'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DeleteResp where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message DeleteResp where
  messageName _
    = Data.Text.pack "event_store.client.projections.DeleteResp"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \DeleteResp"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DeleteResp'_unknownFields
        (\ x__ y__ -> x__ {_DeleteResp'_unknownFields = y__})
  defMessage
    = DeleteResp'_constructor {_DeleteResp'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DeleteResp -> Data.ProtoLens.Encoding.Bytes.Parser DeleteResp
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
                      case tag of {
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x) }
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "DeleteResp"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData DeleteResp where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq (_DeleteResp'_unknownFields x__) ()
{- | Fields :
     
         * 'Proto.Projections_Fields.options' @:: Lens' DisableReq DisableReq'Options@
         * 'Proto.Projections_Fields.maybe'options' @:: Lens' DisableReq (Prelude.Maybe DisableReq'Options)@ -}
data DisableReq
  = DisableReq'_constructor {_DisableReq'options :: !(Prelude.Maybe DisableReq'Options),
                             _DisableReq'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DisableReq where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField DisableReq "options" DisableReq'Options where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DisableReq'options (\ x__ y__ -> x__ {_DisableReq'options = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField DisableReq "maybe'options" (Prelude.Maybe DisableReq'Options) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DisableReq'options (\ x__ y__ -> x__ {_DisableReq'options = y__}))
        Prelude.id
instance Data.ProtoLens.Message DisableReq where
  messageName _
    = Data.Text.pack "event_store.client.projections.DisableReq"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \DisableReq\DC2L\n\
      \\aoptions\CAN\SOH \SOH(\v22.event_store.client.projections.DisableReq.OptionsR\aoptions\SUBH\n\
      \\aOptions\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2)\n\
      \\DLEwrite_checkpoint\CAN\STX \SOH(\bR\SIwriteCheckpoint"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        options__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "options"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor DisableReq'Options)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'options")) ::
              Data.ProtoLens.FieldDescriptor DisableReq
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, options__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DisableReq'_unknownFields
        (\ x__ y__ -> x__ {_DisableReq'_unknownFields = y__})
  defMessage
    = DisableReq'_constructor
        {_DisableReq'options = Prelude.Nothing,
         _DisableReq'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DisableReq -> Data.ProtoLens.Encoding.Bytes.Parser DisableReq
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
                                       "options"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"options") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "DisableReq"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'options") _x
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
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData DisableReq where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DisableReq'_unknownFields x__)
             (Control.DeepSeq.deepseq (_DisableReq'options x__) ())
{- | Fields :
     
         * 'Proto.Projections_Fields.name' @:: Lens' DisableReq'Options Data.Text.Text@
         * 'Proto.Projections_Fields.writeCheckpoint' @:: Lens' DisableReq'Options Prelude.Bool@ -}
data DisableReq'Options
  = DisableReq'Options'_constructor {_DisableReq'Options'name :: !Data.Text.Text,
                                     _DisableReq'Options'writeCheckpoint :: !Prelude.Bool,
                                     _DisableReq'Options'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DisableReq'Options where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField DisableReq'Options "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DisableReq'Options'name
           (\ x__ y__ -> x__ {_DisableReq'Options'name = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField DisableReq'Options "writeCheckpoint" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DisableReq'Options'writeCheckpoint
           (\ x__ y__ -> x__ {_DisableReq'Options'writeCheckpoint = y__}))
        Prelude.id
instance Data.ProtoLens.Message DisableReq'Options where
  messageName _
    = Data.Text.pack
        "event_store.client.projections.DisableReq.Options"
  packedMessageDescriptor _
    = "\n\
      \\aOptions\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2)\n\
      \\DLEwrite_checkpoint\CAN\STX \SOH(\bR\SIwriteCheckpoint"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor DisableReq'Options
        writeCheckpoint__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "write_checkpoint"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"writeCheckpoint")) ::
              Data.ProtoLens.FieldDescriptor DisableReq'Options
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, name__field_descriptor),
           (Data.ProtoLens.Tag 2, writeCheckpoint__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DisableReq'Options'_unknownFields
        (\ x__ y__ -> x__ {_DisableReq'Options'_unknownFields = y__})
  defMessage
    = DisableReq'Options'_constructor
        {_DisableReq'Options'name = Data.ProtoLens.fieldDefault,
         _DisableReq'Options'writeCheckpoint = Data.ProtoLens.fieldDefault,
         _DisableReq'Options'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DisableReq'Options
          -> Data.ProtoLens.Encoding.Bytes.Parser DisableReq'Options
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
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "write_checkpoint"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"writeCheckpoint") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Options"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
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
                (let
                   _v
                     = Lens.Family2.view
                         (Data.ProtoLens.Field.field @"writeCheckpoint") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt
                            (\ b -> if b then 1 else 0)
                            _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData DisableReq'Options where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DisableReq'Options'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_DisableReq'Options'name x__)
                (Control.DeepSeq.deepseq
                   (_DisableReq'Options'writeCheckpoint x__) ()))
{- | Fields :
      -}
data DisableResp
  = DisableResp'_constructor {_DisableResp'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DisableResp where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message DisableResp where
  messageName _
    = Data.Text.pack "event_store.client.projections.DisableResp"
  packedMessageDescriptor _
    = "\n\
      \\vDisableResp"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DisableResp'_unknownFields
        (\ x__ y__ -> x__ {_DisableResp'_unknownFields = y__})
  defMessage
    = DisableResp'_constructor {_DisableResp'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DisableResp -> Data.ProtoLens.Encoding.Bytes.Parser DisableResp
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
                      case tag of {
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x) }
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "DisableResp"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData DisableResp where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq (_DisableResp'_unknownFields x__) ()
{- | Fields :
     
         * 'Proto.Projections_Fields.options' @:: Lens' EnableReq EnableReq'Options@
         * 'Proto.Projections_Fields.maybe'options' @:: Lens' EnableReq (Prelude.Maybe EnableReq'Options)@ -}
data EnableReq
  = EnableReq'_constructor {_EnableReq'options :: !(Prelude.Maybe EnableReq'Options),
                            _EnableReq'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show EnableReq where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField EnableReq "options" EnableReq'Options where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _EnableReq'options (\ x__ y__ -> x__ {_EnableReq'options = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField EnableReq "maybe'options" (Prelude.Maybe EnableReq'Options) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _EnableReq'options (\ x__ y__ -> x__ {_EnableReq'options = y__}))
        Prelude.id
instance Data.ProtoLens.Message EnableReq where
  messageName _
    = Data.Text.pack "event_store.client.projections.EnableReq"
  packedMessageDescriptor _
    = "\n\
      \\tEnableReq\DC2K\n\
      \\aoptions\CAN\SOH \SOH(\v21.event_store.client.projections.EnableReq.OptionsR\aoptions\SUB\GS\n\
      \\aOptions\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        options__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "options"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor EnableReq'Options)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'options")) ::
              Data.ProtoLens.FieldDescriptor EnableReq
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, options__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _EnableReq'_unknownFields
        (\ x__ y__ -> x__ {_EnableReq'_unknownFields = y__})
  defMessage
    = EnableReq'_constructor
        {_EnableReq'options = Prelude.Nothing,
         _EnableReq'_unknownFields = []}
  parseMessage
    = let
        loop :: EnableReq -> Data.ProtoLens.Encoding.Bytes.Parser EnableReq
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
                                       "options"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"options") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "EnableReq"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'options") _x
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
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData EnableReq where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_EnableReq'_unknownFields x__)
             (Control.DeepSeq.deepseq (_EnableReq'options x__) ())
{- | Fields :
     
         * 'Proto.Projections_Fields.name' @:: Lens' EnableReq'Options Data.Text.Text@ -}
data EnableReq'Options
  = EnableReq'Options'_constructor {_EnableReq'Options'name :: !Data.Text.Text,
                                    _EnableReq'Options'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show EnableReq'Options where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField EnableReq'Options "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _EnableReq'Options'name
           (\ x__ y__ -> x__ {_EnableReq'Options'name = y__}))
        Prelude.id
instance Data.ProtoLens.Message EnableReq'Options where
  messageName _
    = Data.Text.pack "event_store.client.projections.EnableReq.Options"
  packedMessageDescriptor _
    = "\n\
      \\aOptions\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor EnableReq'Options
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, name__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _EnableReq'Options'_unknownFields
        (\ x__ y__ -> x__ {_EnableReq'Options'_unknownFields = y__})
  defMessage
    = EnableReq'Options'_constructor
        {_EnableReq'Options'name = Data.ProtoLens.fieldDefault,
         _EnableReq'Options'_unknownFields = []}
  parseMessage
    = let
        loop ::
          EnableReq'Options
          -> Data.ProtoLens.Encoding.Bytes.Parser EnableReq'Options
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
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Options"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
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
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData EnableReq'Options where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_EnableReq'Options'_unknownFields x__)
             (Control.DeepSeq.deepseq (_EnableReq'Options'name x__) ())
{- | Fields :
      -}
data EnableResp
  = EnableResp'_constructor {_EnableResp'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show EnableResp where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message EnableResp where
  messageName _
    = Data.Text.pack "event_store.client.projections.EnableResp"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \EnableResp"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _EnableResp'_unknownFields
        (\ x__ y__ -> x__ {_EnableResp'_unknownFields = y__})
  defMessage
    = EnableResp'_constructor {_EnableResp'_unknownFields = []}
  parseMessage
    = let
        loop ::
          EnableResp -> Data.ProtoLens.Encoding.Bytes.Parser EnableResp
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
                      case tag of {
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x) }
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "EnableResp"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData EnableResp where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq (_EnableResp'_unknownFields x__) ()
{- | Fields :
     
         * 'Proto.Projections_Fields.options' @:: Lens' ResetReq ResetReq'Options@
         * 'Proto.Projections_Fields.maybe'options' @:: Lens' ResetReq (Prelude.Maybe ResetReq'Options)@ -}
data ResetReq
  = ResetReq'_constructor {_ResetReq'options :: !(Prelude.Maybe ResetReq'Options),
                           _ResetReq'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ResetReq where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ResetReq "options" ResetReq'Options where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResetReq'options (\ x__ y__ -> x__ {_ResetReq'options = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ResetReq "maybe'options" (Prelude.Maybe ResetReq'Options) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResetReq'options (\ x__ y__ -> x__ {_ResetReq'options = y__}))
        Prelude.id
instance Data.ProtoLens.Message ResetReq where
  messageName _
    = Data.Text.pack "event_store.client.projections.ResetReq"
  packedMessageDescriptor _
    = "\n\
      \\bResetReq\DC2J\n\
      \\aoptions\CAN\SOH \SOH(\v20.event_store.client.projections.ResetReq.OptionsR\aoptions\SUBH\n\
      \\aOptions\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2)\n\
      \\DLEwrite_checkpoint\CAN\STX \SOH(\bR\SIwriteCheckpoint"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        options__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "options"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ResetReq'Options)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'options")) ::
              Data.ProtoLens.FieldDescriptor ResetReq
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, options__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ResetReq'_unknownFields
        (\ x__ y__ -> x__ {_ResetReq'_unknownFields = y__})
  defMessage
    = ResetReq'_constructor
        {_ResetReq'options = Prelude.Nothing,
         _ResetReq'_unknownFields = []}
  parseMessage
    = let
        loop :: ResetReq -> Data.ProtoLens.Encoding.Bytes.Parser ResetReq
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
                                       "options"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"options") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ResetReq"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'options") _x
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
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ResetReq where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ResetReq'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ResetReq'options x__) ())
{- | Fields :
     
         * 'Proto.Projections_Fields.name' @:: Lens' ResetReq'Options Data.Text.Text@
         * 'Proto.Projections_Fields.writeCheckpoint' @:: Lens' ResetReq'Options Prelude.Bool@ -}
data ResetReq'Options
  = ResetReq'Options'_constructor {_ResetReq'Options'name :: !Data.Text.Text,
                                   _ResetReq'Options'writeCheckpoint :: !Prelude.Bool,
                                   _ResetReq'Options'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ResetReq'Options where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ResetReq'Options "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResetReq'Options'name
           (\ x__ y__ -> x__ {_ResetReq'Options'name = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ResetReq'Options "writeCheckpoint" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResetReq'Options'writeCheckpoint
           (\ x__ y__ -> x__ {_ResetReq'Options'writeCheckpoint = y__}))
        Prelude.id
instance Data.ProtoLens.Message ResetReq'Options where
  messageName _
    = Data.Text.pack "event_store.client.projections.ResetReq.Options"
  packedMessageDescriptor _
    = "\n\
      \\aOptions\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2)\n\
      \\DLEwrite_checkpoint\CAN\STX \SOH(\bR\SIwriteCheckpoint"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor ResetReq'Options
        writeCheckpoint__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "write_checkpoint"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"writeCheckpoint")) ::
              Data.ProtoLens.FieldDescriptor ResetReq'Options
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, name__field_descriptor),
           (Data.ProtoLens.Tag 2, writeCheckpoint__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ResetReq'Options'_unknownFields
        (\ x__ y__ -> x__ {_ResetReq'Options'_unknownFields = y__})
  defMessage
    = ResetReq'Options'_constructor
        {_ResetReq'Options'name = Data.ProtoLens.fieldDefault,
         _ResetReq'Options'writeCheckpoint = Data.ProtoLens.fieldDefault,
         _ResetReq'Options'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ResetReq'Options
          -> Data.ProtoLens.Encoding.Bytes.Parser ResetReq'Options
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
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "write_checkpoint"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"writeCheckpoint") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Options"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
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
                (let
                   _v
                     = Lens.Family2.view
                         (Data.ProtoLens.Field.field @"writeCheckpoint") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt
                            (\ b -> if b then 1 else 0)
                            _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ResetReq'Options where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ResetReq'Options'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ResetReq'Options'name x__)
                (Control.DeepSeq.deepseq
                   (_ResetReq'Options'writeCheckpoint x__) ()))
{- | Fields :
      -}
data ResetResp
  = ResetResp'_constructor {_ResetResp'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ResetResp where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message ResetResp where
  messageName _
    = Data.Text.pack "event_store.client.projections.ResetResp"
  packedMessageDescriptor _
    = "\n\
      \\tResetResp"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ResetResp'_unknownFields
        (\ x__ y__ -> x__ {_ResetResp'_unknownFields = y__})
  defMessage
    = ResetResp'_constructor {_ResetResp'_unknownFields = []}
  parseMessage
    = let
        loop :: ResetResp -> Data.ProtoLens.Encoding.Bytes.Parser ResetResp
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
                      case tag of {
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x) }
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ResetResp"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData ResetResp where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq (_ResetResp'_unknownFields x__) ()
{- | Fields :
     
         * 'Proto.Projections_Fields.options' @:: Lens' ResultReq ResultReq'Options@
         * 'Proto.Projections_Fields.maybe'options' @:: Lens' ResultReq (Prelude.Maybe ResultReq'Options)@ -}
data ResultReq
  = ResultReq'_constructor {_ResultReq'options :: !(Prelude.Maybe ResultReq'Options),
                            _ResultReq'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ResultReq where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ResultReq "options" ResultReq'Options where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResultReq'options (\ x__ y__ -> x__ {_ResultReq'options = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ResultReq "maybe'options" (Prelude.Maybe ResultReq'Options) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResultReq'options (\ x__ y__ -> x__ {_ResultReq'options = y__}))
        Prelude.id
instance Data.ProtoLens.Message ResultReq where
  messageName _
    = Data.Text.pack "event_store.client.projections.ResultReq"
  packedMessageDescriptor _
    = "\n\
      \\tResultReq\DC2K\n\
      \\aoptions\CAN\SOH \SOH(\v21.event_store.client.projections.ResultReq.OptionsR\aoptions\SUB;\n\
      \\aOptions\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2\FS\n\
      \\tpartition\CAN\STX \SOH(\tR\tpartition"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        options__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "options"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ResultReq'Options)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'options")) ::
              Data.ProtoLens.FieldDescriptor ResultReq
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, options__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ResultReq'_unknownFields
        (\ x__ y__ -> x__ {_ResultReq'_unknownFields = y__})
  defMessage
    = ResultReq'_constructor
        {_ResultReq'options = Prelude.Nothing,
         _ResultReq'_unknownFields = []}
  parseMessage
    = let
        loop :: ResultReq -> Data.ProtoLens.Encoding.Bytes.Parser ResultReq
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
                                       "options"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"options") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ResultReq"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'options") _x
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
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ResultReq where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ResultReq'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ResultReq'options x__) ())
{- | Fields :
     
         * 'Proto.Projections_Fields.name' @:: Lens' ResultReq'Options Data.Text.Text@
         * 'Proto.Projections_Fields.partition' @:: Lens' ResultReq'Options Data.Text.Text@ -}
data ResultReq'Options
  = ResultReq'Options'_constructor {_ResultReq'Options'name :: !Data.Text.Text,
                                    _ResultReq'Options'partition :: !Data.Text.Text,
                                    _ResultReq'Options'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ResultReq'Options where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ResultReq'Options "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResultReq'Options'name
           (\ x__ y__ -> x__ {_ResultReq'Options'name = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ResultReq'Options "partition" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResultReq'Options'partition
           (\ x__ y__ -> x__ {_ResultReq'Options'partition = y__}))
        Prelude.id
instance Data.ProtoLens.Message ResultReq'Options where
  messageName _
    = Data.Text.pack "event_store.client.projections.ResultReq.Options"
  packedMessageDescriptor _
    = "\n\
      \\aOptions\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2\FS\n\
      \\tpartition\CAN\STX \SOH(\tR\tpartition"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor ResultReq'Options
        partition__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "partition"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"partition")) ::
              Data.ProtoLens.FieldDescriptor ResultReq'Options
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, name__field_descriptor),
           (Data.ProtoLens.Tag 2, partition__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ResultReq'Options'_unknownFields
        (\ x__ y__ -> x__ {_ResultReq'Options'_unknownFields = y__})
  defMessage
    = ResultReq'Options'_constructor
        {_ResultReq'Options'name = Data.ProtoLens.fieldDefault,
         _ResultReq'Options'partition = Data.ProtoLens.fieldDefault,
         _ResultReq'Options'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ResultReq'Options
          -> Data.ProtoLens.Encoding.Bytes.Parser ResultReq'Options
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
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "partition"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"partition") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Options"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
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
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"partition") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8
                            _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ResultReq'Options where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ResultReq'Options'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ResultReq'Options'name x__)
                (Control.DeepSeq.deepseq (_ResultReq'Options'partition x__) ()))
{- | Fields :
     
         * 'Proto.Projections_Fields.result' @:: Lens' ResultResp Proto.Google.Protobuf.Struct.Value@
         * 'Proto.Projections_Fields.maybe'result' @:: Lens' ResultResp (Prelude.Maybe Proto.Google.Protobuf.Struct.Value)@ -}
data ResultResp
  = ResultResp'_constructor {_ResultResp'result :: !(Prelude.Maybe Proto.Google.Protobuf.Struct.Value),
                             _ResultResp'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ResultResp where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ResultResp "result" Proto.Google.Protobuf.Struct.Value where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResultResp'result (\ x__ y__ -> x__ {_ResultResp'result = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ResultResp "maybe'result" (Prelude.Maybe Proto.Google.Protobuf.Struct.Value) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResultResp'result (\ x__ y__ -> x__ {_ResultResp'result = y__}))
        Prelude.id
instance Data.ProtoLens.Message ResultResp where
  messageName _
    = Data.Text.pack "event_store.client.projections.ResultResp"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \ResultResp\DC2.\n\
      \\ACKresult\CAN\SOH \SOH(\v2\SYN.google.protobuf.ValueR\ACKresult"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        result__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "result"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Google.Protobuf.Struct.Value)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'result")) ::
              Data.ProtoLens.FieldDescriptor ResultResp
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, result__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ResultResp'_unknownFields
        (\ x__ y__ -> x__ {_ResultResp'_unknownFields = y__})
  defMessage
    = ResultResp'_constructor
        {_ResultResp'result = Prelude.Nothing,
         _ResultResp'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ResultResp -> Data.ProtoLens.Encoding.Bytes.Parser ResultResp
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
                                       "result"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"result") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ResultResp"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'result") _x
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
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ResultResp where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ResultResp'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ResultResp'result x__) ())
{- | Fields :
     
         * 'Proto.Projections_Fields.options' @:: Lens' StateReq StateReq'Options@
         * 'Proto.Projections_Fields.maybe'options' @:: Lens' StateReq (Prelude.Maybe StateReq'Options)@ -}
data StateReq
  = StateReq'_constructor {_StateReq'options :: !(Prelude.Maybe StateReq'Options),
                           _StateReq'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StateReq where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField StateReq "options" StateReq'Options where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StateReq'options (\ x__ y__ -> x__ {_StateReq'options = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField StateReq "maybe'options" (Prelude.Maybe StateReq'Options) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StateReq'options (\ x__ y__ -> x__ {_StateReq'options = y__}))
        Prelude.id
instance Data.ProtoLens.Message StateReq where
  messageName _
    = Data.Text.pack "event_store.client.projections.StateReq"
  packedMessageDescriptor _
    = "\n\
      \\bStateReq\DC2J\n\
      \\aoptions\CAN\SOH \SOH(\v20.event_store.client.projections.StateReq.OptionsR\aoptions\SUB;\n\
      \\aOptions\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2\FS\n\
      \\tpartition\CAN\STX \SOH(\tR\tpartition"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        options__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "options"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor StateReq'Options)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'options")) ::
              Data.ProtoLens.FieldDescriptor StateReq
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, options__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _StateReq'_unknownFields
        (\ x__ y__ -> x__ {_StateReq'_unknownFields = y__})
  defMessage
    = StateReq'_constructor
        {_StateReq'options = Prelude.Nothing,
         _StateReq'_unknownFields = []}
  parseMessage
    = let
        loop :: StateReq -> Data.ProtoLens.Encoding.Bytes.Parser StateReq
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
                                       "options"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"options") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "StateReq"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'options") _x
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
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData StateReq where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_StateReq'_unknownFields x__)
             (Control.DeepSeq.deepseq (_StateReq'options x__) ())
{- | Fields :
     
         * 'Proto.Projections_Fields.name' @:: Lens' StateReq'Options Data.Text.Text@
         * 'Proto.Projections_Fields.partition' @:: Lens' StateReq'Options Data.Text.Text@ -}
data StateReq'Options
  = StateReq'Options'_constructor {_StateReq'Options'name :: !Data.Text.Text,
                                   _StateReq'Options'partition :: !Data.Text.Text,
                                   _StateReq'Options'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StateReq'Options where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField StateReq'Options "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StateReq'Options'name
           (\ x__ y__ -> x__ {_StateReq'Options'name = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StateReq'Options "partition" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StateReq'Options'partition
           (\ x__ y__ -> x__ {_StateReq'Options'partition = y__}))
        Prelude.id
instance Data.ProtoLens.Message StateReq'Options where
  messageName _
    = Data.Text.pack "event_store.client.projections.StateReq.Options"
  packedMessageDescriptor _
    = "\n\
      \\aOptions\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2\FS\n\
      \\tpartition\CAN\STX \SOH(\tR\tpartition"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor StateReq'Options
        partition__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "partition"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"partition")) ::
              Data.ProtoLens.FieldDescriptor StateReq'Options
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, name__field_descriptor),
           (Data.ProtoLens.Tag 2, partition__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _StateReq'Options'_unknownFields
        (\ x__ y__ -> x__ {_StateReq'Options'_unknownFields = y__})
  defMessage
    = StateReq'Options'_constructor
        {_StateReq'Options'name = Data.ProtoLens.fieldDefault,
         _StateReq'Options'partition = Data.ProtoLens.fieldDefault,
         _StateReq'Options'_unknownFields = []}
  parseMessage
    = let
        loop ::
          StateReq'Options
          -> Data.ProtoLens.Encoding.Bytes.Parser StateReq'Options
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
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "partition"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"partition") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Options"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
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
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"partition") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8
                            _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData StateReq'Options where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_StateReq'Options'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_StateReq'Options'name x__)
                (Control.DeepSeq.deepseq (_StateReq'Options'partition x__) ()))
{- | Fields :
     
         * 'Proto.Projections_Fields.state' @:: Lens' StateResp Proto.Google.Protobuf.Struct.Value@
         * 'Proto.Projections_Fields.maybe'state' @:: Lens' StateResp (Prelude.Maybe Proto.Google.Protobuf.Struct.Value)@ -}
data StateResp
  = StateResp'_constructor {_StateResp'state :: !(Prelude.Maybe Proto.Google.Protobuf.Struct.Value),
                            _StateResp'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StateResp where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField StateResp "state" Proto.Google.Protobuf.Struct.Value where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StateResp'state (\ x__ y__ -> x__ {_StateResp'state = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField StateResp "maybe'state" (Prelude.Maybe Proto.Google.Protobuf.Struct.Value) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StateResp'state (\ x__ y__ -> x__ {_StateResp'state = y__}))
        Prelude.id
instance Data.ProtoLens.Message StateResp where
  messageName _
    = Data.Text.pack "event_store.client.projections.StateResp"
  packedMessageDescriptor _
    = "\n\
      \\tStateResp\DC2,\n\
      \\ENQstate\CAN\SOH \SOH(\v2\SYN.google.protobuf.ValueR\ENQstate"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        state__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "state"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Google.Protobuf.Struct.Value)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'state")) ::
              Data.ProtoLens.FieldDescriptor StateResp
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, state__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _StateResp'_unknownFields
        (\ x__ y__ -> x__ {_StateResp'_unknownFields = y__})
  defMessage
    = StateResp'_constructor
        {_StateResp'state = Prelude.Nothing,
         _StateResp'_unknownFields = []}
  parseMessage
    = let
        loop :: StateResp -> Data.ProtoLens.Encoding.Bytes.Parser StateResp
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
                                       "state"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"state") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "StateResp"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'state") _x
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
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData StateResp where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_StateResp'_unknownFields x__)
             (Control.DeepSeq.deepseq (_StateResp'state x__) ())
{- | Fields :
     
         * 'Proto.Projections_Fields.options' @:: Lens' StatisticsReq StatisticsReq'Options@
         * 'Proto.Projections_Fields.maybe'options' @:: Lens' StatisticsReq (Prelude.Maybe StatisticsReq'Options)@ -}
data StatisticsReq
  = StatisticsReq'_constructor {_StatisticsReq'options :: !(Prelude.Maybe StatisticsReq'Options),
                                _StatisticsReq'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StatisticsReq where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField StatisticsReq "options" StatisticsReq'Options where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsReq'options
           (\ x__ y__ -> x__ {_StatisticsReq'options = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField StatisticsReq "maybe'options" (Prelude.Maybe StatisticsReq'Options) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsReq'options
           (\ x__ y__ -> x__ {_StatisticsReq'options = y__}))
        Prelude.id
instance Data.ProtoLens.Message StatisticsReq where
  messageName _
    = Data.Text.pack "event_store.client.projections.StatisticsReq"
  packedMessageDescriptor _
    = "\n\
      \\rStatisticsReq\DC2O\n\
      \\aoptions\CAN\SOH \SOH(\v25.event_store.client.projections.StatisticsReq.OptionsR\aoptions\SUB\162\STX\n\
      \\aOptions\DC2\DC4\n\
      \\EOTname\CAN\SOH \SOH(\tH\NULR\EOTname\DC24\n\
      \\ETXall\CAN\STX \SOH(\v2 .event_store.client.shared.EmptyH\NULR\ETXall\DC2@\n\
      \\ttransient\CAN\ETX \SOH(\v2 .event_store.client.shared.EmptyH\NULR\ttransient\DC2B\n\
      \\n\
      \continuous\CAN\EOT \SOH(\v2 .event_store.client.shared.EmptyH\NULR\n\
      \continuous\DC2=\n\
      \\bone_time\CAN\ENQ \SOH(\v2 .event_store.client.shared.EmptyH\NULR\aoneTimeB\ACK\n\
      \\EOTmode"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        options__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "options"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor StatisticsReq'Options)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'options")) ::
              Data.ProtoLens.FieldDescriptor StatisticsReq
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, options__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _StatisticsReq'_unknownFields
        (\ x__ y__ -> x__ {_StatisticsReq'_unknownFields = y__})
  defMessage
    = StatisticsReq'_constructor
        {_StatisticsReq'options = Prelude.Nothing,
         _StatisticsReq'_unknownFields = []}
  parseMessage
    = let
        loop ::
          StatisticsReq -> Data.ProtoLens.Encoding.Bytes.Parser StatisticsReq
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
                                       "options"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"options") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "StatisticsReq"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'options") _x
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
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData StatisticsReq where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_StatisticsReq'_unknownFields x__)
             (Control.DeepSeq.deepseq (_StatisticsReq'options x__) ())
{- | Fields :
     
         * 'Proto.Projections_Fields.maybe'mode' @:: Lens' StatisticsReq'Options (Prelude.Maybe StatisticsReq'Options'Mode)@
         * 'Proto.Projections_Fields.maybe'name' @:: Lens' StatisticsReq'Options (Prelude.Maybe Data.Text.Text)@
         * 'Proto.Projections_Fields.name' @:: Lens' StatisticsReq'Options Data.Text.Text@
         * 'Proto.Projections_Fields.maybe'all' @:: Lens' StatisticsReq'Options (Prelude.Maybe Proto.Shared.Empty)@
         * 'Proto.Projections_Fields.all' @:: Lens' StatisticsReq'Options Proto.Shared.Empty@
         * 'Proto.Projections_Fields.maybe'transient' @:: Lens' StatisticsReq'Options (Prelude.Maybe Proto.Shared.Empty)@
         * 'Proto.Projections_Fields.transient' @:: Lens' StatisticsReq'Options Proto.Shared.Empty@
         * 'Proto.Projections_Fields.maybe'continuous' @:: Lens' StatisticsReq'Options (Prelude.Maybe Proto.Shared.Empty)@
         * 'Proto.Projections_Fields.continuous' @:: Lens' StatisticsReq'Options Proto.Shared.Empty@
         * 'Proto.Projections_Fields.maybe'oneTime' @:: Lens' StatisticsReq'Options (Prelude.Maybe Proto.Shared.Empty)@
         * 'Proto.Projections_Fields.oneTime' @:: Lens' StatisticsReq'Options Proto.Shared.Empty@ -}
data StatisticsReq'Options
  = StatisticsReq'Options'_constructor {_StatisticsReq'Options'mode :: !(Prelude.Maybe StatisticsReq'Options'Mode),
                                        _StatisticsReq'Options'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StatisticsReq'Options where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data StatisticsReq'Options'Mode
  = StatisticsReq'Options'Name !Data.Text.Text |
    StatisticsReq'Options'All !Proto.Shared.Empty |
    StatisticsReq'Options'Transient !Proto.Shared.Empty |
    StatisticsReq'Options'Continuous !Proto.Shared.Empty |
    StatisticsReq'Options'OneTime !Proto.Shared.Empty
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField StatisticsReq'Options "maybe'mode" (Prelude.Maybe StatisticsReq'Options'Mode) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsReq'Options'mode
           (\ x__ y__ -> x__ {_StatisticsReq'Options'mode = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StatisticsReq'Options "maybe'name" (Prelude.Maybe Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsReq'Options'mode
           (\ x__ y__ -> x__ {_StatisticsReq'Options'mode = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (StatisticsReq'Options'Name x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap StatisticsReq'Options'Name y__))
instance Data.ProtoLens.Field.HasField StatisticsReq'Options "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsReq'Options'mode
           (\ x__ y__ -> x__ {_StatisticsReq'Options'mode = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (StatisticsReq'Options'Name x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap StatisticsReq'Options'Name y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField StatisticsReq'Options "maybe'all" (Prelude.Maybe Proto.Shared.Empty) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsReq'Options'mode
           (\ x__ y__ -> x__ {_StatisticsReq'Options'mode = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (StatisticsReq'Options'All x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap StatisticsReq'Options'All y__))
instance Data.ProtoLens.Field.HasField StatisticsReq'Options "all" Proto.Shared.Empty where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsReq'Options'mode
           (\ x__ y__ -> x__ {_StatisticsReq'Options'mode = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (StatisticsReq'Options'All x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap StatisticsReq'Options'All y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField StatisticsReq'Options "maybe'transient" (Prelude.Maybe Proto.Shared.Empty) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsReq'Options'mode
           (\ x__ y__ -> x__ {_StatisticsReq'Options'mode = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (StatisticsReq'Options'Transient x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap StatisticsReq'Options'Transient y__))
instance Data.ProtoLens.Field.HasField StatisticsReq'Options "transient" Proto.Shared.Empty where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsReq'Options'mode
           (\ x__ y__ -> x__ {_StatisticsReq'Options'mode = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (StatisticsReq'Options'Transient x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap StatisticsReq'Options'Transient y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField StatisticsReq'Options "maybe'continuous" (Prelude.Maybe Proto.Shared.Empty) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsReq'Options'mode
           (\ x__ y__ -> x__ {_StatisticsReq'Options'mode = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (StatisticsReq'Options'Continuous x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap StatisticsReq'Options'Continuous y__))
instance Data.ProtoLens.Field.HasField StatisticsReq'Options "continuous" Proto.Shared.Empty where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsReq'Options'mode
           (\ x__ y__ -> x__ {_StatisticsReq'Options'mode = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (StatisticsReq'Options'Continuous x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap StatisticsReq'Options'Continuous y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField StatisticsReq'Options "maybe'oneTime" (Prelude.Maybe Proto.Shared.Empty) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsReq'Options'mode
           (\ x__ y__ -> x__ {_StatisticsReq'Options'mode = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (StatisticsReq'Options'OneTime x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap StatisticsReq'Options'OneTime y__))
instance Data.ProtoLens.Field.HasField StatisticsReq'Options "oneTime" Proto.Shared.Empty where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsReq'Options'mode
           (\ x__ y__ -> x__ {_StatisticsReq'Options'mode = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (StatisticsReq'Options'OneTime x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap StatisticsReq'Options'OneTime y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message StatisticsReq'Options where
  messageName _
    = Data.Text.pack
        "event_store.client.projections.StatisticsReq.Options"
  packedMessageDescriptor _
    = "\n\
      \\aOptions\DC2\DC4\n\
      \\EOTname\CAN\SOH \SOH(\tH\NULR\EOTname\DC24\n\
      \\ETXall\CAN\STX \SOH(\v2 .event_store.client.shared.EmptyH\NULR\ETXall\DC2@\n\
      \\ttransient\CAN\ETX \SOH(\v2 .event_store.client.shared.EmptyH\NULR\ttransient\DC2B\n\
      \\n\
      \continuous\CAN\EOT \SOH(\v2 .event_store.client.shared.EmptyH\NULR\n\
      \continuous\DC2=\n\
      \\bone_time\CAN\ENQ \SOH(\v2 .event_store.client.shared.EmptyH\NULR\aoneTimeB\ACK\n\
      \\EOTmode"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'name")) ::
              Data.ProtoLens.FieldDescriptor StatisticsReq'Options
        all__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "all"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.Empty)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'all")) ::
              Data.ProtoLens.FieldDescriptor StatisticsReq'Options
        transient__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "transient"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.Empty)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'transient")) ::
              Data.ProtoLens.FieldDescriptor StatisticsReq'Options
        continuous__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "continuous"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.Empty)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'continuous")) ::
              Data.ProtoLens.FieldDescriptor StatisticsReq'Options
        oneTime__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "one_time"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.Empty)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'oneTime")) ::
              Data.ProtoLens.FieldDescriptor StatisticsReq'Options
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, name__field_descriptor),
           (Data.ProtoLens.Tag 2, all__field_descriptor),
           (Data.ProtoLens.Tag 3, transient__field_descriptor),
           (Data.ProtoLens.Tag 4, continuous__field_descriptor),
           (Data.ProtoLens.Tag 5, oneTime__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _StatisticsReq'Options'_unknownFields
        (\ x__ y__ -> x__ {_StatisticsReq'Options'_unknownFields = y__})
  defMessage
    = StatisticsReq'Options'_constructor
        {_StatisticsReq'Options'mode = Prelude.Nothing,
         _StatisticsReq'Options'_unknownFields = []}
  parseMessage
    = let
        loop ::
          StatisticsReq'Options
          -> Data.ProtoLens.Encoding.Bytes.Parser StatisticsReq'Options
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
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "all"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"all") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "transient"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"transient") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "continuous"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"continuous") y x)
                        42
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "one_time"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"oneTime") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Options"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'mode") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just (StatisticsReq'Options'Name v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.Text.Encoding.encodeUtf8
                          v)
                (Prelude.Just (StatisticsReq'Options'All v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage
                          v)
                (Prelude.Just (StatisticsReq'Options'Transient v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage
                          v)
                (Prelude.Just (StatisticsReq'Options'Continuous v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage
                          v)
                (Prelude.Just (StatisticsReq'Options'OneTime v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage
                          v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData StatisticsReq'Options where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_StatisticsReq'Options'_unknownFields x__)
             (Control.DeepSeq.deepseq (_StatisticsReq'Options'mode x__) ())
instance Control.DeepSeq.NFData StatisticsReq'Options'Mode where
  rnf (StatisticsReq'Options'Name x__) = Control.DeepSeq.rnf x__
  rnf (StatisticsReq'Options'All x__) = Control.DeepSeq.rnf x__
  rnf (StatisticsReq'Options'Transient x__) = Control.DeepSeq.rnf x__
  rnf (StatisticsReq'Options'Continuous x__)
    = Control.DeepSeq.rnf x__
  rnf (StatisticsReq'Options'OneTime x__) = Control.DeepSeq.rnf x__
_StatisticsReq'Options'Name ::
  Data.ProtoLens.Prism.Prism' StatisticsReq'Options'Mode Data.Text.Text
_StatisticsReq'Options'Name
  = Data.ProtoLens.Prism.prism'
      StatisticsReq'Options'Name
      (\ p__
         -> case p__ of
              (StatisticsReq'Options'Name p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_StatisticsReq'Options'All ::
  Data.ProtoLens.Prism.Prism' StatisticsReq'Options'Mode Proto.Shared.Empty
_StatisticsReq'Options'All
  = Data.ProtoLens.Prism.prism'
      StatisticsReq'Options'All
      (\ p__
         -> case p__ of
              (StatisticsReq'Options'All p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_StatisticsReq'Options'Transient ::
  Data.ProtoLens.Prism.Prism' StatisticsReq'Options'Mode Proto.Shared.Empty
_StatisticsReq'Options'Transient
  = Data.ProtoLens.Prism.prism'
      StatisticsReq'Options'Transient
      (\ p__
         -> case p__ of
              (StatisticsReq'Options'Transient p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_StatisticsReq'Options'Continuous ::
  Data.ProtoLens.Prism.Prism' StatisticsReq'Options'Mode Proto.Shared.Empty
_StatisticsReq'Options'Continuous
  = Data.ProtoLens.Prism.prism'
      StatisticsReq'Options'Continuous
      (\ p__
         -> case p__ of
              (StatisticsReq'Options'Continuous p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_StatisticsReq'Options'OneTime ::
  Data.ProtoLens.Prism.Prism' StatisticsReq'Options'Mode Proto.Shared.Empty
_StatisticsReq'Options'OneTime
  = Data.ProtoLens.Prism.prism'
      StatisticsReq'Options'OneTime
      (\ p__
         -> case p__ of
              (StatisticsReq'Options'OneTime p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
     
         * 'Proto.Projections_Fields.details' @:: Lens' StatisticsResp StatisticsResp'Details@
         * 'Proto.Projections_Fields.maybe'details' @:: Lens' StatisticsResp (Prelude.Maybe StatisticsResp'Details)@ -}
data StatisticsResp
  = StatisticsResp'_constructor {_StatisticsResp'details :: !(Prelude.Maybe StatisticsResp'Details),
                                 _StatisticsResp'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StatisticsResp where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField StatisticsResp "details" StatisticsResp'Details where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsResp'details
           (\ x__ y__ -> x__ {_StatisticsResp'details = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField StatisticsResp "maybe'details" (Prelude.Maybe StatisticsResp'Details) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsResp'details
           (\ x__ y__ -> x__ {_StatisticsResp'details = y__}))
        Prelude.id
instance Data.ProtoLens.Message StatisticsResp where
  messageName _
    = Data.Text.pack "event_store.client.projections.StatisticsResp"
  packedMessageDescriptor _
    = "\n\
      \\SOStatisticsResp\DC2P\n\
      \\adetails\CAN\SOH \SOH(\v26.event_store.client.projections.StatisticsResp.DetailsR\adetails\SUB\135\ACK\n\
      \\aDetails\DC2.\n\
      \\DC2coreProcessingTime\CAN\SOH \SOH(\ETXR\DC2coreProcessingTime\DC2\CAN\n\
      \\aversion\CAN\STX \SOH(\ETXR\aversion\DC2\DC4\n\
      \\ENQepoch\CAN\ETX \SOH(\ETXR\ENQepoch\DC2$\n\
      \\reffectiveName\CAN\EOT \SOH(\tR\reffectiveName\DC2*\n\
      \\DLEwritesInProgress\CAN\ENQ \SOH(\ENQR\DLEwritesInProgress\DC2(\n\
      \\SIreadsInProgress\CAN\ACK \SOH(\ENQR\SIreadsInProgress\DC2*\n\
      \\DLEpartitionsCached\CAN\a \SOH(\ENQR\DLEpartitionsCached\DC2\SYN\n\
      \\ACKstatus\CAN\b \SOH(\tR\ACKstatus\DC2 \n\
      \\vstateReason\CAN\t \SOH(\tR\vstateReason\DC2\DC2\n\
      \\EOTname\CAN\n\
      \ \SOH(\tR\EOTname\DC2\DC2\n\
      \\EOTmode\CAN\v \SOH(\tR\EOTmode\DC2\SUB\n\
      \\bposition\CAN\f \SOH(\tR\bposition\DC2\SUB\n\
      \\bprogress\CAN\r \SOH(\STXR\bprogress\DC2&\n\
      \\SOlastCheckpoint\CAN\SO \SOH(\tR\SOlastCheckpoint\DC2@\n\
      \\ESCeventsProcessedAfterRestart\CAN\SI \SOH(\ETXR\ESCeventsProcessedAfterRestart\DC2*\n\
      \\DLEcheckpointStatus\CAN\DLE \SOH(\tR\DLEcheckpointStatus\DC2&\n\
      \\SObufferedEvents\CAN\DC1 \SOH(\ETXR\SObufferedEvents\DC2N\n\
      \\"writePendingEventsBeforeCheckpoint\CAN\DC2 \SOH(\ENQR\"writePendingEventsBeforeCheckpoint\DC2L\n\
      \!writePendingEventsAfterCheckpoint\CAN\DC3 \SOH(\ENQR!writePendingEventsAfterCheckpoint"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        details__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "details"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor StatisticsResp'Details)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'details")) ::
              Data.ProtoLens.FieldDescriptor StatisticsResp
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, details__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _StatisticsResp'_unknownFields
        (\ x__ y__ -> x__ {_StatisticsResp'_unknownFields = y__})
  defMessage
    = StatisticsResp'_constructor
        {_StatisticsResp'details = Prelude.Nothing,
         _StatisticsResp'_unknownFields = []}
  parseMessage
    = let
        loop ::
          StatisticsResp
          -> Data.ProtoLens.Encoding.Bytes.Parser StatisticsResp
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
                                       "details"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"details") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "StatisticsResp"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'details") _x
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
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData StatisticsResp where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_StatisticsResp'_unknownFields x__)
             (Control.DeepSeq.deepseq (_StatisticsResp'details x__) ())
{- | Fields :
     
         * 'Proto.Projections_Fields.coreProcessingTime' @:: Lens' StatisticsResp'Details Data.Int.Int64@
         * 'Proto.Projections_Fields.version' @:: Lens' StatisticsResp'Details Data.Int.Int64@
         * 'Proto.Projections_Fields.epoch' @:: Lens' StatisticsResp'Details Data.Int.Int64@
         * 'Proto.Projections_Fields.effectiveName' @:: Lens' StatisticsResp'Details Data.Text.Text@
         * 'Proto.Projections_Fields.writesInProgress' @:: Lens' StatisticsResp'Details Data.Int.Int32@
         * 'Proto.Projections_Fields.readsInProgress' @:: Lens' StatisticsResp'Details Data.Int.Int32@
         * 'Proto.Projections_Fields.partitionsCached' @:: Lens' StatisticsResp'Details Data.Int.Int32@
         * 'Proto.Projections_Fields.status' @:: Lens' StatisticsResp'Details Data.Text.Text@
         * 'Proto.Projections_Fields.stateReason' @:: Lens' StatisticsResp'Details Data.Text.Text@
         * 'Proto.Projections_Fields.name' @:: Lens' StatisticsResp'Details Data.Text.Text@
         * 'Proto.Projections_Fields.mode' @:: Lens' StatisticsResp'Details Data.Text.Text@
         * 'Proto.Projections_Fields.position' @:: Lens' StatisticsResp'Details Data.Text.Text@
         * 'Proto.Projections_Fields.progress' @:: Lens' StatisticsResp'Details Prelude.Float@
         * 'Proto.Projections_Fields.lastCheckpoint' @:: Lens' StatisticsResp'Details Data.Text.Text@
         * 'Proto.Projections_Fields.eventsProcessedAfterRestart' @:: Lens' StatisticsResp'Details Data.Int.Int64@
         * 'Proto.Projections_Fields.checkpointStatus' @:: Lens' StatisticsResp'Details Data.Text.Text@
         * 'Proto.Projections_Fields.bufferedEvents' @:: Lens' StatisticsResp'Details Data.Int.Int64@
         * 'Proto.Projections_Fields.writePendingEventsBeforeCheckpoint' @:: Lens' StatisticsResp'Details Data.Int.Int32@
         * 'Proto.Projections_Fields.writePendingEventsAfterCheckpoint' @:: Lens' StatisticsResp'Details Data.Int.Int32@ -}
data StatisticsResp'Details
  = StatisticsResp'Details'_constructor {_StatisticsResp'Details'coreProcessingTime :: !Data.Int.Int64,
                                         _StatisticsResp'Details'version :: !Data.Int.Int64,
                                         _StatisticsResp'Details'epoch :: !Data.Int.Int64,
                                         _StatisticsResp'Details'effectiveName :: !Data.Text.Text,
                                         _StatisticsResp'Details'writesInProgress :: !Data.Int.Int32,
                                         _StatisticsResp'Details'readsInProgress :: !Data.Int.Int32,
                                         _StatisticsResp'Details'partitionsCached :: !Data.Int.Int32,
                                         _StatisticsResp'Details'status :: !Data.Text.Text,
                                         _StatisticsResp'Details'stateReason :: !Data.Text.Text,
                                         _StatisticsResp'Details'name :: !Data.Text.Text,
                                         _StatisticsResp'Details'mode :: !Data.Text.Text,
                                         _StatisticsResp'Details'position :: !Data.Text.Text,
                                         _StatisticsResp'Details'progress :: !Prelude.Float,
                                         _StatisticsResp'Details'lastCheckpoint :: !Data.Text.Text,
                                         _StatisticsResp'Details'eventsProcessedAfterRestart :: !Data.Int.Int64,
                                         _StatisticsResp'Details'checkpointStatus :: !Data.Text.Text,
                                         _StatisticsResp'Details'bufferedEvents :: !Data.Int.Int64,
                                         _StatisticsResp'Details'writePendingEventsBeforeCheckpoint :: !Data.Int.Int32,
                                         _StatisticsResp'Details'writePendingEventsAfterCheckpoint :: !Data.Int.Int32,
                                         _StatisticsResp'Details'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StatisticsResp'Details where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField StatisticsResp'Details "coreProcessingTime" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsResp'Details'coreProcessingTime
           (\ x__ y__
              -> x__ {_StatisticsResp'Details'coreProcessingTime = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StatisticsResp'Details "version" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsResp'Details'version
           (\ x__ y__ -> x__ {_StatisticsResp'Details'version = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StatisticsResp'Details "epoch" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsResp'Details'epoch
           (\ x__ y__ -> x__ {_StatisticsResp'Details'epoch = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StatisticsResp'Details "effectiveName" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsResp'Details'effectiveName
           (\ x__ y__ -> x__ {_StatisticsResp'Details'effectiveName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StatisticsResp'Details "writesInProgress" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsResp'Details'writesInProgress
           (\ x__ y__
              -> x__ {_StatisticsResp'Details'writesInProgress = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StatisticsResp'Details "readsInProgress" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsResp'Details'readsInProgress
           (\ x__ y__ -> x__ {_StatisticsResp'Details'readsInProgress = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StatisticsResp'Details "partitionsCached" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsResp'Details'partitionsCached
           (\ x__ y__
              -> x__ {_StatisticsResp'Details'partitionsCached = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StatisticsResp'Details "status" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsResp'Details'status
           (\ x__ y__ -> x__ {_StatisticsResp'Details'status = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StatisticsResp'Details "stateReason" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsResp'Details'stateReason
           (\ x__ y__ -> x__ {_StatisticsResp'Details'stateReason = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StatisticsResp'Details "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsResp'Details'name
           (\ x__ y__ -> x__ {_StatisticsResp'Details'name = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StatisticsResp'Details "mode" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsResp'Details'mode
           (\ x__ y__ -> x__ {_StatisticsResp'Details'mode = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StatisticsResp'Details "position" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsResp'Details'position
           (\ x__ y__ -> x__ {_StatisticsResp'Details'position = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StatisticsResp'Details "progress" Prelude.Float where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsResp'Details'progress
           (\ x__ y__ -> x__ {_StatisticsResp'Details'progress = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StatisticsResp'Details "lastCheckpoint" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsResp'Details'lastCheckpoint
           (\ x__ y__ -> x__ {_StatisticsResp'Details'lastCheckpoint = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StatisticsResp'Details "eventsProcessedAfterRestart" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsResp'Details'eventsProcessedAfterRestart
           (\ x__ y__
              -> x__
                   {_StatisticsResp'Details'eventsProcessedAfterRestart = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StatisticsResp'Details "checkpointStatus" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsResp'Details'checkpointStatus
           (\ x__ y__
              -> x__ {_StatisticsResp'Details'checkpointStatus = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StatisticsResp'Details "bufferedEvents" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsResp'Details'bufferedEvents
           (\ x__ y__ -> x__ {_StatisticsResp'Details'bufferedEvents = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StatisticsResp'Details "writePendingEventsBeforeCheckpoint" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsResp'Details'writePendingEventsBeforeCheckpoint
           (\ x__ y__
              -> x__
                   {_StatisticsResp'Details'writePendingEventsBeforeCheckpoint = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StatisticsResp'Details "writePendingEventsAfterCheckpoint" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StatisticsResp'Details'writePendingEventsAfterCheckpoint
           (\ x__ y__
              -> x__
                   {_StatisticsResp'Details'writePendingEventsAfterCheckpoint = y__}))
        Prelude.id
instance Data.ProtoLens.Message StatisticsResp'Details where
  messageName _
    = Data.Text.pack
        "event_store.client.projections.StatisticsResp.Details"
  packedMessageDescriptor _
    = "\n\
      \\aDetails\DC2.\n\
      \\DC2coreProcessingTime\CAN\SOH \SOH(\ETXR\DC2coreProcessingTime\DC2\CAN\n\
      \\aversion\CAN\STX \SOH(\ETXR\aversion\DC2\DC4\n\
      \\ENQepoch\CAN\ETX \SOH(\ETXR\ENQepoch\DC2$\n\
      \\reffectiveName\CAN\EOT \SOH(\tR\reffectiveName\DC2*\n\
      \\DLEwritesInProgress\CAN\ENQ \SOH(\ENQR\DLEwritesInProgress\DC2(\n\
      \\SIreadsInProgress\CAN\ACK \SOH(\ENQR\SIreadsInProgress\DC2*\n\
      \\DLEpartitionsCached\CAN\a \SOH(\ENQR\DLEpartitionsCached\DC2\SYN\n\
      \\ACKstatus\CAN\b \SOH(\tR\ACKstatus\DC2 \n\
      \\vstateReason\CAN\t \SOH(\tR\vstateReason\DC2\DC2\n\
      \\EOTname\CAN\n\
      \ \SOH(\tR\EOTname\DC2\DC2\n\
      \\EOTmode\CAN\v \SOH(\tR\EOTmode\DC2\SUB\n\
      \\bposition\CAN\f \SOH(\tR\bposition\DC2\SUB\n\
      \\bprogress\CAN\r \SOH(\STXR\bprogress\DC2&\n\
      \\SOlastCheckpoint\CAN\SO \SOH(\tR\SOlastCheckpoint\DC2@\n\
      \\ESCeventsProcessedAfterRestart\CAN\SI \SOH(\ETXR\ESCeventsProcessedAfterRestart\DC2*\n\
      \\DLEcheckpointStatus\CAN\DLE \SOH(\tR\DLEcheckpointStatus\DC2&\n\
      \\SObufferedEvents\CAN\DC1 \SOH(\ETXR\SObufferedEvents\DC2N\n\
      \\"writePendingEventsBeforeCheckpoint\CAN\DC2 \SOH(\ENQR\"writePendingEventsBeforeCheckpoint\DC2L\n\
      \!writePendingEventsAfterCheckpoint\CAN\DC3 \SOH(\ENQR!writePendingEventsAfterCheckpoint"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        coreProcessingTime__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "coreProcessingTime"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"coreProcessingTime")) ::
              Data.ProtoLens.FieldDescriptor StatisticsResp'Details
        version__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "version"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"version")) ::
              Data.ProtoLens.FieldDescriptor StatisticsResp'Details
        epoch__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "epoch"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"epoch")) ::
              Data.ProtoLens.FieldDescriptor StatisticsResp'Details
        effectiveName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "effectiveName"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"effectiveName")) ::
              Data.ProtoLens.FieldDescriptor StatisticsResp'Details
        writesInProgress__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "writesInProgress"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"writesInProgress")) ::
              Data.ProtoLens.FieldDescriptor StatisticsResp'Details
        readsInProgress__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "readsInProgress"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"readsInProgress")) ::
              Data.ProtoLens.FieldDescriptor StatisticsResp'Details
        partitionsCached__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "partitionsCached"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"partitionsCached")) ::
              Data.ProtoLens.FieldDescriptor StatisticsResp'Details
        status__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "status"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"status")) ::
              Data.ProtoLens.FieldDescriptor StatisticsResp'Details
        stateReason__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "stateReason"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"stateReason")) ::
              Data.ProtoLens.FieldDescriptor StatisticsResp'Details
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor StatisticsResp'Details
        mode__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mode"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"mode")) ::
              Data.ProtoLens.FieldDescriptor StatisticsResp'Details
        position__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "position"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"position")) ::
              Data.ProtoLens.FieldDescriptor StatisticsResp'Details
        progress__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "progress"
              (Data.ProtoLens.ScalarField Data.ProtoLens.FloatField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Float)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"progress")) ::
              Data.ProtoLens.FieldDescriptor StatisticsResp'Details
        lastCheckpoint__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "lastCheckpoint"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"lastCheckpoint")) ::
              Data.ProtoLens.FieldDescriptor StatisticsResp'Details
        eventsProcessedAfterRestart__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "eventsProcessedAfterRestart"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"eventsProcessedAfterRestart")) ::
              Data.ProtoLens.FieldDescriptor StatisticsResp'Details
        checkpointStatus__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "checkpointStatus"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"checkpointStatus")) ::
              Data.ProtoLens.FieldDescriptor StatisticsResp'Details
        bufferedEvents__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "bufferedEvents"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"bufferedEvents")) ::
              Data.ProtoLens.FieldDescriptor StatisticsResp'Details
        writePendingEventsBeforeCheckpoint__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "writePendingEventsBeforeCheckpoint"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field
                    @"writePendingEventsBeforeCheckpoint")) ::
              Data.ProtoLens.FieldDescriptor StatisticsResp'Details
        writePendingEventsAfterCheckpoint__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "writePendingEventsAfterCheckpoint"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field
                    @"writePendingEventsAfterCheckpoint")) ::
              Data.ProtoLens.FieldDescriptor StatisticsResp'Details
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, coreProcessingTime__field_descriptor),
           (Data.ProtoLens.Tag 2, version__field_descriptor),
           (Data.ProtoLens.Tag 3, epoch__field_descriptor),
           (Data.ProtoLens.Tag 4, effectiveName__field_descriptor),
           (Data.ProtoLens.Tag 5, writesInProgress__field_descriptor),
           (Data.ProtoLens.Tag 6, readsInProgress__field_descriptor),
           (Data.ProtoLens.Tag 7, partitionsCached__field_descriptor),
           (Data.ProtoLens.Tag 8, status__field_descriptor),
           (Data.ProtoLens.Tag 9, stateReason__field_descriptor),
           (Data.ProtoLens.Tag 10, name__field_descriptor),
           (Data.ProtoLens.Tag 11, mode__field_descriptor),
           (Data.ProtoLens.Tag 12, position__field_descriptor),
           (Data.ProtoLens.Tag 13, progress__field_descriptor),
           (Data.ProtoLens.Tag 14, lastCheckpoint__field_descriptor),
           (Data.ProtoLens.Tag 15, 
            eventsProcessedAfterRestart__field_descriptor),
           (Data.ProtoLens.Tag 16, checkpointStatus__field_descriptor),
           (Data.ProtoLens.Tag 17, bufferedEvents__field_descriptor),
           (Data.ProtoLens.Tag 18, 
            writePendingEventsBeforeCheckpoint__field_descriptor),
           (Data.ProtoLens.Tag 19, 
            writePendingEventsAfterCheckpoint__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _StatisticsResp'Details'_unknownFields
        (\ x__ y__ -> x__ {_StatisticsResp'Details'_unknownFields = y__})
  defMessage
    = StatisticsResp'Details'_constructor
        {_StatisticsResp'Details'coreProcessingTime = Data.ProtoLens.fieldDefault,
         _StatisticsResp'Details'version = Data.ProtoLens.fieldDefault,
         _StatisticsResp'Details'epoch = Data.ProtoLens.fieldDefault,
         _StatisticsResp'Details'effectiveName = Data.ProtoLens.fieldDefault,
         _StatisticsResp'Details'writesInProgress = Data.ProtoLens.fieldDefault,
         _StatisticsResp'Details'readsInProgress = Data.ProtoLens.fieldDefault,
         _StatisticsResp'Details'partitionsCached = Data.ProtoLens.fieldDefault,
         _StatisticsResp'Details'status = Data.ProtoLens.fieldDefault,
         _StatisticsResp'Details'stateReason = Data.ProtoLens.fieldDefault,
         _StatisticsResp'Details'name = Data.ProtoLens.fieldDefault,
         _StatisticsResp'Details'mode = Data.ProtoLens.fieldDefault,
         _StatisticsResp'Details'position = Data.ProtoLens.fieldDefault,
         _StatisticsResp'Details'progress = Data.ProtoLens.fieldDefault,
         _StatisticsResp'Details'lastCheckpoint = Data.ProtoLens.fieldDefault,
         _StatisticsResp'Details'eventsProcessedAfterRestart = Data.ProtoLens.fieldDefault,
         _StatisticsResp'Details'checkpointStatus = Data.ProtoLens.fieldDefault,
         _StatisticsResp'Details'bufferedEvents = Data.ProtoLens.fieldDefault,
         _StatisticsResp'Details'writePendingEventsBeforeCheckpoint = Data.ProtoLens.fieldDefault,
         _StatisticsResp'Details'writePendingEventsAfterCheckpoint = Data.ProtoLens.fieldDefault,
         _StatisticsResp'Details'_unknownFields = []}
  parseMessage
    = let
        loop ::
          StatisticsResp'Details
          -> Data.ProtoLens.Encoding.Bytes.Parser StatisticsResp'Details
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
                                       "coreProcessingTime"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"coreProcessingTime") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "version"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"version") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "epoch"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"epoch") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "effectiveName"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"effectiveName") y x)
                        40
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "writesInProgress"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"writesInProgress") y x)
                        48
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "readsInProgress"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"readsInProgress") y x)
                        56
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "partitionsCached"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"partitionsCached") y x)
                        66
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "status"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"status") y x)
                        74
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "stateReason"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"stateReason") y x)
                        82
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        90
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "mode"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"mode") y x)
                        98
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "position"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"position") y x)
                        109
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToFloat
                                          Data.ProtoLens.Encoding.Bytes.getFixed32)
                                       "progress"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"progress") y x)
                        114
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "lastCheckpoint"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"lastCheckpoint") y x)
                        120
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "eventsProcessedAfterRestart"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"eventsProcessedAfterRestart")
                                     y
                                     x)
                        130
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "checkpointStatus"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"checkpointStatus") y x)
                        136
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "bufferedEvents"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"bufferedEvents") y x)
                        144
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "writePendingEventsBeforeCheckpoint"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field
                                        @"writePendingEventsBeforeCheckpoint")
                                     y
                                     x)
                        152
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "writePendingEventsAfterCheckpoint"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field
                                        @"writePendingEventsAfterCheckpoint")
                                     y
                                     x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Details"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view
                      (Data.ProtoLens.Field.field @"coreProcessingTime") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"version") _x
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
                      _v = Lens.Family2.view (Data.ProtoLens.Field.field @"epoch") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            ((Prelude..)
                               Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                   ((Data.Monoid.<>)
                      (let
                         _v
                           = Lens.Family2.view
                               (Data.ProtoLens.Field.field @"effectiveName") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                               ((Prelude..)
                                  (\ bs
                                     -> (Data.Monoid.<>)
                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                             (Prelude.fromIntegral (Data.ByteString.length bs)))
                                          (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                  Data.Text.Encoding.encodeUtf8
                                  _v))
                      ((Data.Monoid.<>)
                         (let
                            _v
                              = Lens.Family2.view
                                  (Data.ProtoLens.Field.field @"writesInProgress") _x
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
                                 = Lens.Family2.view
                                     (Data.ProtoLens.Field.field @"readsInProgress") _x
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
                                        (Data.ProtoLens.Field.field @"partitionsCached") _x
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
                                  (let
                                     _v
                                       = Lens.Family2.view (Data.ProtoLens.Field.field @"status") _x
                                   in
                                     if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                         Data.Monoid.mempty
                                     else
                                         (Data.Monoid.<>)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt 66)
                                           ((Prelude..)
                                              (\ bs
                                                 -> (Data.Monoid.<>)
                                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                         (Prelude.fromIntegral
                                                            (Data.ByteString.length bs)))
                                                      (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                              Data.Text.Encoding.encodeUtf8
                                              _v))
                                  ((Data.Monoid.<>)
                                     (let
                                        _v
                                          = Lens.Family2.view
                                              (Data.ProtoLens.Field.field @"stateReason") _x
                                      in
                                        if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                            Data.Monoid.mempty
                                        else
                                            (Data.Monoid.<>)
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt 74)
                                              ((Prelude..)
                                                 (\ bs
                                                    -> (Data.Monoid.<>)
                                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                            (Prelude.fromIntegral
                                                               (Data.ByteString.length bs)))
                                                         (Data.ProtoLens.Encoding.Bytes.putBytes
                                                            bs))
                                                 Data.Text.Encoding.encodeUtf8
                                                 _v))
                                     ((Data.Monoid.<>)
                                        (let
                                           _v
                                             = Lens.Family2.view
                                                 (Data.ProtoLens.Field.field @"name") _x
                                         in
                                           if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                               Data.Monoid.mempty
                                           else
                                               (Data.Monoid.<>)
                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 82)
                                                 ((Prelude..)
                                                    (\ bs
                                                       -> (Data.Monoid.<>)
                                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                               (Prelude.fromIntegral
                                                                  (Data.ByteString.length bs)))
                                                            (Data.ProtoLens.Encoding.Bytes.putBytes
                                                               bs))
                                                    Data.Text.Encoding.encodeUtf8
                                                    _v))
                                        ((Data.Monoid.<>)
                                           (let
                                              _v
                                                = Lens.Family2.view
                                                    (Data.ProtoLens.Field.field @"mode") _x
                                            in
                                              if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                                  Data.Monoid.mempty
                                              else
                                                  (Data.Monoid.<>)
                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt 90)
                                                    ((Prelude..)
                                                       (\ bs
                                                          -> (Data.Monoid.<>)
                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                  (Prelude.fromIntegral
                                                                     (Data.ByteString.length bs)))
                                                               (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                  bs))
                                                       Data.Text.Encoding.encodeUtf8
                                                       _v))
                                           ((Data.Monoid.<>)
                                              (let
                                                 _v
                                                   = Lens.Family2.view
                                                       (Data.ProtoLens.Field.field @"position") _x
                                               in
                                                 if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                                     Data.Monoid.mempty
                                                 else
                                                     (Data.Monoid.<>)
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
                                                          Data.Text.Encoding.encodeUtf8
                                                          _v))
                                              ((Data.Monoid.<>)
                                                 (let
                                                    _v
                                                      = Lens.Family2.view
                                                          (Data.ProtoLens.Field.field @"progress")
                                                          _x
                                                  in
                                                    if (Prelude.==)
                                                         _v Data.ProtoLens.fieldDefault then
                                                        Data.Monoid.mempty
                                                    else
                                                        (Data.Monoid.<>)
                                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                             109)
                                                          ((Prelude..)
                                                             Data.ProtoLens.Encoding.Bytes.putFixed32
                                                             Data.ProtoLens.Encoding.Bytes.floatToWord
                                                             _v))
                                                 ((Data.Monoid.<>)
                                                    (let
                                                       _v
                                                         = Lens.Family2.view
                                                             (Data.ProtoLens.Field.field
                                                                @"lastCheckpoint")
                                                             _x
                                                     in
                                                       if (Prelude.==)
                                                            _v Data.ProtoLens.fieldDefault then
                                                           Data.Monoid.mempty
                                                       else
                                                           (Data.Monoid.<>)
                                                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                114)
                                                             ((Prelude..)
                                                                (\ bs
                                                                   -> (Data.Monoid.<>)
                                                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                           (Prelude.fromIntegral
                                                                              (Data.ByteString.length
                                                                                 bs)))
                                                                        (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                           bs))
                                                                Data.Text.Encoding.encodeUtf8
                                                                _v))
                                                    ((Data.Monoid.<>)
                                                       (let
                                                          _v
                                                            = Lens.Family2.view
                                                                (Data.ProtoLens.Field.field
                                                                   @"eventsProcessedAfterRestart")
                                                                _x
                                                        in
                                                          if (Prelude.==)
                                                               _v Data.ProtoLens.fieldDefault then
                                                              Data.Monoid.mempty
                                                          else
                                                              (Data.Monoid.<>)
                                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                   120)
                                                                ((Prelude..)
                                                                   Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                   Prelude.fromIntegral
                                                                   _v))
                                                       ((Data.Monoid.<>)
                                                          (let
                                                             _v
                                                               = Lens.Family2.view
                                                                   (Data.ProtoLens.Field.field
                                                                      @"checkpointStatus")
                                                                   _x
                                                           in
                                                             if (Prelude.==)
                                                                  _v
                                                                  Data.ProtoLens.fieldDefault then
                                                                 Data.Monoid.mempty
                                                             else
                                                                 (Data.Monoid.<>)
                                                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                      130)
                                                                   ((Prelude..)
                                                                      (\ bs
                                                                         -> (Data.Monoid.<>)
                                                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                 (Prelude.fromIntegral
                                                                                    (Data.ByteString.length
                                                                                       bs)))
                                                                              (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                 bs))
                                                                      Data.Text.Encoding.encodeUtf8
                                                                      _v))
                                                          ((Data.Monoid.<>)
                                                             (let
                                                                _v
                                                                  = Lens.Family2.view
                                                                      (Data.ProtoLens.Field.field
                                                                         @"bufferedEvents")
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
                                                                         Prelude.fromIntegral
                                                                         _v))
                                                             ((Data.Monoid.<>)
                                                                (let
                                                                   _v
                                                                     = Lens.Family2.view
                                                                         (Data.ProtoLens.Field.field
                                                                            @"writePendingEventsBeforeCheckpoint")
                                                                         _x
                                                                 in
                                                                   if (Prelude.==)
                                                                        _v
                                                                        Data.ProtoLens.fieldDefault then
                                                                       Data.Monoid.mempty
                                                                   else
                                                                       (Data.Monoid.<>)
                                                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                            144)
                                                                         ((Prelude..)
                                                                            Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                            Prelude.fromIntegral
                                                                            _v))
                                                                ((Data.Monoid.<>)
                                                                   (let
                                                                      _v
                                                                        = Lens.Family2.view
                                                                            (Data.ProtoLens.Field.field
                                                                               @"writePendingEventsAfterCheckpoint")
                                                                            _x
                                                                    in
                                                                      if (Prelude.==)
                                                                           _v
                                                                           Data.ProtoLens.fieldDefault then
                                                                          Data.Monoid.mempty
                                                                      else
                                                                          (Data.Monoid.<>)
                                                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                               152)
                                                                            ((Prelude..)
                                                                               Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                               Prelude.fromIntegral
                                                                               _v))
                                                                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                                                                      (Lens.Family2.view
                                                                         Data.ProtoLens.unknownFields
                                                                         _x))))))))))))))))))))
instance Control.DeepSeq.NFData StatisticsResp'Details where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_StatisticsResp'Details'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_StatisticsResp'Details'coreProcessingTime x__)
                (Control.DeepSeq.deepseq
                   (_StatisticsResp'Details'version x__)
                   (Control.DeepSeq.deepseq
                      (_StatisticsResp'Details'epoch x__)
                      (Control.DeepSeq.deepseq
                         (_StatisticsResp'Details'effectiveName x__)
                         (Control.DeepSeq.deepseq
                            (_StatisticsResp'Details'writesInProgress x__)
                            (Control.DeepSeq.deepseq
                               (_StatisticsResp'Details'readsInProgress x__)
                               (Control.DeepSeq.deepseq
                                  (_StatisticsResp'Details'partitionsCached x__)
                                  (Control.DeepSeq.deepseq
                                     (_StatisticsResp'Details'status x__)
                                     (Control.DeepSeq.deepseq
                                        (_StatisticsResp'Details'stateReason x__)
                                        (Control.DeepSeq.deepseq
                                           (_StatisticsResp'Details'name x__)
                                           (Control.DeepSeq.deepseq
                                              (_StatisticsResp'Details'mode x__)
                                              (Control.DeepSeq.deepseq
                                                 (_StatisticsResp'Details'position x__)
                                                 (Control.DeepSeq.deepseq
                                                    (_StatisticsResp'Details'progress x__)
                                                    (Control.DeepSeq.deepseq
                                                       (_StatisticsResp'Details'lastCheckpoint x__)
                                                       (Control.DeepSeq.deepseq
                                                          (_StatisticsResp'Details'eventsProcessedAfterRestart
                                                             x__)
                                                          (Control.DeepSeq.deepseq
                                                             (_StatisticsResp'Details'checkpointStatus
                                                                x__)
                                                             (Control.DeepSeq.deepseq
                                                                (_StatisticsResp'Details'bufferedEvents
                                                                   x__)
                                                                (Control.DeepSeq.deepseq
                                                                   (_StatisticsResp'Details'writePendingEventsBeforeCheckpoint
                                                                      x__)
                                                                   (Control.DeepSeq.deepseq
                                                                      (_StatisticsResp'Details'writePendingEventsAfterCheckpoint
                                                                         x__)
                                                                      ())))))))))))))))))))
{- | Fields :
     
         * 'Proto.Projections_Fields.options' @:: Lens' UpdateReq UpdateReq'Options@
         * 'Proto.Projections_Fields.maybe'options' @:: Lens' UpdateReq (Prelude.Maybe UpdateReq'Options)@ -}
data UpdateReq
  = UpdateReq'_constructor {_UpdateReq'options :: !(Prelude.Maybe UpdateReq'Options),
                            _UpdateReq'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show UpdateReq where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField UpdateReq "options" UpdateReq'Options where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UpdateReq'options (\ x__ y__ -> x__ {_UpdateReq'options = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField UpdateReq "maybe'options" (Prelude.Maybe UpdateReq'Options) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UpdateReq'options (\ x__ y__ -> x__ {_UpdateReq'options = y__}))
        Prelude.id
instance Data.ProtoLens.Message UpdateReq where
  messageName _
    = Data.Text.pack "event_store.client.projections.UpdateReq"
  packedMessageDescriptor _
    = "\n\
      \\tUpdateReq\DC2K\n\
      \\aoptions\CAN\SOH \SOH(\v21.event_store.client.projections.UpdateReq.OptionsR\aoptions\SUB\179\SOH\n\
      \\aOptions\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2\DC4\n\
      \\ENQquery\CAN\STX \SOH(\tR\ENQquery\DC2#\n\
      \\femit_enabled\CAN\ETX \SOH(\bH\NULR\vemitEnabled\DC2J\n\
      \\SIno_emit_options\CAN\EOT \SOH(\v2 .event_store.client.shared.EmptyH\NULR\rnoEmitOptionsB\r\n\
      \\vemit_option"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        options__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "options"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor UpdateReq'Options)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'options")) ::
              Data.ProtoLens.FieldDescriptor UpdateReq
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, options__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _UpdateReq'_unknownFields
        (\ x__ y__ -> x__ {_UpdateReq'_unknownFields = y__})
  defMessage
    = UpdateReq'_constructor
        {_UpdateReq'options = Prelude.Nothing,
         _UpdateReq'_unknownFields = []}
  parseMessage
    = let
        loop :: UpdateReq -> Data.ProtoLens.Encoding.Bytes.Parser UpdateReq
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
                                       "options"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"options") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "UpdateReq"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'options") _x
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
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData UpdateReq where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_UpdateReq'_unknownFields x__)
             (Control.DeepSeq.deepseq (_UpdateReq'options x__) ())
{- | Fields :
     
         * 'Proto.Projections_Fields.name' @:: Lens' UpdateReq'Options Data.Text.Text@
         * 'Proto.Projections_Fields.query' @:: Lens' UpdateReq'Options Data.Text.Text@
         * 'Proto.Projections_Fields.maybe'emitOption' @:: Lens' UpdateReq'Options (Prelude.Maybe UpdateReq'Options'EmitOption)@
         * 'Proto.Projections_Fields.maybe'emitEnabled' @:: Lens' UpdateReq'Options (Prelude.Maybe Prelude.Bool)@
         * 'Proto.Projections_Fields.emitEnabled' @:: Lens' UpdateReq'Options Prelude.Bool@
         * 'Proto.Projections_Fields.maybe'noEmitOptions' @:: Lens' UpdateReq'Options (Prelude.Maybe Proto.Shared.Empty)@
         * 'Proto.Projections_Fields.noEmitOptions' @:: Lens' UpdateReq'Options Proto.Shared.Empty@ -}
data UpdateReq'Options
  = UpdateReq'Options'_constructor {_UpdateReq'Options'name :: !Data.Text.Text,
                                    _UpdateReq'Options'query :: !Data.Text.Text,
                                    _UpdateReq'Options'emitOption :: !(Prelude.Maybe UpdateReq'Options'EmitOption),
                                    _UpdateReq'Options'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show UpdateReq'Options where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data UpdateReq'Options'EmitOption
  = UpdateReq'Options'EmitEnabled !Prelude.Bool |
    UpdateReq'Options'NoEmitOptions !Proto.Shared.Empty
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField UpdateReq'Options "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UpdateReq'Options'name
           (\ x__ y__ -> x__ {_UpdateReq'Options'name = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField UpdateReq'Options "query" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UpdateReq'Options'query
           (\ x__ y__ -> x__ {_UpdateReq'Options'query = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField UpdateReq'Options "maybe'emitOption" (Prelude.Maybe UpdateReq'Options'EmitOption) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UpdateReq'Options'emitOption
           (\ x__ y__ -> x__ {_UpdateReq'Options'emitOption = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField UpdateReq'Options "maybe'emitEnabled" (Prelude.Maybe Prelude.Bool) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UpdateReq'Options'emitOption
           (\ x__ y__ -> x__ {_UpdateReq'Options'emitOption = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (UpdateReq'Options'EmitEnabled x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap UpdateReq'Options'EmitEnabled y__))
instance Data.ProtoLens.Field.HasField UpdateReq'Options "emitEnabled" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UpdateReq'Options'emitOption
           (\ x__ y__ -> x__ {_UpdateReq'Options'emitOption = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (UpdateReq'Options'EmitEnabled x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap UpdateReq'Options'EmitEnabled y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField UpdateReq'Options "maybe'noEmitOptions" (Prelude.Maybe Proto.Shared.Empty) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UpdateReq'Options'emitOption
           (\ x__ y__ -> x__ {_UpdateReq'Options'emitOption = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (UpdateReq'Options'NoEmitOptions x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap UpdateReq'Options'NoEmitOptions y__))
instance Data.ProtoLens.Field.HasField UpdateReq'Options "noEmitOptions" Proto.Shared.Empty where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UpdateReq'Options'emitOption
           (\ x__ y__ -> x__ {_UpdateReq'Options'emitOption = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (UpdateReq'Options'NoEmitOptions x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap UpdateReq'Options'NoEmitOptions y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message UpdateReq'Options where
  messageName _
    = Data.Text.pack "event_store.client.projections.UpdateReq.Options"
  packedMessageDescriptor _
    = "\n\
      \\aOptions\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2\DC4\n\
      \\ENQquery\CAN\STX \SOH(\tR\ENQquery\DC2#\n\
      \\femit_enabled\CAN\ETX \SOH(\bH\NULR\vemitEnabled\DC2J\n\
      \\SIno_emit_options\CAN\EOT \SOH(\v2 .event_store.client.shared.EmptyH\NULR\rnoEmitOptionsB\r\n\
      \\vemit_option"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor UpdateReq'Options
        query__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "query"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"query")) ::
              Data.ProtoLens.FieldDescriptor UpdateReq'Options
        emitEnabled__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "emit_enabled"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'emitEnabled")) ::
              Data.ProtoLens.FieldDescriptor UpdateReq'Options
        noEmitOptions__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "no_emit_options"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Shared.Empty)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'noEmitOptions")) ::
              Data.ProtoLens.FieldDescriptor UpdateReq'Options
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, name__field_descriptor),
           (Data.ProtoLens.Tag 2, query__field_descriptor),
           (Data.ProtoLens.Tag 3, emitEnabled__field_descriptor),
           (Data.ProtoLens.Tag 4, noEmitOptions__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _UpdateReq'Options'_unknownFields
        (\ x__ y__ -> x__ {_UpdateReq'Options'_unknownFields = y__})
  defMessage
    = UpdateReq'Options'_constructor
        {_UpdateReq'Options'name = Data.ProtoLens.fieldDefault,
         _UpdateReq'Options'query = Data.ProtoLens.fieldDefault,
         _UpdateReq'Options'emitOption = Prelude.Nothing,
         _UpdateReq'Options'_unknownFields = []}
  parseMessage
    = let
        loop ::
          UpdateReq'Options
          -> Data.ProtoLens.Encoding.Bytes.Parser UpdateReq'Options
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
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "query"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"query") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "emit_enabled"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"emitEnabled") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "no_emit_options"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"noEmitOptions") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Options"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
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
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"query") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8
                            _v))
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view
                          (Data.ProtoLens.Field.field @"maybe'emitOption") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just (UpdateReq'Options'EmitEnabled v))
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                             ((Prelude..)
                                Data.ProtoLens.Encoding.Bytes.putVarInt
                                (\ b -> if b then 1 else 0)
                                v)
                      (Prelude.Just (UpdateReq'Options'NoEmitOptions v))
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage
                                v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData UpdateReq'Options where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_UpdateReq'Options'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_UpdateReq'Options'name x__)
                (Control.DeepSeq.deepseq
                   (_UpdateReq'Options'query x__)
                   (Control.DeepSeq.deepseq (_UpdateReq'Options'emitOption x__) ())))
instance Control.DeepSeq.NFData UpdateReq'Options'EmitOption where
  rnf (UpdateReq'Options'EmitEnabled x__) = Control.DeepSeq.rnf x__
  rnf (UpdateReq'Options'NoEmitOptions x__) = Control.DeepSeq.rnf x__
_UpdateReq'Options'EmitEnabled ::
  Data.ProtoLens.Prism.Prism' UpdateReq'Options'EmitOption Prelude.Bool
_UpdateReq'Options'EmitEnabled
  = Data.ProtoLens.Prism.prism'
      UpdateReq'Options'EmitEnabled
      (\ p__
         -> case p__ of
              (UpdateReq'Options'EmitEnabled p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_UpdateReq'Options'NoEmitOptions ::
  Data.ProtoLens.Prism.Prism' UpdateReq'Options'EmitOption Proto.Shared.Empty
_UpdateReq'Options'NoEmitOptions
  = Data.ProtoLens.Prism.prism'
      UpdateReq'Options'NoEmitOptions
      (\ p__
         -> case p__ of
              (UpdateReq'Options'NoEmitOptions p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
      -}
data UpdateResp
  = UpdateResp'_constructor {_UpdateResp'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show UpdateResp where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message UpdateResp where
  messageName _
    = Data.Text.pack "event_store.client.projections.UpdateResp"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \UpdateResp"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _UpdateResp'_unknownFields
        (\ x__ y__ -> x__ {_UpdateResp'_unknownFields = y__})
  defMessage
    = UpdateResp'_constructor {_UpdateResp'_unknownFields = []}
  parseMessage
    = let
        loop ::
          UpdateResp -> Data.ProtoLens.Encoding.Bytes.Parser UpdateResp
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
                      case tag of {
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x) }
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "UpdateResp"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData UpdateResp where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq (_UpdateResp'_unknownFields x__) ()
data Projections = Projections {}
instance Data.ProtoLens.Service.Types.Service Projections where
  type ServiceName Projections = "Projections"
  type ServicePackage Projections = "event_store.client.projections"
  type ServiceMethods Projections = '["create",
                                      "delete",
                                      "disable",
                                      "enable",
                                      "reset",
                                      "restartSubsystem",
                                      "result",
                                      "state",
                                      "statistics",
                                      "update"]
instance Data.ProtoLens.Service.Types.HasMethodImpl Projections "create" where
  type MethodName Projections "create" = "Create"
  type MethodInput Projections "create" = CreateReq
  type MethodOutput Projections "create" = CreateResp
  type MethodStreamingType Projections "create" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Projections "update" where
  type MethodName Projections "update" = "Update"
  type MethodInput Projections "update" = UpdateReq
  type MethodOutput Projections "update" = UpdateResp
  type MethodStreamingType Projections "update" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Projections "delete" where
  type MethodName Projections "delete" = "Delete"
  type MethodInput Projections "delete" = DeleteReq
  type MethodOutput Projections "delete" = DeleteResp
  type MethodStreamingType Projections "delete" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Projections "statistics" where
  type MethodName Projections "statistics" = "Statistics"
  type MethodInput Projections "statistics" = StatisticsReq
  type MethodOutput Projections "statistics" = StatisticsResp
  type MethodStreamingType Projections "statistics" = 'Data.ProtoLens.Service.Types.ServerStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Projections "disable" where
  type MethodName Projections "disable" = "Disable"
  type MethodInput Projections "disable" = DisableReq
  type MethodOutput Projections "disable" = DisableResp
  type MethodStreamingType Projections "disable" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Projections "enable" where
  type MethodName Projections "enable" = "Enable"
  type MethodInput Projections "enable" = EnableReq
  type MethodOutput Projections "enable" = EnableResp
  type MethodStreamingType Projections "enable" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Projections "reset" where
  type MethodName Projections "reset" = "Reset"
  type MethodInput Projections "reset" = ResetReq
  type MethodOutput Projections "reset" = ResetResp
  type MethodStreamingType Projections "reset" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Projections "state" where
  type MethodName Projections "state" = "State"
  type MethodInput Projections "state" = StateReq
  type MethodOutput Projections "state" = StateResp
  type MethodStreamingType Projections "state" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Projections "result" where
  type MethodName Projections "result" = "Result"
  type MethodInput Projections "result" = ResultReq
  type MethodOutput Projections "result" = ResultResp
  type MethodStreamingType Projections "result" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Projections "restartSubsystem" where
  type MethodName Projections "restartSubsystem" = "RestartSubsystem"
  type MethodInput Projections "restartSubsystem" = Proto.Shared.Empty
  type MethodOutput Projections "restartSubsystem" = Proto.Shared.Empty
  type MethodStreamingType Projections "restartSubsystem" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\DC1projections.proto\DC2\RSevent_store.client.projections\SUB\FSgoogle/protobuf/struct.proto\SUB\fshared.proto\"\245\ETX\n\
    \\tCreateReq\DC2K\n\
    \\aoptions\CAN\SOH \SOH(\v21.event_store.client.projections.CreateReq.OptionsR\aoptions\SUB\154\ETX\n\
    \\aOptions\DC2=\n\
    \\bone_time\CAN\SOH \SOH(\v2 .event_store.client.shared.EmptyH\NULR\aoneTime\DC2[\n\
    \\ttransient\CAN\STX \SOH(\v2;.event_store.client.projections.CreateReq.Options.TransientH\NULR\ttransient\DC2^\n\
    \\n\
    \continuous\CAN\ETX \SOH(\v2<.event_store.client.projections.CreateReq.Options.ContinuousH\NULR\n\
    \continuous\DC2\DC4\n\
    \\ENQquery\CAN\EOT \SOH(\tR\ENQquery\SUB\US\n\
    \\tTransient\DC2\DC2\n\
    \\EOTname\CAN\SOH \SOH(\tR\EOTname\SUBT\n\
    \\n\
    \Continuous\DC2\DC2\n\
    \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC22\n\
    \\NAKtrack_emitted_streams\CAN\STX \SOH(\bR\DC3trackEmittedStreamsB\ACK\n\
    \\EOTmode\"\f\n\
    \\n\
    \CreateResp\"\142\STX\n\
    \\tUpdateReq\DC2K\n\
    \\aoptions\CAN\SOH \SOH(\v21.event_store.client.projections.UpdateReq.OptionsR\aoptions\SUB\179\SOH\n\
    \\aOptions\DC2\DC2\n\
    \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2\DC4\n\
    \\ENQquery\CAN\STX \SOH(\tR\ENQquery\DC2#\n\
    \\femit_enabled\CAN\ETX \SOH(\bH\NULR\vemitEnabled\DC2J\n\
    \\SIno_emit_options\CAN\EOT \SOH(\v2 .event_store.client.shared.EmptyH\NULR\rnoEmitOptionsB\r\n\
    \\vemit_option\"\f\n\
    \\n\
    \UpdateResp\"\152\STX\n\
    \\tDeleteReq\DC2K\n\
    \\aoptions\CAN\SOH \SOH(\v21.event_store.client.projections.DeleteReq.OptionsR\aoptions\SUB\189\SOH\n\
    \\aOptions\DC2\DC2\n\
    \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC24\n\
    \\SYNdelete_emitted_streams\CAN\STX \SOH(\bR\DC4deleteEmittedStreams\DC2.\n\
    \\DC3delete_state_stream\CAN\ETX \SOH(\bR\DC1deleteStateStream\DC28\n\
    \\CANdelete_checkpoint_stream\CAN\EOT \SOH(\bR\SYNdeleteCheckpointStream\"\f\n\
    \\n\
    \DeleteResp\"\133\ETX\n\
    \\rStatisticsReq\DC2O\n\
    \\aoptions\CAN\SOH \SOH(\v25.event_store.client.projections.StatisticsReq.OptionsR\aoptions\SUB\162\STX\n\
    \\aOptions\DC2\DC4\n\
    \\EOTname\CAN\SOH \SOH(\tH\NULR\EOTname\DC24\n\
    \\ETXall\CAN\STX \SOH(\v2 .event_store.client.shared.EmptyH\NULR\ETXall\DC2@\n\
    \\ttransient\CAN\ETX \SOH(\v2 .event_store.client.shared.EmptyH\NULR\ttransient\DC2B\n\
    \\n\
    \continuous\CAN\EOT \SOH(\v2 .event_store.client.shared.EmptyH\NULR\n\
    \continuous\DC2=\n\
    \\bone_time\CAN\ENQ \SOH(\v2 .event_store.client.shared.EmptyH\NULR\aoneTimeB\ACK\n\
    \\EOTmode\"\236\ACK\n\
    \\SOStatisticsResp\DC2P\n\
    \\adetails\CAN\SOH \SOH(\v26.event_store.client.projections.StatisticsResp.DetailsR\adetails\SUB\135\ACK\n\
    \\aDetails\DC2.\n\
    \\DC2coreProcessingTime\CAN\SOH \SOH(\ETXR\DC2coreProcessingTime\DC2\CAN\n\
    \\aversion\CAN\STX \SOH(\ETXR\aversion\DC2\DC4\n\
    \\ENQepoch\CAN\ETX \SOH(\ETXR\ENQepoch\DC2$\n\
    \\reffectiveName\CAN\EOT \SOH(\tR\reffectiveName\DC2*\n\
    \\DLEwritesInProgress\CAN\ENQ \SOH(\ENQR\DLEwritesInProgress\DC2(\n\
    \\SIreadsInProgress\CAN\ACK \SOH(\ENQR\SIreadsInProgress\DC2*\n\
    \\DLEpartitionsCached\CAN\a \SOH(\ENQR\DLEpartitionsCached\DC2\SYN\n\
    \\ACKstatus\CAN\b \SOH(\tR\ACKstatus\DC2 \n\
    \\vstateReason\CAN\t \SOH(\tR\vstateReason\DC2\DC2\n\
    \\EOTname\CAN\n\
    \ \SOH(\tR\EOTname\DC2\DC2\n\
    \\EOTmode\CAN\v \SOH(\tR\EOTmode\DC2\SUB\n\
    \\bposition\CAN\f \SOH(\tR\bposition\DC2\SUB\n\
    \\bprogress\CAN\r \SOH(\STXR\bprogress\DC2&\n\
    \\SOlastCheckpoint\CAN\SO \SOH(\tR\SOlastCheckpoint\DC2@\n\
    \\ESCeventsProcessedAfterRestart\CAN\SI \SOH(\ETXR\ESCeventsProcessedAfterRestart\DC2*\n\
    \\DLEcheckpointStatus\CAN\DLE \SOH(\tR\DLEcheckpointStatus\DC2&\n\
    \\SObufferedEvents\CAN\DC1 \SOH(\ETXR\SObufferedEvents\DC2N\n\
    \\"writePendingEventsBeforeCheckpoint\CAN\DC2 \SOH(\ENQR\"writePendingEventsBeforeCheckpoint\DC2L\n\
    \!writePendingEventsAfterCheckpoint\CAN\DC3 \SOH(\ENQR!writePendingEventsAfterCheckpoint\"\147\SOH\n\
    \\bStateReq\DC2J\n\
    \\aoptions\CAN\SOH \SOH(\v20.event_store.client.projections.StateReq.OptionsR\aoptions\SUB;\n\
    \\aOptions\DC2\DC2\n\
    \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2\FS\n\
    \\tpartition\CAN\STX \SOH(\tR\tpartition\"9\n\
    \\tStateResp\DC2,\n\
    \\ENQstate\CAN\SOH \SOH(\v2\SYN.google.protobuf.ValueR\ENQstate\"\149\SOH\n\
    \\tResultReq\DC2K\n\
    \\aoptions\CAN\SOH \SOH(\v21.event_store.client.projections.ResultReq.OptionsR\aoptions\SUB;\n\
    \\aOptions\DC2\DC2\n\
    \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2\FS\n\
    \\tpartition\CAN\STX \SOH(\tR\tpartition\"<\n\
    \\n\
    \ResultResp\DC2.\n\
    \\ACKresult\CAN\SOH \SOH(\v2\SYN.google.protobuf.ValueR\ACKresult\"\160\SOH\n\
    \\bResetReq\DC2J\n\
    \\aoptions\CAN\SOH \SOH(\v20.event_store.client.projections.ResetReq.OptionsR\aoptions\SUBH\n\
    \\aOptions\DC2\DC2\n\
    \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2)\n\
    \\DLEwrite_checkpoint\CAN\STX \SOH(\bR\SIwriteCheckpoint\"\v\n\
    \\tResetResp\"w\n\
    \\tEnableReq\DC2K\n\
    \\aoptions\CAN\SOH \SOH(\v21.event_store.client.projections.EnableReq.OptionsR\aoptions\SUB\GS\n\
    \\aOptions\DC2\DC2\n\
    \\EOTname\CAN\SOH \SOH(\tR\EOTname\"\f\n\
    \\n\
    \EnableResp\"\164\SOH\n\
    \\n\
    \DisableReq\DC2L\n\
    \\aoptions\CAN\SOH \SOH(\v22.event_store.client.projections.DisableReq.OptionsR\aoptions\SUBH\n\
    \\aOptions\DC2\DC2\n\
    \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2)\n\
    \\DLEwrite_checkpoint\CAN\STX \SOH(\bR\SIwriteCheckpoint\"\r\n\
    \\vDisableResp2\217\a\n\
    \\vProjections\DC2_\n\
    \\ACKCreate\DC2).event_store.client.projections.CreateReq\SUB*.event_store.client.projections.CreateResp\DC2_\n\
    \\ACKUpdate\DC2).event_store.client.projections.UpdateReq\SUB*.event_store.client.projections.UpdateResp\DC2_\n\
    \\ACKDelete\DC2).event_store.client.projections.DeleteReq\SUB*.event_store.client.projections.DeleteResp\DC2m\n\
    \\n\
    \Statistics\DC2-.event_store.client.projections.StatisticsReq\SUB..event_store.client.projections.StatisticsResp0\SOH\DC2b\n\
    \\aDisable\DC2*.event_store.client.projections.DisableReq\SUB+.event_store.client.projections.DisableResp\DC2_\n\
    \\ACKEnable\DC2).event_store.client.projections.EnableReq\SUB*.event_store.client.projections.EnableResp\DC2\\\n\
    \\ENQReset\DC2(.event_store.client.projections.ResetReq\SUB).event_store.client.projections.ResetResp\DC2\\\n\
    \\ENQState\DC2(.event_store.client.projections.StateReq\SUB).event_store.client.projections.StateResp\DC2_\n\
    \\ACKResult\DC2).event_store.client.projections.ResultReq\SUB*.event_store.client.projections.ResultResp\DC2V\n\
    \\DLERestartSubsystem\DC2 .event_store.client.shared.Empty\SUB .event_store.client.shared.EmptyB+\n\
    \)com.eventstore.dbclient.proto.projectionsJ\198)\n\
    \\a\DC2\ENQ\NUL\NUL\173\SOH\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\SOH\NUL'\n\
    \\b\n\
    \\SOH\b\DC2\ETX\STX\NULB\n\
    \\t\n\
    \\STX\b\SOH\DC2\ETX\STX\NULB\n\
    \\t\n\
    \\STX\ETX\NUL\DC2\ETX\EOT\NUL&\n\
    \\t\n\
    \\STX\ETX\SOH\DC2\ETX\ENQ\NUL\SYN\n\
    \\n\
    \\n\
    \\STX\ACK\NUL\DC2\EOT\a\NUL\DC2\SOH\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX\a\b\DC3\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETX\b\b4\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX\b\f\DC2\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX\b\DC4\GS\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX\b(2\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\SOH\DC2\ETX\t\b4\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\SOH\DC2\ETX\t\f\DC2\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\STX\DC2\ETX\t\DC4\GS\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\ETX\DC2\ETX\t(2\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\STX\DC2\ETX\n\
    \\b4\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\SOH\DC2\ETX\n\
    \\f\DC2\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\STX\DC2\ETX\n\
    \\DC4\GS\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\ETX\DC2\ETX\n\
    \(2\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\ETX\DC2\ETX\v\bG\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\SOH\DC2\ETX\v\f\SYN\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\STX\DC2\ETX\v\CAN%\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\ACK\DC2\ETX\v06\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\ETX\DC2\ETX\v7E\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\EOT\DC2\ETX\f\b7\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\EOT\SOH\DC2\ETX\f\f\DC3\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\EOT\STX\DC2\ETX\f\NAK\US\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\EOT\ETX\DC2\ETX\f*5\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\ENQ\DC2\ETX\r\b4\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ENQ\SOH\DC2\ETX\r\f\DC2\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ENQ\STX\DC2\ETX\r\DC4\GS\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ENQ\ETX\DC2\ETX\r(2\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\ACK\DC2\ETX\SO\b1\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ACK\SOH\DC2\ETX\SO\f\DC1\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ACK\STX\DC2\ETX\SO\DC3\ESC\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ACK\ETX\DC2\ETX\SO&/\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\a\DC2\ETX\SI\b1\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\a\SOH\DC2\ETX\SI\f\DC1\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\a\STX\DC2\ETX\SI\DC3\ESC\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\a\ETX\DC2\ETX\SI&/\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\b\DC2\ETX\DLE\b4\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\b\SOH\DC2\ETX\DLE\f\DC2\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\b\STX\DC2\ETX\DLE\DC4\GS\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\b\ETX\DC2\ETX\DLE(2\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\t\DC2\ETX\DC1\bi\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\t\SOH\DC2\ETX\DC1\f\FS\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\t\STX\DC2\ETX\DC1\RS=\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\t\ETX\DC2\ETX\DC1Hg\n\
    \\n\
    \\n\
    \\STX\EOT\NUL\DC2\EOT\DC4\NUL'\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\DC4\b\DC1\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\NAK\b\FS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ACK\DC2\ETX\NAK\b\SI\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\NAK\DLE\ETB\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\NAK\SUB\ESC\n\
    \\f\n\
    \\EOT\EOT\NUL\ETX\NUL\DC2\EOT\ETB\b&\t\n\
    \\f\n\
    \\ENQ\EOT\NUL\ETX\NUL\SOH\DC2\ETX\ETB\DLE\ETB\n\
    \\SO\n\
    \\ACK\EOT\NUL\ETX\NUL\b\NUL\DC2\EOT\CAN\DLE\FS\DC1\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\b\NUL\SOH\DC2\ETX\CAN\SYN\SUB\n\
    \\r\n\
    \\ACK\EOT\NUL\ETX\NUL\STX\NUL\DC2\ETX\EM\CANE\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\NUL\ACK\DC2\ETX\EM\CAN7\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\NUL\SOH\DC2\ETX\EM8@\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\NUL\ETX\DC2\ETX\EMCD\n\
    \\r\n\
    \\ACK\EOT\NUL\ETX\NUL\STX\SOH\DC2\ETX\SUB\CAN0\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\SOH\ACK\DC2\ETX\SUB\CAN!\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\SOH\SOH\DC2\ETX\SUB\"+\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\SOH\ETX\DC2\ETX\SUB./\n\
    \\r\n\
    \\ACK\EOT\NUL\ETX\NUL\STX\STX\DC2\ETX\ESC\CAN2\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\STX\ACK\DC2\ETX\ESC\CAN\"\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\STX\SOH\DC2\ETX\ESC#-\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\STX\ETX\DC2\ETX\ESC01\n\
    \\r\n\
    \\ACK\EOT\NUL\ETX\NUL\STX\ETX\DC2\ETX\GS\DLE!\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\ETX\ENQ\DC2\ETX\GS\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\ETX\SOH\DC2\ETX\GS\ETB\FS\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\ETX\ETX\DC2\ETX\GS\US \n\
    \\SO\n\
    \\ACK\EOT\NUL\ETX\NUL\ETX\NUL\DC2\EOT\US\DLE!\DC1\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\ETX\NUL\SOH\DC2\ETX\US\CAN!\n\
    \\SI\n\
    \\b\EOT\NUL\ETX\NUL\ETX\NUL\STX\NUL\DC2\ETX \CAN(\n\
    \\DLE\n\
    \\t\EOT\NUL\ETX\NUL\ETX\NUL\STX\NUL\ENQ\DC2\ETX \CAN\RS\n\
    \\DLE\n\
    \\t\EOT\NUL\ETX\NUL\ETX\NUL\STX\NUL\SOH\DC2\ETX \US#\n\
    \\DLE\n\
    \\t\EOT\NUL\ETX\NUL\ETX\NUL\STX\NUL\ETX\DC2\ETX &'\n\
    \\SO\n\
    \\ACK\EOT\NUL\ETX\NUL\ETX\SOH\DC2\EOT\"\DLE%\DC1\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\ETX\SOH\SOH\DC2\ETX\"\CAN\"\n\
    \\SI\n\
    \\b\EOT\NUL\ETX\NUL\ETX\SOH\STX\NUL\DC2\ETX#\CAN(\n\
    \\DLE\n\
    \\t\EOT\NUL\ETX\NUL\ETX\SOH\STX\NUL\ENQ\DC2\ETX#\CAN\RS\n\
    \\DLE\n\
    \\t\EOT\NUL\ETX\NUL\ETX\SOH\STX\NUL\SOH\DC2\ETX#\US#\n\
    \\DLE\n\
    \\t\EOT\NUL\ETX\NUL\ETX\SOH\STX\NUL\ETX\DC2\ETX#&'\n\
    \\SI\n\
    \\b\EOT\NUL\ETX\NUL\ETX\SOH\STX\SOH\DC2\ETX$\CAN7\n\
    \\DLE\n\
    \\t\EOT\NUL\ETX\NUL\ETX\SOH\STX\SOH\ENQ\DC2\ETX$\CAN\FS\n\
    \\DLE\n\
    \\t\EOT\NUL\ETX\NUL\ETX\SOH\STX\SOH\SOH\DC2\ETX$\GS2\n\
    \\DLE\n\
    \\t\EOT\NUL\ETX\NUL\ETX\SOH\STX\SOH\ETX\DC2\ETX$56\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT)\NUL*\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX)\b\DC2\n\
    \\n\
    \\n\
    \\STX\EOT\STX\DC2\EOT,\NUL7\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX,\b\DC1\n\
    \\v\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX-\b\FS\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ACK\DC2\ETX-\b\SI\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX-\DLE\ETB\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX-\SUB\ESC\n\
    \\f\n\
    \\EOT\EOT\STX\ETX\NUL\DC2\EOT/\b6\t\n\
    \\f\n\
    \\ENQ\EOT\STX\ETX\NUL\SOH\DC2\ETX/\DLE\ETB\n\
    \\r\n\
    \\ACK\EOT\STX\ETX\NUL\STX\NUL\DC2\ETX0\DLE \n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\NUL\ENQ\DC2\ETX0\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\NUL\SOH\DC2\ETX0\ETB\ESC\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\NUL\ETX\DC2\ETX0\RS\US\n\
    \\r\n\
    \\ACK\EOT\STX\ETX\NUL\STX\SOH\DC2\ETX1\DLE!\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\SOH\ENQ\DC2\ETX1\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\SOH\SOH\DC2\ETX1\ETB\FS\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\SOH\ETX\DC2\ETX1\US \n\
    \\SO\n\
    \\ACK\EOT\STX\ETX\NUL\b\NUL\DC2\EOT2\DLE5\DC1\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\b\NUL\SOH\DC2\ETX2\SYN!\n\
    \\r\n\
    \\ACK\EOT\STX\ETX\NUL\STX\STX\DC2\ETX3\CAN.\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\STX\ENQ\DC2\ETX3\CAN\FS\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\STX\SOH\DC2\ETX3\GS)\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\STX\ETX\DC2\ETX3,-\n\
    \\r\n\
    \\ACK\EOT\STX\ETX\NUL\STX\ETX\DC2\ETX4\CANL\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\ETX\ACK\DC2\ETX4\CAN7\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\ETX\SOH\DC2\ETX48G\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\ETX\ETX\DC2\ETX4JK\n\
    \\n\
    \\n\
    \\STX\EOT\ETX\DC2\EOT9\NUL:\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETX9\b\DC2\n\
    \\n\
    \\n\
    \\STX\EOT\EOT\DC2\EOT<\NULE\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\EOT\SOH\DC2\ETX<\b\DC1\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\NUL\DC2\ETX=\b\FS\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ACK\DC2\ETX=\b\SI\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\SOH\DC2\ETX=\DLE\ETB\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ETX\DC2\ETX=\SUB\ESC\n\
    \\f\n\
    \\EOT\EOT\EOT\ETX\NUL\DC2\EOT?\bD\t\n\
    \\f\n\
    \\ENQ\EOT\EOT\ETX\NUL\SOH\DC2\ETX?\DLE\ETB\n\
    \\r\n\
    \\ACK\EOT\EOT\ETX\NUL\STX\NUL\DC2\ETX@\DLE \n\
    \\SO\n\
    \\a\EOT\EOT\ETX\NUL\STX\NUL\ENQ\DC2\ETX@\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\EOT\ETX\NUL\STX\NUL\SOH\DC2\ETX@\ETB\ESC\n\
    \\SO\n\
    \\a\EOT\EOT\ETX\NUL\STX\NUL\ETX\DC2\ETX@\RS\US\n\
    \\r\n\
    \\ACK\EOT\EOT\ETX\NUL\STX\SOH\DC2\ETXA\DLE0\n\
    \\SO\n\
    \\a\EOT\EOT\ETX\NUL\STX\SOH\ENQ\DC2\ETXA\DLE\DC4\n\
    \\SO\n\
    \\a\EOT\EOT\ETX\NUL\STX\SOH\SOH\DC2\ETXA\NAK+\n\
    \\SO\n\
    \\a\EOT\EOT\ETX\NUL\STX\SOH\ETX\DC2\ETXA./\n\
    \\r\n\
    \\ACK\EOT\EOT\ETX\NUL\STX\STX\DC2\ETXB\DLE-\n\
    \\SO\n\
    \\a\EOT\EOT\ETX\NUL\STX\STX\ENQ\DC2\ETXB\DLE\DC4\n\
    \\SO\n\
    \\a\EOT\EOT\ETX\NUL\STX\STX\SOH\DC2\ETXB\NAK(\n\
    \\SO\n\
    \\a\EOT\EOT\ETX\NUL\STX\STX\ETX\DC2\ETXB+,\n\
    \\r\n\
    \\ACK\EOT\EOT\ETX\NUL\STX\ETX\DC2\ETXC\DLE2\n\
    \\SO\n\
    \\a\EOT\EOT\ETX\NUL\STX\ETX\ENQ\DC2\ETXC\DLE\DC4\n\
    \\SO\n\
    \\a\EOT\EOT\ETX\NUL\STX\ETX\SOH\DC2\ETXC\NAK-\n\
    \\SO\n\
    \\a\EOT\EOT\ETX\NUL\STX\ETX\ETX\DC2\ETXC01\n\
    \\n\
    \\n\
    \\STX\EOT\ENQ\DC2\EOTG\NULH\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ENQ\SOH\DC2\ETXG\b\DC2\n\
    \\n\
    \\n\
    \\STX\EOT\ACK\DC2\EOTJ\NULU\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ACK\SOH\DC2\ETXJ\b\NAK\n\
    \\v\n\
    \\EOT\EOT\ACK\STX\NUL\DC2\ETXK\b\FS\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ACK\DC2\ETXK\b\SI\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\SOH\DC2\ETXK\DLE\ETB\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ETX\DC2\ETXK\SUB\ESC\n\
    \\f\n\
    \\EOT\EOT\ACK\ETX\NUL\DC2\EOTL\bT\t\n\
    \\f\n\
    \\ENQ\EOT\ACK\ETX\NUL\SOH\DC2\ETXL\DLE\ETB\n\
    \\SO\n\
    \\ACK\EOT\ACK\ETX\NUL\b\NUL\DC2\EOTM\DLES\DC1\n\
    \\SO\n\
    \\a\EOT\ACK\ETX\NUL\b\NUL\SOH\DC2\ETXM\SYN\SUB\n\
    \\r\n\
    \\ACK\EOT\ACK\ETX\NUL\STX\NUL\DC2\ETXN\CAN(\n\
    \\SO\n\
    \\a\EOT\ACK\ETX\NUL\STX\NUL\ENQ\DC2\ETXN\CAN\RS\n\
    \\SO\n\
    \\a\EOT\ACK\ETX\NUL\STX\NUL\SOH\DC2\ETXN\US#\n\
    \\SO\n\
    \\a\EOT\ACK\ETX\NUL\STX\NUL\ETX\DC2\ETXN&'\n\
    \\r\n\
    \\ACK\EOT\ACK\ETX\NUL\STX\SOH\DC2\ETXO\CAN@\n\
    \\SO\n\
    \\a\EOT\ACK\ETX\NUL\STX\SOH\ACK\DC2\ETXO\CAN7\n\
    \\SO\n\
    \\a\EOT\ACK\ETX\NUL\STX\SOH\SOH\DC2\ETXO8;\n\
    \\SO\n\
    \\a\EOT\ACK\ETX\NUL\STX\SOH\ETX\DC2\ETXO>?\n\
    \\r\n\
    \\ACK\EOT\ACK\ETX\NUL\STX\STX\DC2\ETXP\CANF\n\
    \\SO\n\
    \\a\EOT\ACK\ETX\NUL\STX\STX\ACK\DC2\ETXP\CAN7\n\
    \\SO\n\
    \\a\EOT\ACK\ETX\NUL\STX\STX\SOH\DC2\ETXP8A\n\
    \\SO\n\
    \\a\EOT\ACK\ETX\NUL\STX\STX\ETX\DC2\ETXPDE\n\
    \\r\n\
    \\ACK\EOT\ACK\ETX\NUL\STX\ETX\DC2\ETXQ\CANG\n\
    \\SO\n\
    \\a\EOT\ACK\ETX\NUL\STX\ETX\ACK\DC2\ETXQ\CAN7\n\
    \\SO\n\
    \\a\EOT\ACK\ETX\NUL\STX\ETX\SOH\DC2\ETXQ8B\n\
    \\SO\n\
    \\a\EOT\ACK\ETX\NUL\STX\ETX\ETX\DC2\ETXQEF\n\
    \\r\n\
    \\ACK\EOT\ACK\ETX\NUL\STX\EOT\DC2\ETXR\CANE\n\
    \\SO\n\
    \\a\EOT\ACK\ETX\NUL\STX\EOT\ACK\DC2\ETXR\CAN7\n\
    \\SO\n\
    \\a\EOT\ACK\ETX\NUL\STX\EOT\SOH\DC2\ETXR8@\n\
    \\SO\n\
    \\a\EOT\ACK\ETX\NUL\STX\EOT\ETX\DC2\ETXRCD\n\
    \\n\
    \\n\
    \\STX\EOT\a\DC2\EOTW\NULo\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\a\SOH\DC2\ETXW\b\SYN\n\
    \\v\n\
    \\EOT\EOT\a\STX\NUL\DC2\ETXX\b\FS\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\ACK\DC2\ETXX\b\SI\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\SOH\DC2\ETXX\DLE\ETB\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\ETX\DC2\ETXX\SUB\ESC\n\
    \\f\n\
    \\EOT\EOT\a\ETX\NUL\DC2\EOTZ\bn\t\n\
    \\f\n\
    \\ENQ\EOT\a\ETX\NUL\SOH\DC2\ETXZ\DLE\ETB\n\
    \\r\n\
    \\ACK\EOT\a\ETX\NUL\STX\NUL\DC2\ETX[\DLE-\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\NUL\ENQ\DC2\ETX[\DLE\NAK\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\NUL\SOH\DC2\ETX[\SYN(\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\NUL\ETX\DC2\ETX[+,\n\
    \\r\n\
    \\ACK\EOT\a\ETX\NUL\STX\SOH\DC2\ETX\\\DLE\"\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\SOH\ENQ\DC2\ETX\\\DLE\NAK\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\SOH\SOH\DC2\ETX\\\SYN\GS\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\SOH\ETX\DC2\ETX\\ !\n\
    \\r\n\
    \\ACK\EOT\a\ETX\NUL\STX\STX\DC2\ETX]\DLE \n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\STX\ENQ\DC2\ETX]\DLE\NAK\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\STX\SOH\DC2\ETX]\SYN\ESC\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\STX\ETX\DC2\ETX]\RS\US\n\
    \\r\n\
    \\ACK\EOT\a\ETX\NUL\STX\ETX\DC2\ETX^\DLE)\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\ETX\ENQ\DC2\ETX^\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\ETX\SOH\DC2\ETX^\ETB$\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\ETX\ETX\DC2\ETX^'(\n\
    \\r\n\
    \\ACK\EOT\a\ETX\NUL\STX\EOT\DC2\ETX_\DLE+\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\EOT\ENQ\DC2\ETX_\DLE\NAK\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\EOT\SOH\DC2\ETX_\SYN&\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\EOT\ETX\DC2\ETX_)*\n\
    \\r\n\
    \\ACK\EOT\a\ETX\NUL\STX\ENQ\DC2\ETX`\DLE*\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\ENQ\ENQ\DC2\ETX`\DLE\NAK\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\ENQ\SOH\DC2\ETX`\SYN%\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\ENQ\ETX\DC2\ETX`()\n\
    \\r\n\
    \\ACK\EOT\a\ETX\NUL\STX\ACK\DC2\ETXa\DLE+\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\ACK\ENQ\DC2\ETXa\DLE\NAK\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\ACK\SOH\DC2\ETXa\SYN&\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\ACK\ETX\DC2\ETXa)*\n\
    \\r\n\
    \\ACK\EOT\a\ETX\NUL\STX\a\DC2\ETXb\DLE\"\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\a\ENQ\DC2\ETXb\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\a\SOH\DC2\ETXb\ETB\GS\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\a\ETX\DC2\ETXb !\n\
    \\r\n\
    \\ACK\EOT\a\ETX\NUL\STX\b\DC2\ETXc\DLE'\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\b\ENQ\DC2\ETXc\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\b\SOH\DC2\ETXc\ETB\"\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\b\ETX\DC2\ETXc%&\n\
    \\r\n\
    \\ACK\EOT\a\ETX\NUL\STX\t\DC2\ETXd\DLE!\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\t\ENQ\DC2\ETXd\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\t\SOH\DC2\ETXd\ETB\ESC\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\t\ETX\DC2\ETXd\RS \n\
    \\r\n\
    \\ACK\EOT\a\ETX\NUL\STX\n\
    \\DC2\ETXe\DLE!\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\n\
    \\ENQ\DC2\ETXe\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\n\
    \\SOH\DC2\ETXe\ETB\ESC\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\n\
    \\ETX\DC2\ETXe\RS \n\
    \\r\n\
    \\ACK\EOT\a\ETX\NUL\STX\v\DC2\ETXf\DLE%\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\v\ENQ\DC2\ETXf\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\v\SOH\DC2\ETXf\ETB\US\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\v\ETX\DC2\ETXf\"$\n\
    \\r\n\
    \\ACK\EOT\a\ETX\NUL\STX\f\DC2\ETXg\DLE$\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\f\ENQ\DC2\ETXg\DLE\NAK\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\f\SOH\DC2\ETXg\SYN\RS\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\f\ETX\DC2\ETXg!#\n\
    \\r\n\
    \\ACK\EOT\a\ETX\NUL\STX\r\DC2\ETXh\DLE+\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\r\ENQ\DC2\ETXh\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\r\SOH\DC2\ETXh\ETB%\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\r\ETX\DC2\ETXh(*\n\
    \\r\n\
    \\ACK\EOT\a\ETX\NUL\STX\SO\DC2\ETXi\DLE7\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\SO\ENQ\DC2\ETXi\DLE\NAK\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\SO\SOH\DC2\ETXi\SYN1\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\SO\ETX\DC2\ETXi46\n\
    \\r\n\
    \\ACK\EOT\a\ETX\NUL\STX\SI\DC2\ETXj\DLE-\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\SI\ENQ\DC2\ETXj\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\SI\SOH\DC2\ETXj\ETB'\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\SI\ETX\DC2\ETXj*,\n\
    \\r\n\
    \\ACK\EOT\a\ETX\NUL\STX\DLE\DC2\ETXk\DLE*\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\DLE\ENQ\DC2\ETXk\DLE\NAK\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\DLE\SOH\DC2\ETXk\SYN$\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\DLE\ETX\DC2\ETXk')\n\
    \\r\n\
    \\ACK\EOT\a\ETX\NUL\STX\DC1\DC2\ETXl\DLE>\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\DC1\ENQ\DC2\ETXl\DLE\NAK\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\DC1\SOH\DC2\ETXl\SYN8\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\DC1\ETX\DC2\ETXl;=\n\
    \\r\n\
    \\ACK\EOT\a\ETX\NUL\STX\DC2\DC2\ETXm\DLE=\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\DC2\ENQ\DC2\ETXm\DLE\NAK\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\DC2\SOH\DC2\ETXm\SYN7\n\
    \\SO\n\
    \\a\EOT\a\ETX\NUL\STX\DC2\ETX\DC2\ETXm:<\n\
    \\n\
    \\n\
    \\STX\EOT\b\DC2\EOTq\NULx\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\b\SOH\DC2\ETXq\b\DLE\n\
    \\v\n\
    \\EOT\EOT\b\STX\NUL\DC2\ETXr\b\FS\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\ACK\DC2\ETXr\b\SI\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\SOH\DC2\ETXr\DLE\ETB\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\ETX\DC2\ETXr\SUB\ESC\n\
    \\f\n\
    \\EOT\EOT\b\ETX\NUL\DC2\EOTt\bw\t\n\
    \\f\n\
    \\ENQ\EOT\b\ETX\NUL\SOH\DC2\ETXt\DLE\ETB\n\
    \\r\n\
    \\ACK\EOT\b\ETX\NUL\STX\NUL\DC2\ETXu\DLE \n\
    \\SO\n\
    \\a\EOT\b\ETX\NUL\STX\NUL\ENQ\DC2\ETXu\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\b\ETX\NUL\STX\NUL\SOH\DC2\ETXu\ETB\ESC\n\
    \\SO\n\
    \\a\EOT\b\ETX\NUL\STX\NUL\ETX\DC2\ETXu\RS\US\n\
    \\r\n\
    \\ACK\EOT\b\ETX\NUL\STX\SOH\DC2\ETXv\DLE%\n\
    \\SO\n\
    \\a\EOT\b\ETX\NUL\STX\SOH\ENQ\DC2\ETXv\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\b\ETX\NUL\STX\SOH\SOH\DC2\ETXv\ETB \n\
    \\SO\n\
    \\a\EOT\b\ETX\NUL\STX\SOH\ETX\DC2\ETXv#$\n\
    \\n\
    \\n\
    \\STX\EOT\t\DC2\EOTz\NUL|\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\t\SOH\DC2\ETXz\b\DC1\n\
    \\v\n\
    \\EOT\EOT\t\STX\NUL\DC2\ETX{\b(\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\ACK\DC2\ETX{\b\GS\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\SOH\DC2\ETX{\RS#\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\ETX\DC2\ETX{&'\n\
    \\v\n\
    \\STX\EOT\n\
    \\DC2\ENQ~\NUL\133\SOH\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\n\
    \\SOH\DC2\ETX~\b\DC1\n\
    \\v\n\
    \\EOT\EOT\n\
    \\STX\NUL\DC2\ETX\DEL\b\FS\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ACK\DC2\ETX\DEL\b\SI\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\SOH\DC2\ETX\DEL\DLE\ETB\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ETX\DC2\ETX\DEL\SUB\ESC\n\
    \\SO\n\
    \\EOT\EOT\n\
    \\ETX\NUL\DC2\ACK\129\SOH\b\132\SOH\t\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\ETX\NUL\SOH\DC2\EOT\129\SOH\DLE\ETB\n\
    \\SO\n\
    \\ACK\EOT\n\
    \\ETX\NUL\STX\NUL\DC2\EOT\130\SOH\DLE \n\
    \\SI\n\
    \\a\EOT\n\
    \\ETX\NUL\STX\NUL\ENQ\DC2\EOT\130\SOH\DLE\SYN\n\
    \\SI\n\
    \\a\EOT\n\
    \\ETX\NUL\STX\NUL\SOH\DC2\EOT\130\SOH\ETB\ESC\n\
    \\SI\n\
    \\a\EOT\n\
    \\ETX\NUL\STX\NUL\ETX\DC2\EOT\130\SOH\RS\US\n\
    \\SO\n\
    \\ACK\EOT\n\
    \\ETX\NUL\STX\SOH\DC2\EOT\131\SOH\DLE%\n\
    \\SI\n\
    \\a\EOT\n\
    \\ETX\NUL\STX\SOH\ENQ\DC2\EOT\131\SOH\DLE\SYN\n\
    \\SI\n\
    \\a\EOT\n\
    \\ETX\NUL\STX\SOH\SOH\DC2\EOT\131\SOH\ETB \n\
    \\SI\n\
    \\a\EOT\n\
    \\ETX\NUL\STX\SOH\ETX\DC2\EOT\131\SOH#$\n\
    \\f\n\
    \\STX\EOT\v\DC2\ACK\135\SOH\NUL\137\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\v\SOH\DC2\EOT\135\SOH\b\DC2\n\
    \\f\n\
    \\EOT\EOT\v\STX\NUL\DC2\EOT\136\SOH\b)\n\
    \\r\n\
    \\ENQ\EOT\v\STX\NUL\ACK\DC2\EOT\136\SOH\b\GS\n\
    \\r\n\
    \\ENQ\EOT\v\STX\NUL\SOH\DC2\EOT\136\SOH\RS$\n\
    \\r\n\
    \\ENQ\EOT\v\STX\NUL\ETX\DC2\EOT\136\SOH'(\n\
    \\f\n\
    \\STX\EOT\f\DC2\ACK\139\SOH\NUL\146\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\f\SOH\DC2\EOT\139\SOH\b\DLE\n\
    \\f\n\
    \\EOT\EOT\f\STX\NUL\DC2\EOT\140\SOH\b\FS\n\
    \\r\n\
    \\ENQ\EOT\f\STX\NUL\ACK\DC2\EOT\140\SOH\b\SI\n\
    \\r\n\
    \\ENQ\EOT\f\STX\NUL\SOH\DC2\EOT\140\SOH\DLE\ETB\n\
    \\r\n\
    \\ENQ\EOT\f\STX\NUL\ETX\DC2\EOT\140\SOH\SUB\ESC\n\
    \\SO\n\
    \\EOT\EOT\f\ETX\NUL\DC2\ACK\142\SOH\b\145\SOH\t\n\
    \\r\n\
    \\ENQ\EOT\f\ETX\NUL\SOH\DC2\EOT\142\SOH\DLE\ETB\n\
    \\SO\n\
    \\ACK\EOT\f\ETX\NUL\STX\NUL\DC2\EOT\143\SOH\DLE \n\
    \\SI\n\
    \\a\EOT\f\ETX\NUL\STX\NUL\ENQ\DC2\EOT\143\SOH\DLE\SYN\n\
    \\SI\n\
    \\a\EOT\f\ETX\NUL\STX\NUL\SOH\DC2\EOT\143\SOH\ETB\ESC\n\
    \\SI\n\
    \\a\EOT\f\ETX\NUL\STX\NUL\ETX\DC2\EOT\143\SOH\RS\US\n\
    \\SO\n\
    \\ACK\EOT\f\ETX\NUL\STX\SOH\DC2\EOT\144\SOH\DLE*\n\
    \\SI\n\
    \\a\EOT\f\ETX\NUL\STX\SOH\ENQ\DC2\EOT\144\SOH\DLE\DC4\n\
    \\SI\n\
    \\a\EOT\f\ETX\NUL\STX\SOH\SOH\DC2\EOT\144\SOH\NAK%\n\
    \\SI\n\
    \\a\EOT\f\ETX\NUL\STX\SOH\ETX\DC2\EOT\144\SOH()\n\
    \\f\n\
    \\STX\EOT\r\DC2\ACK\148\SOH\NUL\149\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\r\SOH\DC2\EOT\148\SOH\b\DC1\n\
    \\f\n\
    \\STX\EOT\SO\DC2\ACK\152\SOH\NUL\158\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\SO\SOH\DC2\EOT\152\SOH\b\DC1\n\
    \\f\n\
    \\EOT\EOT\SO\STX\NUL\DC2\EOT\153\SOH\b\FS\n\
    \\r\n\
    \\ENQ\EOT\SO\STX\NUL\ACK\DC2\EOT\153\SOH\b\SI\n\
    \\r\n\
    \\ENQ\EOT\SO\STX\NUL\SOH\DC2\EOT\153\SOH\DLE\ETB\n\
    \\r\n\
    \\ENQ\EOT\SO\STX\NUL\ETX\DC2\EOT\153\SOH\SUB\ESC\n\
    \\SO\n\
    \\EOT\EOT\SO\ETX\NUL\DC2\ACK\155\SOH\b\157\SOH\t\n\
    \\r\n\
    \\ENQ\EOT\SO\ETX\NUL\SOH\DC2\EOT\155\SOH\DLE\ETB\n\
    \\SO\n\
    \\ACK\EOT\SO\ETX\NUL\STX\NUL\DC2\EOT\156\SOH\DLE \n\
    \\SI\n\
    \\a\EOT\SO\ETX\NUL\STX\NUL\ENQ\DC2\EOT\156\SOH\DLE\SYN\n\
    \\SI\n\
    \\a\EOT\SO\ETX\NUL\STX\NUL\SOH\DC2\EOT\156\SOH\ETB\ESC\n\
    \\SI\n\
    \\a\EOT\SO\ETX\NUL\STX\NUL\ETX\DC2\EOT\156\SOH\RS\US\n\
    \\f\n\
    \\STX\EOT\SI\DC2\ACK\160\SOH\NUL\161\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\SI\SOH\DC2\EOT\160\SOH\b\DC2\n\
    \\f\n\
    \\STX\EOT\DLE\DC2\ACK\163\SOH\NUL\170\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\DLE\SOH\DC2\EOT\163\SOH\b\DC2\n\
    \\f\n\
    \\EOT\EOT\DLE\STX\NUL\DC2\EOT\164\SOH\b\FS\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\NUL\ACK\DC2\EOT\164\SOH\b\SI\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\NUL\SOH\DC2\EOT\164\SOH\DLE\ETB\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\NUL\ETX\DC2\EOT\164\SOH\SUB\ESC\n\
    \\SO\n\
    \\EOT\EOT\DLE\ETX\NUL\DC2\ACK\166\SOH\b\169\SOH\t\n\
    \\r\n\
    \\ENQ\EOT\DLE\ETX\NUL\SOH\DC2\EOT\166\SOH\DLE\ETB\n\
    \\SO\n\
    \\ACK\EOT\DLE\ETX\NUL\STX\NUL\DC2\EOT\167\SOH\DLE \n\
    \\SI\n\
    \\a\EOT\DLE\ETX\NUL\STX\NUL\ENQ\DC2\EOT\167\SOH\DLE\SYN\n\
    \\SI\n\
    \\a\EOT\DLE\ETX\NUL\STX\NUL\SOH\DC2\EOT\167\SOH\ETB\ESC\n\
    \\SI\n\
    \\a\EOT\DLE\ETX\NUL\STX\NUL\ETX\DC2\EOT\167\SOH\RS\US\n\
    \\SO\n\
    \\ACK\EOT\DLE\ETX\NUL\STX\SOH\DC2\EOT\168\SOH\DLE*\n\
    \\SI\n\
    \\a\EOT\DLE\ETX\NUL\STX\SOH\ENQ\DC2\EOT\168\SOH\DLE\DC4\n\
    \\SI\n\
    \\a\EOT\DLE\ETX\NUL\STX\SOH\SOH\DC2\EOT\168\SOH\NAK%\n\
    \\SI\n\
    \\a\EOT\DLE\ETX\NUL\STX\SOH\ETX\DC2\EOT\168\SOH()\n\
    \\f\n\
    \\STX\EOT\DC1\DC2\ACK\172\SOH\NUL\173\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\DC1\SOH\DC2\EOT\172\SOH\b\DC3b\ACKproto3"