{- This file was auto-generated from shared.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Shared (
        Empty(), StreamIdentifier(), UUID(), UUID'Value(..),
        _UUID'Structured', _UUID'String, UUID'Structured()
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
{- | Fields :
      -}
data Empty
  = Empty'_constructor {_Empty'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Empty where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message Empty where
  messageName _ = Data.Text.pack "event_store.client.shared.Empty"
  packedMessageDescriptor _
    = "\n\
      \\ENQEmpty"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Empty'_unknownFields
        (\ x__ y__ -> x__ {_Empty'_unknownFields = y__})
  defMessage = Empty'_constructor {_Empty'_unknownFields = []}
  parseMessage
    = let
        loop :: Empty -> Data.ProtoLens.Encoding.Bytes.Parser Empty
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
          (do loop Data.ProtoLens.defMessage) "Empty"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData Empty where
  rnf
    = \ x__ -> Control.DeepSeq.deepseq (_Empty'_unknownFields x__) ()
{- | Fields :
     
         * 'Proto.Shared_Fields.streamName' @:: Lens' StreamIdentifier Data.ByteString.ByteString@ -}
data StreamIdentifier
  = StreamIdentifier'_constructor {_StreamIdentifier'streamName :: !Data.ByteString.ByteString,
                                   _StreamIdentifier'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StreamIdentifier where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField StreamIdentifier "streamName" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StreamIdentifier'streamName
           (\ x__ y__ -> x__ {_StreamIdentifier'streamName = y__}))
        Prelude.id
instance Data.ProtoLens.Message StreamIdentifier where
  messageName _
    = Data.Text.pack "event_store.client.shared.StreamIdentifier"
  packedMessageDescriptor _
    = "\n\
      \\DLEStreamIdentifier\DC2\RS\n\
      \\n\
      \streamName\CAN\ETX \SOH(\fR\n\
      \streamNameJ\EOT\b\SOH\DLE\ETX"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        streamName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "streamName"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"streamName")) ::
              Data.ProtoLens.FieldDescriptor StreamIdentifier
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 3, streamName__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _StreamIdentifier'_unknownFields
        (\ x__ y__ -> x__ {_StreamIdentifier'_unknownFields = y__})
  defMessage
    = StreamIdentifier'_constructor
        {_StreamIdentifier'streamName = Data.ProtoLens.fieldDefault,
         _StreamIdentifier'_unknownFields = []}
  parseMessage
    = let
        loop ::
          StreamIdentifier
          -> Data.ProtoLens.Encoding.Bytes.Parser StreamIdentifier
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
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "streamName"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"streamName") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "StreamIdentifier"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view (Data.ProtoLens.Field.field @"streamName") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                      ((\ bs
                          -> (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                  (Prelude.fromIntegral (Data.ByteString.length bs)))
                               (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData StreamIdentifier where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_StreamIdentifier'_unknownFields x__)
             (Control.DeepSeq.deepseq (_StreamIdentifier'streamName x__) ())
{- | Fields :
     
         * 'Proto.Shared_Fields.maybe'value' @:: Lens' UUID (Prelude.Maybe UUID'Value)@
         * 'Proto.Shared_Fields.maybe'structured' @:: Lens' UUID (Prelude.Maybe UUID'Structured)@
         * 'Proto.Shared_Fields.structured' @:: Lens' UUID UUID'Structured@
         * 'Proto.Shared_Fields.maybe'string' @:: Lens' UUID (Prelude.Maybe Data.Text.Text)@
         * 'Proto.Shared_Fields.string' @:: Lens' UUID Data.Text.Text@ -}
data UUID
  = UUID'_constructor {_UUID'value :: !(Prelude.Maybe UUID'Value),
                       _UUID'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show UUID where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data UUID'Value
  = UUID'Structured' !UUID'Structured | UUID'String !Data.Text.Text
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField UUID "maybe'value" (Prelude.Maybe UUID'Value) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UUID'value (\ x__ y__ -> x__ {_UUID'value = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField UUID "maybe'structured" (Prelude.Maybe UUID'Structured) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UUID'value (\ x__ y__ -> x__ {_UUID'value = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (UUID'Structured' x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap UUID'Structured' y__))
instance Data.ProtoLens.Field.HasField UUID "structured" UUID'Structured where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UUID'value (\ x__ y__ -> x__ {_UUID'value = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (UUID'Structured' x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap UUID'Structured' y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField UUID "maybe'string" (Prelude.Maybe Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UUID'value (\ x__ y__ -> x__ {_UUID'value = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (UUID'String x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap UUID'String y__))
instance Data.ProtoLens.Field.HasField UUID "string" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UUID'value (\ x__ y__ -> x__ {_UUID'value = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (UUID'String x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap UUID'String y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Message UUID where
  messageName _ = Data.Text.pack "event_store.client.shared.UUID"
  packedMessageDescriptor _
    = "\n\
      \\EOTUUID\DC2L\n\
      \\n\
      \structured\CAN\SOH \SOH(\v2*.event_store.client.shared.UUID.StructuredH\NULR\n\
      \structured\DC2\CAN\n\
      \\ACKstring\CAN\STX \SOH(\tH\NULR\ACKstring\SUBv\n\
      \\n\
      \Structured\DC22\n\
      \\NAKmost_significant_bits\CAN\SOH \SOH(\ETXR\DC3mostSignificantBits\DC24\n\
      \\SYNleast_significant_bits\CAN\STX \SOH(\ETXR\DC4leastSignificantBitsB\a\n\
      \\ENQvalue"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        structured__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "structured"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor UUID'Structured)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'structured")) ::
              Data.ProtoLens.FieldDescriptor UUID
        string__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "string"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'string")) ::
              Data.ProtoLens.FieldDescriptor UUID
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, structured__field_descriptor),
           (Data.ProtoLens.Tag 2, string__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _UUID'_unknownFields
        (\ x__ y__ -> x__ {_UUID'_unknownFields = y__})
  defMessage
    = UUID'_constructor
        {_UUID'value = Prelude.Nothing, _UUID'_unknownFields = []}
  parseMessage
    = let
        loop :: UUID -> Data.ProtoLens.Encoding.Bytes.Parser UUID
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
                                       "structured"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"structured") y x)
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
                                       "string"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"string") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "UUID"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'value") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just (UUID'Structured' v))
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
                (Prelude.Just (UUID'String v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.Text.Encoding.encodeUtf8
                          v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData UUID where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_UUID'_unknownFields x__)
             (Control.DeepSeq.deepseq (_UUID'value x__) ())
instance Control.DeepSeq.NFData UUID'Value where
  rnf (UUID'Structured' x__) = Control.DeepSeq.rnf x__
  rnf (UUID'String x__) = Control.DeepSeq.rnf x__
_UUID'Structured' ::
  Data.ProtoLens.Prism.Prism' UUID'Value UUID'Structured
_UUID'Structured'
  = Data.ProtoLens.Prism.prism'
      UUID'Structured'
      (\ p__
         -> case p__ of
              (UUID'Structured' p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_UUID'String ::
  Data.ProtoLens.Prism.Prism' UUID'Value Data.Text.Text
_UUID'String
  = Data.ProtoLens.Prism.prism'
      UUID'String
      (\ p__
         -> case p__ of
              (UUID'String p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
     
         * 'Proto.Shared_Fields.mostSignificantBits' @:: Lens' UUID'Structured Data.Int.Int64@
         * 'Proto.Shared_Fields.leastSignificantBits' @:: Lens' UUID'Structured Data.Int.Int64@ -}
data UUID'Structured
  = UUID'Structured'_constructor {_UUID'Structured'mostSignificantBits :: !Data.Int.Int64,
                                  _UUID'Structured'leastSignificantBits :: !Data.Int.Int64,
                                  _UUID'Structured'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show UUID'Structured where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField UUID'Structured "mostSignificantBits" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UUID'Structured'mostSignificantBits
           (\ x__ y__ -> x__ {_UUID'Structured'mostSignificantBits = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField UUID'Structured "leastSignificantBits" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UUID'Structured'leastSignificantBits
           (\ x__ y__ -> x__ {_UUID'Structured'leastSignificantBits = y__}))
        Prelude.id
instance Data.ProtoLens.Message UUID'Structured where
  messageName _
    = Data.Text.pack "event_store.client.shared.UUID.Structured"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \Structured\DC22\n\
      \\NAKmost_significant_bits\CAN\SOH \SOH(\ETXR\DC3mostSignificantBits\DC24\n\
      \\SYNleast_significant_bits\CAN\STX \SOH(\ETXR\DC4leastSignificantBits"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        mostSignificantBits__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "most_significant_bits"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"mostSignificantBits")) ::
              Data.ProtoLens.FieldDescriptor UUID'Structured
        leastSignificantBits__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "least_significant_bits"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"leastSignificantBits")) ::
              Data.ProtoLens.FieldDescriptor UUID'Structured
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, mostSignificantBits__field_descriptor),
           (Data.ProtoLens.Tag 2, leastSignificantBits__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _UUID'Structured'_unknownFields
        (\ x__ y__ -> x__ {_UUID'Structured'_unknownFields = y__})
  defMessage
    = UUID'Structured'_constructor
        {_UUID'Structured'mostSignificantBits = Data.ProtoLens.fieldDefault,
         _UUID'Structured'leastSignificantBits = Data.ProtoLens.fieldDefault,
         _UUID'Structured'_unknownFields = []}
  parseMessage
    = let
        loop ::
          UUID'Structured
          -> Data.ProtoLens.Encoding.Bytes.Parser UUID'Structured
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
                                       "most_significant_bits"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"mostSignificantBits") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "least_significant_bits"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"leastSignificantBits") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Structured"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view
                      (Data.ProtoLens.Field.field @"mostSignificantBits") _x
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
                   _v
                     = Lens.Family2.view
                         (Data.ProtoLens.Field.field @"leastSignificantBits") _x
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
instance Control.DeepSeq.NFData UUID'Structured where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_UUID'Structured'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_UUID'Structured'mostSignificantBits x__)
                (Control.DeepSeq.deepseq
                   (_UUID'Structured'leastSignificantBits x__) ()))
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\fshared.proto\DC2\EMevent_store.client.shared\"\239\SOH\n\
    \\EOTUUID\DC2L\n\
    \\n\
    \structured\CAN\SOH \SOH(\v2*.event_store.client.shared.UUID.StructuredH\NULR\n\
    \structured\DC2\CAN\n\
    \\ACKstring\CAN\STX \SOH(\tH\NULR\ACKstring\SUBv\n\
    \\n\
    \Structured\DC22\n\
    \\NAKmost_significant_bits\CAN\SOH \SOH(\ETXR\DC3mostSignificantBits\DC24\n\
    \\SYNleast_significant_bits\CAN\STX \SOH(\ETXR\DC4leastSignificantBitsB\a\n\
    \\ENQvalue\"\a\n\
    \\ENQEmpty\"8\n\
    \\DLEStreamIdentifier\DC2\RS\n\
    \\n\
    \streamName\CAN\ETX \SOH(\fR\n\
    \streamNameJ\EOT\b\SOH\DLE\ETXB&\n\
    \$com.eventstore.dbclient.proto.sharedJ\137\EOT\n\
    \\ACK\DC2\EOT\NUL\NUL\NAK\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\SOH\NUL\"\n\
    \\b\n\
    \\SOH\b\DC2\ETX\STX\NUL=\n\
    \\t\n\
    \\STX\b\SOH\DC2\ETX\STX\NUL=\n\
    \\n\
    \\n\
    \\STX\EOT\NUL\DC2\EOT\EOT\NUL\SO\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\EOT\b\f\n\
    \\f\n\
    \\EOT\EOT\NUL\b\NUL\DC2\EOT\ENQ\b\b\t\n\
    \\f\n\
    \\ENQ\EOT\NUL\b\NUL\SOH\DC2\ETX\ENQ\SO\DC3\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\ACK\DLE*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ACK\DC2\ETX\ACK\DLE\SUB\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\ACK\ESC%\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\ACK()\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\SOH\DC2\ETX\a\DLE\"\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ENQ\DC2\ETX\a\DLE\SYN\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\SOH\DC2\ETX\a\ETB\GS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ETX\DC2\ETX\a !\n\
    \\f\n\
    \\EOT\EOT\NUL\ETX\NUL\DC2\EOT\n\
    \\b\r\t\n\
    \\f\n\
    \\ENQ\EOT\NUL\ETX\NUL\SOH\DC2\ETX\n\
    \\DLE\SUB\n\
    \\r\n\
    \\ACK\EOT\NUL\ETX\NUL\STX\NUL\DC2\ETX\v\DLE0\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\NUL\ENQ\DC2\ETX\v\DLE\NAK\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\NUL\SOH\DC2\ETX\v\SYN+\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\NUL\ETX\DC2\ETX\v./\n\
    \\r\n\
    \\ACK\EOT\NUL\ETX\NUL\STX\SOH\DC2\ETX\f\DLE1\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\SOH\ENQ\DC2\ETX\f\DLE\NAK\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\SOH\SOH\DC2\ETX\f\SYN,\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\SOH\ETX\DC2\ETX\f/0\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT\SI\NUL\DLE\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\SI\b\r\n\
    \\n\
    \\n\
    \\STX\EOT\STX\DC2\EOT\DC2\NUL\NAK\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX\DC2\b\CAN\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\t\DC2\ETX\DC3\b\CAN\n\
    \\v\n\
    \\EOT\EOT\STX\t\NUL\DC2\ETX\DC3\DC1\ETB\n\
    \\f\n\
    \\ENQ\EOT\STX\t\NUL\SOH\DC2\ETX\DC3\DC1\DC2\n\
    \\f\n\
    \\ENQ\EOT\STX\t\NUL\STX\DC2\ETX\DC3\SYN\ETB\n\
    \\v\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX\DC4\b\GS\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ENQ\DC2\ETX\DC4\b\r\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX\DC4\SO\CAN\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX\DC4\ESC\FSb\ACKproto3"