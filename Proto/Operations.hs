{- This file was auto-generated from operations.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Operations (
        Operations(..), ScavengeResp(), ScavengeResp'ScavengeResult(..),
        ScavengeResp'ScavengeResult(),
        ScavengeResp'ScavengeResult'UnrecognizedValue,
        SetNodePriorityReq(), StartScavengeReq(),
        StartScavengeReq'Options(), StopScavengeReq(),
        StopScavengeReq'Options()
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
     
         * 'Proto.Operations_Fields.scavengeId' @:: Lens' ScavengeResp Data.Text.Text@
         * 'Proto.Operations_Fields.scavengeResult' @:: Lens' ScavengeResp ScavengeResp'ScavengeResult@ -}
data ScavengeResp
  = ScavengeResp'_constructor {_ScavengeResp'scavengeId :: !Data.Text.Text,
                               _ScavengeResp'scavengeResult :: !ScavengeResp'ScavengeResult,
                               _ScavengeResp'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ScavengeResp where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ScavengeResp "scavengeId" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ScavengeResp'scavengeId
           (\ x__ y__ -> x__ {_ScavengeResp'scavengeId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ScavengeResp "scavengeResult" ScavengeResp'ScavengeResult where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ScavengeResp'scavengeResult
           (\ x__ y__ -> x__ {_ScavengeResp'scavengeResult = y__}))
        Prelude.id
instance Data.ProtoLens.Message ScavengeResp where
  messageName _
    = Data.Text.pack "event_store.client.operations.ScavengeResp"
  packedMessageDescriptor _
    = "\n\
      \\fScavengeResp\DC2\US\n\
      \\vscavenge_id\CAN\SOH \SOH(\tR\n\
      \scavengeId\DC2c\n\
      \\SIscavenge_result\CAN\STX \SOH(\SO2:.event_store.client.operations.ScavengeResp.ScavengeResultR\SOscavengeResult\":\n\
      \\SOScavengeResult\DC2\v\n\
      \\aStarted\DLE\NUL\DC2\SO\n\
      \\n\
      \InProgress\DLE\SOH\DC2\v\n\
      \\aStopped\DLE\STX"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        scavengeId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "scavenge_id"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"scavengeId")) ::
              Data.ProtoLens.FieldDescriptor ScavengeResp
        scavengeResult__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "scavenge_result"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor ScavengeResp'ScavengeResult)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"scavengeResult")) ::
              Data.ProtoLens.FieldDescriptor ScavengeResp
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, scavengeId__field_descriptor),
           (Data.ProtoLens.Tag 2, scavengeResult__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ScavengeResp'_unknownFields
        (\ x__ y__ -> x__ {_ScavengeResp'_unknownFields = y__})
  defMessage
    = ScavengeResp'_constructor
        {_ScavengeResp'scavengeId = Data.ProtoLens.fieldDefault,
         _ScavengeResp'scavengeResult = Data.ProtoLens.fieldDefault,
         _ScavengeResp'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ScavengeResp -> Data.ProtoLens.Encoding.Bytes.Parser ScavengeResp
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
                                       "scavenge_id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"scavengeId") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "scavenge_result"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"scavengeResult") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ScavengeResp"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view (Data.ProtoLens.Field.field @"scavengeId") _x
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
                         (Data.ProtoLens.Field.field @"scavengeResult") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            ((Prelude..)
                               Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                            Prelude.fromEnum
                            _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ScavengeResp where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ScavengeResp'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ScavengeResp'scavengeId x__)
                (Control.DeepSeq.deepseq (_ScavengeResp'scavengeResult x__) ()))
newtype ScavengeResp'ScavengeResult'UnrecognizedValue
  = ScavengeResp'ScavengeResult'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data ScavengeResp'ScavengeResult
  = ScavengeResp'Started |
    ScavengeResp'InProgress |
    ScavengeResp'Stopped |
    ScavengeResp'ScavengeResult'Unrecognized !ScavengeResp'ScavengeResult'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum ScavengeResp'ScavengeResult where
  maybeToEnum 0 = Prelude.Just ScavengeResp'Started
  maybeToEnum 1 = Prelude.Just ScavengeResp'InProgress
  maybeToEnum 2 = Prelude.Just ScavengeResp'Stopped
  maybeToEnum k
    = Prelude.Just
        (ScavengeResp'ScavengeResult'Unrecognized
           (ScavengeResp'ScavengeResult'UnrecognizedValue
              (Prelude.fromIntegral k)))
  showEnum ScavengeResp'Started = "Started"
  showEnum ScavengeResp'InProgress = "InProgress"
  showEnum ScavengeResp'Stopped = "Stopped"
  showEnum
    (ScavengeResp'ScavengeResult'Unrecognized (ScavengeResp'ScavengeResult'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "Started" = Prelude.Just ScavengeResp'Started
    | (Prelude.==) k "InProgress"
    = Prelude.Just ScavengeResp'InProgress
    | (Prelude.==) k "Stopped" = Prelude.Just ScavengeResp'Stopped
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded ScavengeResp'ScavengeResult where
  minBound = ScavengeResp'Started
  maxBound = ScavengeResp'Stopped
instance Prelude.Enum ScavengeResp'ScavengeResult where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum ScavengeResult: "
              (Prelude.show k__)))
        Prelude.id
        (Data.ProtoLens.maybeToEnum k__)
  fromEnum ScavengeResp'Started = 0
  fromEnum ScavengeResp'InProgress = 1
  fromEnum ScavengeResp'Stopped = 2
  fromEnum
    (ScavengeResp'ScavengeResult'Unrecognized (ScavengeResp'ScavengeResult'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ ScavengeResp'Stopped
    = Prelude.error
        "ScavengeResp'ScavengeResult.succ: bad argument ScavengeResp'Stopped. This value would be out of bounds."
  succ ScavengeResp'Started = ScavengeResp'InProgress
  succ ScavengeResp'InProgress = ScavengeResp'Stopped
  succ (ScavengeResp'ScavengeResult'Unrecognized _)
    = Prelude.error
        "ScavengeResp'ScavengeResult.succ: bad argument: unrecognized value"
  pred ScavengeResp'Started
    = Prelude.error
        "ScavengeResp'ScavengeResult.pred: bad argument ScavengeResp'Started. This value would be out of bounds."
  pred ScavengeResp'InProgress = ScavengeResp'Started
  pred ScavengeResp'Stopped = ScavengeResp'InProgress
  pred (ScavengeResp'ScavengeResult'Unrecognized _)
    = Prelude.error
        "ScavengeResp'ScavengeResult.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault ScavengeResp'ScavengeResult where
  fieldDefault = ScavengeResp'Started
instance Control.DeepSeq.NFData ScavengeResp'ScavengeResult where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :
     
         * 'Proto.Operations_Fields.priority' @:: Lens' SetNodePriorityReq Data.Int.Int32@ -}
data SetNodePriorityReq
  = SetNodePriorityReq'_constructor {_SetNodePriorityReq'priority :: !Data.Int.Int32,
                                     _SetNodePriorityReq'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SetNodePriorityReq where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField SetNodePriorityReq "priority" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SetNodePriorityReq'priority
           (\ x__ y__ -> x__ {_SetNodePriorityReq'priority = y__}))
        Prelude.id
instance Data.ProtoLens.Message SetNodePriorityReq where
  messageName _
    = Data.Text.pack "event_store.client.operations.SetNodePriorityReq"
  packedMessageDescriptor _
    = "\n\
      \\DC2SetNodePriorityReq\DC2\SUB\n\
      \\bpriority\CAN\SOH \SOH(\ENQR\bpriority"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        priority__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "priority"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"priority")) ::
              Data.ProtoLens.FieldDescriptor SetNodePriorityReq
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, priority__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _SetNodePriorityReq'_unknownFields
        (\ x__ y__ -> x__ {_SetNodePriorityReq'_unknownFields = y__})
  defMessage
    = SetNodePriorityReq'_constructor
        {_SetNodePriorityReq'priority = Data.ProtoLens.fieldDefault,
         _SetNodePriorityReq'_unknownFields = []}
  parseMessage
    = let
        loop ::
          SetNodePriorityReq
          -> Data.ProtoLens.Encoding.Bytes.Parser SetNodePriorityReq
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
                                       "priority"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"priority") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "SetNodePriorityReq"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"priority") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData SetNodePriorityReq where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_SetNodePriorityReq'_unknownFields x__)
             (Control.DeepSeq.deepseq (_SetNodePriorityReq'priority x__) ())
{- | Fields :
     
         * 'Proto.Operations_Fields.options' @:: Lens' StartScavengeReq StartScavengeReq'Options@
         * 'Proto.Operations_Fields.maybe'options' @:: Lens' StartScavengeReq (Prelude.Maybe StartScavengeReq'Options)@ -}
data StartScavengeReq
  = StartScavengeReq'_constructor {_StartScavengeReq'options :: !(Prelude.Maybe StartScavengeReq'Options),
                                   _StartScavengeReq'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StartScavengeReq where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField StartScavengeReq "options" StartScavengeReq'Options where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StartScavengeReq'options
           (\ x__ y__ -> x__ {_StartScavengeReq'options = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField StartScavengeReq "maybe'options" (Prelude.Maybe StartScavengeReq'Options) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StartScavengeReq'options
           (\ x__ y__ -> x__ {_StartScavengeReq'options = y__}))
        Prelude.id
instance Data.ProtoLens.Message StartScavengeReq where
  messageName _
    = Data.Text.pack "event_store.client.operations.StartScavengeReq"
  packedMessageDescriptor _
    = "\n\
      \\DLEStartScavengeReq\DC2Q\n\
      \\aoptions\CAN\SOH \SOH(\v27.event_store.client.operations.StartScavengeReq.OptionsR\aoptions\SUBV\n\
      \\aOptions\DC2!\n\
      \\fthread_count\CAN\SOH \SOH(\ENQR\vthreadCount\DC2(\n\
      \\DLEstart_from_chunk\CAN\STX \SOH(\ENQR\SOstartFromChunk"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        options__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "options"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor StartScavengeReq'Options)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'options")) ::
              Data.ProtoLens.FieldDescriptor StartScavengeReq
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, options__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _StartScavengeReq'_unknownFields
        (\ x__ y__ -> x__ {_StartScavengeReq'_unknownFields = y__})
  defMessage
    = StartScavengeReq'_constructor
        {_StartScavengeReq'options = Prelude.Nothing,
         _StartScavengeReq'_unknownFields = []}
  parseMessage
    = let
        loop ::
          StartScavengeReq
          -> Data.ProtoLens.Encoding.Bytes.Parser StartScavengeReq
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
          (do loop Data.ProtoLens.defMessage) "StartScavengeReq"
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
instance Control.DeepSeq.NFData StartScavengeReq where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_StartScavengeReq'_unknownFields x__)
             (Control.DeepSeq.deepseq (_StartScavengeReq'options x__) ())
{- | Fields :
     
         * 'Proto.Operations_Fields.threadCount' @:: Lens' StartScavengeReq'Options Data.Int.Int32@
         * 'Proto.Operations_Fields.startFromChunk' @:: Lens' StartScavengeReq'Options Data.Int.Int32@ -}
data StartScavengeReq'Options
  = StartScavengeReq'Options'_constructor {_StartScavengeReq'Options'threadCount :: !Data.Int.Int32,
                                           _StartScavengeReq'Options'startFromChunk :: !Data.Int.Int32,
                                           _StartScavengeReq'Options'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StartScavengeReq'Options where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField StartScavengeReq'Options "threadCount" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StartScavengeReq'Options'threadCount
           (\ x__ y__ -> x__ {_StartScavengeReq'Options'threadCount = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StartScavengeReq'Options "startFromChunk" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StartScavengeReq'Options'startFromChunk
           (\ x__ y__
              -> x__ {_StartScavengeReq'Options'startFromChunk = y__}))
        Prelude.id
instance Data.ProtoLens.Message StartScavengeReq'Options where
  messageName _
    = Data.Text.pack
        "event_store.client.operations.StartScavengeReq.Options"
  packedMessageDescriptor _
    = "\n\
      \\aOptions\DC2!\n\
      \\fthread_count\CAN\SOH \SOH(\ENQR\vthreadCount\DC2(\n\
      \\DLEstart_from_chunk\CAN\STX \SOH(\ENQR\SOstartFromChunk"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        threadCount__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "thread_count"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"threadCount")) ::
              Data.ProtoLens.FieldDescriptor StartScavengeReq'Options
        startFromChunk__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "start_from_chunk"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"startFromChunk")) ::
              Data.ProtoLens.FieldDescriptor StartScavengeReq'Options
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, threadCount__field_descriptor),
           (Data.ProtoLens.Tag 2, startFromChunk__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _StartScavengeReq'Options'_unknownFields
        (\ x__ y__ -> x__ {_StartScavengeReq'Options'_unknownFields = y__})
  defMessage
    = StartScavengeReq'Options'_constructor
        {_StartScavengeReq'Options'threadCount = Data.ProtoLens.fieldDefault,
         _StartScavengeReq'Options'startFromChunk = Data.ProtoLens.fieldDefault,
         _StartScavengeReq'Options'_unknownFields = []}
  parseMessage
    = let
        loop ::
          StartScavengeReq'Options
          -> Data.ProtoLens.Encoding.Bytes.Parser StartScavengeReq'Options
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
                                       "thread_count"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"threadCount") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "start_from_chunk"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"startFromChunk") y x)
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
                _v
                  = Lens.Family2.view (Data.ProtoLens.Field.field @"threadCount") _x
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
                         (Data.ProtoLens.Field.field @"startFromChunk") _x
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
instance Control.DeepSeq.NFData StartScavengeReq'Options where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_StartScavengeReq'Options'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_StartScavengeReq'Options'threadCount x__)
                (Control.DeepSeq.deepseq
                   (_StartScavengeReq'Options'startFromChunk x__) ()))
{- | Fields :
     
         * 'Proto.Operations_Fields.options' @:: Lens' StopScavengeReq StopScavengeReq'Options@
         * 'Proto.Operations_Fields.maybe'options' @:: Lens' StopScavengeReq (Prelude.Maybe StopScavengeReq'Options)@ -}
data StopScavengeReq
  = StopScavengeReq'_constructor {_StopScavengeReq'options :: !(Prelude.Maybe StopScavengeReq'Options),
                                  _StopScavengeReq'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StopScavengeReq where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField StopScavengeReq "options" StopScavengeReq'Options where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StopScavengeReq'options
           (\ x__ y__ -> x__ {_StopScavengeReq'options = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField StopScavengeReq "maybe'options" (Prelude.Maybe StopScavengeReq'Options) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StopScavengeReq'options
           (\ x__ y__ -> x__ {_StopScavengeReq'options = y__}))
        Prelude.id
instance Data.ProtoLens.Message StopScavengeReq where
  messageName _
    = Data.Text.pack "event_store.client.operations.StopScavengeReq"
  packedMessageDescriptor _
    = "\n\
      \\SIStopScavengeReq\DC2P\n\
      \\aoptions\CAN\SOH \SOH(\v26.event_store.client.operations.StopScavengeReq.OptionsR\aoptions\SUB*\n\
      \\aOptions\DC2\US\n\
      \\vscavenge_id\CAN\SOH \SOH(\tR\n\
      \scavengeId"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        options__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "options"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor StopScavengeReq'Options)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'options")) ::
              Data.ProtoLens.FieldDescriptor StopScavengeReq
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, options__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _StopScavengeReq'_unknownFields
        (\ x__ y__ -> x__ {_StopScavengeReq'_unknownFields = y__})
  defMessage
    = StopScavengeReq'_constructor
        {_StopScavengeReq'options = Prelude.Nothing,
         _StopScavengeReq'_unknownFields = []}
  parseMessage
    = let
        loop ::
          StopScavengeReq
          -> Data.ProtoLens.Encoding.Bytes.Parser StopScavengeReq
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
          (do loop Data.ProtoLens.defMessage) "StopScavengeReq"
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
instance Control.DeepSeq.NFData StopScavengeReq where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_StopScavengeReq'_unknownFields x__)
             (Control.DeepSeq.deepseq (_StopScavengeReq'options x__) ())
{- | Fields :
     
         * 'Proto.Operations_Fields.scavengeId' @:: Lens' StopScavengeReq'Options Data.Text.Text@ -}
data StopScavengeReq'Options
  = StopScavengeReq'Options'_constructor {_StopScavengeReq'Options'scavengeId :: !Data.Text.Text,
                                          _StopScavengeReq'Options'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StopScavengeReq'Options where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField StopScavengeReq'Options "scavengeId" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StopScavengeReq'Options'scavengeId
           (\ x__ y__ -> x__ {_StopScavengeReq'Options'scavengeId = y__}))
        Prelude.id
instance Data.ProtoLens.Message StopScavengeReq'Options where
  messageName _
    = Data.Text.pack
        "event_store.client.operations.StopScavengeReq.Options"
  packedMessageDescriptor _
    = "\n\
      \\aOptions\DC2\US\n\
      \\vscavenge_id\CAN\SOH \SOH(\tR\n\
      \scavengeId"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        scavengeId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "scavenge_id"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"scavengeId")) ::
              Data.ProtoLens.FieldDescriptor StopScavengeReq'Options
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, scavengeId__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _StopScavengeReq'Options'_unknownFields
        (\ x__ y__ -> x__ {_StopScavengeReq'Options'_unknownFields = y__})
  defMessage
    = StopScavengeReq'Options'_constructor
        {_StopScavengeReq'Options'scavengeId = Data.ProtoLens.fieldDefault,
         _StopScavengeReq'Options'_unknownFields = []}
  parseMessage
    = let
        loop ::
          StopScavengeReq'Options
          -> Data.ProtoLens.Encoding.Bytes.Parser StopScavengeReq'Options
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
                                       "scavenge_id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"scavengeId") y x)
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
                _v
                  = Lens.Family2.view (Data.ProtoLens.Field.field @"scavengeId") _x
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
instance Control.DeepSeq.NFData StopScavengeReq'Options where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_StopScavengeReq'Options'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_StopScavengeReq'Options'scavengeId x__) ())
data Operations = Operations {}
instance Data.ProtoLens.Service.Types.Service Operations where
  type ServiceName Operations = "Operations"
  type ServicePackage Operations = "event_store.client.operations"
  type ServiceMethods Operations = '["mergeIndexes",
                                     "resignNode",
                                     "setNodePriority",
                                     "shutdown",
                                     "startScavenge",
                                     "stopScavenge"]
instance Data.ProtoLens.Service.Types.HasMethodImpl Operations "startScavenge" where
  type MethodName Operations "startScavenge" = "StartScavenge"
  type MethodInput Operations "startScavenge" = StartScavengeReq
  type MethodOutput Operations "startScavenge" = ScavengeResp
  type MethodStreamingType Operations "startScavenge" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Operations "stopScavenge" where
  type MethodName Operations "stopScavenge" = "StopScavenge"
  type MethodInput Operations "stopScavenge" = StopScavengeReq
  type MethodOutput Operations "stopScavenge" = ScavengeResp
  type MethodStreamingType Operations "stopScavenge" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Operations "shutdown" where
  type MethodName Operations "shutdown" = "Shutdown"
  type MethodInput Operations "shutdown" = Proto.Shared.Empty
  type MethodOutput Operations "shutdown" = Proto.Shared.Empty
  type MethodStreamingType Operations "shutdown" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Operations "mergeIndexes" where
  type MethodName Operations "mergeIndexes" = "MergeIndexes"
  type MethodInput Operations "mergeIndexes" = Proto.Shared.Empty
  type MethodOutput Operations "mergeIndexes" = Proto.Shared.Empty
  type MethodStreamingType Operations "mergeIndexes" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Operations "resignNode" where
  type MethodName Operations "resignNode" = "ResignNode"
  type MethodInput Operations "resignNode" = Proto.Shared.Empty
  type MethodOutput Operations "resignNode" = Proto.Shared.Empty
  type MethodStreamingType Operations "resignNode" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Operations "setNodePriority" where
  type MethodName Operations "setNodePriority" = "SetNodePriority"
  type MethodInput Operations "setNodePriority" = SetNodePriorityReq
  type MethodOutput Operations "setNodePriority" = Proto.Shared.Empty
  type MethodStreamingType Operations "setNodePriority" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\DLEoperations.proto\DC2\GSevent_store.client.operations\SUB\fshared.proto\"\189\SOH\n\
    \\DLEStartScavengeReq\DC2Q\n\
    \\aoptions\CAN\SOH \SOH(\v27.event_store.client.operations.StartScavengeReq.OptionsR\aoptions\SUBV\n\
    \\aOptions\DC2!\n\
    \\fthread_count\CAN\SOH \SOH(\ENQR\vthreadCount\DC2(\n\
    \\DLEstart_from_chunk\CAN\STX \SOH(\ENQR\SOstartFromChunk\"\143\SOH\n\
    \\SIStopScavengeReq\DC2P\n\
    \\aoptions\CAN\SOH \SOH(\v26.event_store.client.operations.StopScavengeReq.OptionsR\aoptions\SUB*\n\
    \\aOptions\DC2\US\n\
    \\vscavenge_id\CAN\SOH \SOH(\tR\n\
    \scavengeId\"\208\SOH\n\
    \\fScavengeResp\DC2\US\n\
    \\vscavenge_id\CAN\SOH \SOH(\tR\n\
    \scavengeId\DC2c\n\
    \\SIscavenge_result\CAN\STX \SOH(\SO2:.event_store.client.operations.ScavengeResp.ScavengeResultR\SOscavengeResult\":\n\
    \\SOScavengeResult\DC2\v\n\
    \\aStarted\DLE\NUL\DC2\SO\n\
    \\n\
    \InProgress\DLE\SOH\DC2\v\n\
    \\aStopped\DLE\STX\"0\n\
    \\DC2SetNodePriorityReq\DC2\SUB\n\
    \\bpriority\CAN\SOH \SOH(\ENQR\bpriority2\198\EOT\n\
    \\n\
    \Operations\DC2m\n\
    \\rStartScavenge\DC2/.event_store.client.operations.StartScavengeReq\SUB+.event_store.client.operations.ScavengeResp\DC2k\n\
    \\fStopScavenge\DC2..event_store.client.operations.StopScavengeReq\SUB+.event_store.client.operations.ScavengeResp\DC2N\n\
    \\bShutdown\DC2 .event_store.client.shared.Empty\SUB .event_store.client.shared.Empty\DC2R\n\
    \\fMergeIndexes\DC2 .event_store.client.shared.Empty\SUB .event_store.client.shared.Empty\DC2P\n\
    \\n\
    \ResignNode\DC2 .event_store.client.shared.Empty\SUB .event_store.client.shared.Empty\DC2f\n\
    \\SISetNodePriority\DC21.event_store.client.operations.SetNodePriorityReq\SUB .event_store.client.shared.EmptyB*\n\
    \(com.eventstore.dbclient.proto.operationsJ\175\t\n\
    \\ACK\DC2\EOT\NUL\NUL+\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\SOH\NUL&\n\
    \\b\n\
    \\SOH\b\DC2\ETX\STX\NULA\n\
    \\t\n\
    \\STX\b\SOH\DC2\ETX\STX\NULA\n\
    \\t\n\
    \\STX\ETX\NUL\DC2\ETX\EOT\NUL\SYN\n\
    \\n\
    \\n\
    \\STX\ACK\NUL\DC2\EOT\ACK\NUL\r\SOH\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX\ACK\b\DC2\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETX\a\bD\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX\a\f\EM\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX\a\ESC+\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX\a6B\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\SOH\DC2\ETX\b\bB\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\SOH\DC2\ETX\b\f\CAN\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\STX\DC2\ETX\b\SUB)\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\ETX\DC2\ETX\b4@\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\STX\DC2\ETX\t\ba\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\SOH\DC2\ETX\t\f\DC4\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\STX\DC2\ETX\t\SYN5\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\ETX\DC2\ETX\t@_\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\ETX\DC2\ETX\n\
    \\be\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\SOH\DC2\ETX\n\
    \\f\CAN\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\STX\DC2\ETX\n\
    \\SUB9\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\ETX\DC2\ETX\n\
    \Dc\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\EOT\DC2\ETX\v\bc\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\EOT\SOH\DC2\ETX\v\f\SYN\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\EOT\STX\DC2\ETX\v\CAN7\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\EOT\ETX\DC2\ETX\vBa\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\ENQ\DC2\ETX\f\b[\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ENQ\SOH\DC2\ETX\f\f\ESC\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ENQ\STX\DC2\ETX\f\GS/\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ENQ\ETX\DC2\ETX\f:Y\n\
    \\n\
    \\n\
    \\STX\EOT\NUL\DC2\EOT\SI\NUL\NAK\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\SI\b\CAN\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\DLE\b\FS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ACK\DC2\ETX\DLE\b\SI\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\DLE\DLE\ETB\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\DLE\SUB\ESC\n\
    \\f\n\
    \\EOT\EOT\NUL\ETX\NUL\DC2\EOT\DC1\b\DC4\t\n\
    \\f\n\
    \\ENQ\EOT\NUL\ETX\NUL\SOH\DC2\ETX\DC1\DLE\ETB\n\
    \\r\n\
    \\ACK\EOT\NUL\ETX\NUL\STX\NUL\DC2\ETX\DC2\DLE'\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\NUL\ENQ\DC2\ETX\DC2\DLE\NAK\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\NUL\SOH\DC2\ETX\DC2\SYN\"\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\NUL\ETX\DC2\ETX\DC2%&\n\
    \\r\n\
    \\ACK\EOT\NUL\ETX\NUL\STX\SOH\DC2\ETX\DC3\DLE+\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\SOH\ENQ\DC2\ETX\DC3\DLE\NAK\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\SOH\SOH\DC2\ETX\DC3\SYN&\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\SOH\ETX\DC2\ETX\DC3)*\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT\ETB\NUL\FS\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\ETB\b\ETB\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\CAN\b\FS\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ACK\DC2\ETX\CAN\b\SI\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\CAN\DLE\ETB\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\CAN\SUB\ESC\n\
    \\f\n\
    \\EOT\EOT\SOH\ETX\NUL\DC2\EOT\EM\b\ESC\t\n\
    \\f\n\
    \\ENQ\EOT\SOH\ETX\NUL\SOH\DC2\ETX\EM\DLE\ETB\n\
    \\r\n\
    \\ACK\EOT\SOH\ETX\NUL\STX\NUL\DC2\ETX\SUB\DLE'\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\NUL\ENQ\DC2\ETX\SUB\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\NUL\SOH\DC2\ETX\SUB\ETB\"\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\NUL\ETX\DC2\ETX\SUB%&\n\
    \\n\
    \\n\
    \\STX\EOT\STX\DC2\EOT\RS\NUL'\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX\RS\b\DC4\n\
    \\v\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX\US\b\US\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ENQ\DC2\ETX\US\b\SO\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX\US\SI\SUB\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX\US\GS\RS\n\
    \\v\n\
    \\EOT\EOT\STX\STX\SOH\DC2\ETX \b+\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ACK\DC2\ETX \b\SYN\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\SOH\DC2\ETX \ETB&\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ETX\DC2\ETX )*\n\
    \\f\n\
    \\EOT\EOT\STX\EOT\NUL\DC2\EOT\"\b&\t\n\
    \\f\n\
    \\ENQ\EOT\STX\EOT\NUL\SOH\DC2\ETX\"\r\ESC\n\
    \\r\n\
    \\ACK\EOT\STX\EOT\NUL\STX\NUL\DC2\ETX#\DLE\FS\n\
    \\SO\n\
    \\a\EOT\STX\EOT\NUL\STX\NUL\SOH\DC2\ETX#\DLE\ETB\n\
    \\SO\n\
    \\a\EOT\STX\EOT\NUL\STX\NUL\STX\DC2\ETX#\SUB\ESC\n\
    \\r\n\
    \\ACK\EOT\STX\EOT\NUL\STX\SOH\DC2\ETX$\DLE\US\n\
    \\SO\n\
    \\a\EOT\STX\EOT\NUL\STX\SOH\SOH\DC2\ETX$\DLE\SUB\n\
    \\SO\n\
    \\a\EOT\STX\EOT\NUL\STX\SOH\STX\DC2\ETX$\GS\RS\n\
    \\r\n\
    \\ACK\EOT\STX\EOT\NUL\STX\STX\DC2\ETX%\DLE\FS\n\
    \\SO\n\
    \\a\EOT\STX\EOT\NUL\STX\STX\SOH\DC2\ETX%\DLE\ETB\n\
    \\SO\n\
    \\a\EOT\STX\EOT\NUL\STX\STX\STX\DC2\ETX%\SUB\ESC\n\
    \\n\
    \\n\
    \\STX\EOT\ETX\DC2\EOT)\NUL+\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETX)\b\SUB\n\
    \\v\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\ETX*\b\ESC\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ENQ\DC2\ETX*\b\r\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\ETX*\SO\SYN\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\ETX*\EM\SUBb\ACKproto3"