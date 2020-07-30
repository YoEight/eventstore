{- This file was auto-generated from users.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Users (
        Users(..), ChangePasswordReq(), ChangePasswordReq'Options(),
        ChangePasswordResp(), CreateReq(), CreateReq'Options(),
        CreateResp(), DeleteReq(), DeleteReq'Options(), DeleteResp(),
        DetailsReq(), DetailsReq'Options(), DetailsResp(),
        DetailsResp'UserDetails(), DetailsResp'UserDetails'DateTime(),
        DisableReq(), DisableReq'Options(), DisableResp(), EnableReq(),
        EnableReq'Options(), EnableResp(), ResetPasswordReq(),
        ResetPasswordReq'Options(), ResetPasswordResp(), UpdateReq(),
        UpdateReq'Options(), UpdateResp()
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
     
         * 'Proto.Users_Fields.options' @:: Lens' ChangePasswordReq ChangePasswordReq'Options@
         * 'Proto.Users_Fields.maybe'options' @:: Lens' ChangePasswordReq (Prelude.Maybe ChangePasswordReq'Options)@ -}
data ChangePasswordReq
  = ChangePasswordReq'_constructor {_ChangePasswordReq'options :: !(Prelude.Maybe ChangePasswordReq'Options),
                                    _ChangePasswordReq'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ChangePasswordReq where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ChangePasswordReq "options" ChangePasswordReq'Options where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ChangePasswordReq'options
           (\ x__ y__ -> x__ {_ChangePasswordReq'options = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ChangePasswordReq "maybe'options" (Prelude.Maybe ChangePasswordReq'Options) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ChangePasswordReq'options
           (\ x__ y__ -> x__ {_ChangePasswordReq'options = y__}))
        Prelude.id
instance Data.ProtoLens.Message ChangePasswordReq where
  messageName _
    = Data.Text.pack "event_store.client.users.ChangePasswordReq"
  packedMessageDescriptor _
    = "\n\
      \\DC1ChangePasswordReq\DC2M\n\
      \\aoptions\CAN\SOH \SOH(\v23.event_store.client.users.ChangePasswordReq.OptionsR\aoptions\SUBv\n\
      \\aOptions\DC2\GS\n\
      \\n\
      \login_name\CAN\SOH \SOH(\tR\tloginName\DC2)\n\
      \\DLEcurrent_password\CAN\STX \SOH(\tR\SIcurrentPassword\DC2!\n\
      \\fnew_password\CAN\ETX \SOH(\tR\vnewPassword"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        options__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "options"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ChangePasswordReq'Options)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'options")) ::
              Data.ProtoLens.FieldDescriptor ChangePasswordReq
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, options__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ChangePasswordReq'_unknownFields
        (\ x__ y__ -> x__ {_ChangePasswordReq'_unknownFields = y__})
  defMessage
    = ChangePasswordReq'_constructor
        {_ChangePasswordReq'options = Prelude.Nothing,
         _ChangePasswordReq'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ChangePasswordReq
          -> Data.ProtoLens.Encoding.Bytes.Parser ChangePasswordReq
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
          (do loop Data.ProtoLens.defMessage) "ChangePasswordReq"
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
instance Control.DeepSeq.NFData ChangePasswordReq where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ChangePasswordReq'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ChangePasswordReq'options x__) ())
{- | Fields :
     
         * 'Proto.Users_Fields.loginName' @:: Lens' ChangePasswordReq'Options Data.Text.Text@
         * 'Proto.Users_Fields.currentPassword' @:: Lens' ChangePasswordReq'Options Data.Text.Text@
         * 'Proto.Users_Fields.newPassword' @:: Lens' ChangePasswordReq'Options Data.Text.Text@ -}
data ChangePasswordReq'Options
  = ChangePasswordReq'Options'_constructor {_ChangePasswordReq'Options'loginName :: !Data.Text.Text,
                                            _ChangePasswordReq'Options'currentPassword :: !Data.Text.Text,
                                            _ChangePasswordReq'Options'newPassword :: !Data.Text.Text,
                                            _ChangePasswordReq'Options'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ChangePasswordReq'Options where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ChangePasswordReq'Options "loginName" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ChangePasswordReq'Options'loginName
           (\ x__ y__ -> x__ {_ChangePasswordReq'Options'loginName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ChangePasswordReq'Options "currentPassword" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ChangePasswordReq'Options'currentPassword
           (\ x__ y__
              -> x__ {_ChangePasswordReq'Options'currentPassword = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ChangePasswordReq'Options "newPassword" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ChangePasswordReq'Options'newPassword
           (\ x__ y__ -> x__ {_ChangePasswordReq'Options'newPassword = y__}))
        Prelude.id
instance Data.ProtoLens.Message ChangePasswordReq'Options where
  messageName _
    = Data.Text.pack
        "event_store.client.users.ChangePasswordReq.Options"
  packedMessageDescriptor _
    = "\n\
      \\aOptions\DC2\GS\n\
      \\n\
      \login_name\CAN\SOH \SOH(\tR\tloginName\DC2)\n\
      \\DLEcurrent_password\CAN\STX \SOH(\tR\SIcurrentPassword\DC2!\n\
      \\fnew_password\CAN\ETX \SOH(\tR\vnewPassword"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        loginName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "login_name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"loginName")) ::
              Data.ProtoLens.FieldDescriptor ChangePasswordReq'Options
        currentPassword__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "current_password"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"currentPassword")) ::
              Data.ProtoLens.FieldDescriptor ChangePasswordReq'Options
        newPassword__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "new_password"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"newPassword")) ::
              Data.ProtoLens.FieldDescriptor ChangePasswordReq'Options
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, loginName__field_descriptor),
           (Data.ProtoLens.Tag 2, currentPassword__field_descriptor),
           (Data.ProtoLens.Tag 3, newPassword__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ChangePasswordReq'Options'_unknownFields
        (\ x__ y__
           -> x__ {_ChangePasswordReq'Options'_unknownFields = y__})
  defMessage
    = ChangePasswordReq'Options'_constructor
        {_ChangePasswordReq'Options'loginName = Data.ProtoLens.fieldDefault,
         _ChangePasswordReq'Options'currentPassword = Data.ProtoLens.fieldDefault,
         _ChangePasswordReq'Options'newPassword = Data.ProtoLens.fieldDefault,
         _ChangePasswordReq'Options'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ChangePasswordReq'Options
          -> Data.ProtoLens.Encoding.Bytes.Parser ChangePasswordReq'Options
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
                                       "login_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"loginName") y x)
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
                                       "current_password"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"currentPassword") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "new_password"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"newPassword") y x)
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
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"loginName") _x
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
                         (Data.ProtoLens.Field.field @"currentPassword") _x
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
                   (let
                      _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"newPassword") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                            ((Prelude..)
                               (\ bs
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                          (Prelude.fromIntegral (Data.ByteString.length bs)))
                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                               Data.Text.Encoding.encodeUtf8
                               _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData ChangePasswordReq'Options where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ChangePasswordReq'Options'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ChangePasswordReq'Options'loginName x__)
                (Control.DeepSeq.deepseq
                   (_ChangePasswordReq'Options'currentPassword x__)
                   (Control.DeepSeq.deepseq
                      (_ChangePasswordReq'Options'newPassword x__) ())))
{- | Fields :
      -}
data ChangePasswordResp
  = ChangePasswordResp'_constructor {_ChangePasswordResp'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ChangePasswordResp where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message ChangePasswordResp where
  messageName _
    = Data.Text.pack "event_store.client.users.ChangePasswordResp"
  packedMessageDescriptor _
    = "\n\
      \\DC2ChangePasswordResp"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ChangePasswordResp'_unknownFields
        (\ x__ y__ -> x__ {_ChangePasswordResp'_unknownFields = y__})
  defMessage
    = ChangePasswordResp'_constructor
        {_ChangePasswordResp'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ChangePasswordResp
          -> Data.ProtoLens.Encoding.Bytes.Parser ChangePasswordResp
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
          (do loop Data.ProtoLens.defMessage) "ChangePasswordResp"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData ChangePasswordResp where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ChangePasswordResp'_unknownFields x__) ()
{- | Fields :
     
         * 'Proto.Users_Fields.options' @:: Lens' CreateReq CreateReq'Options@
         * 'Proto.Users_Fields.maybe'options' @:: Lens' CreateReq (Prelude.Maybe CreateReq'Options)@ -}
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
  messageName _ = Data.Text.pack "event_store.client.users.CreateReq"
  packedMessageDescriptor _
    = "\n\
      \\tCreateReq\DC2E\n\
      \\aoptions\CAN\SOH \SOH(\v2+.event_store.client.users.CreateReq.OptionsR\aoptions\SUBy\n\
      \\aOptions\DC2\GS\n\
      \\n\
      \login_name\CAN\SOH \SOH(\tR\tloginName\DC2\SUB\n\
      \\bpassword\CAN\STX \SOH(\tR\bpassword\DC2\ESC\n\
      \\tfull_name\CAN\ETX \SOH(\tR\bfullName\DC2\SYN\n\
      \\ACKgroups\CAN\EOT \ETX(\tR\ACKgroups"
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
     
         * 'Proto.Users_Fields.loginName' @:: Lens' CreateReq'Options Data.Text.Text@
         * 'Proto.Users_Fields.password' @:: Lens' CreateReq'Options Data.Text.Text@
         * 'Proto.Users_Fields.fullName' @:: Lens' CreateReq'Options Data.Text.Text@
         * 'Proto.Users_Fields.groups' @:: Lens' CreateReq'Options [Data.Text.Text]@
         * 'Proto.Users_Fields.vec'groups' @:: Lens' CreateReq'Options (Data.Vector.Vector Data.Text.Text)@ -}
data CreateReq'Options
  = CreateReq'Options'_constructor {_CreateReq'Options'loginName :: !Data.Text.Text,
                                    _CreateReq'Options'password :: !Data.Text.Text,
                                    _CreateReq'Options'fullName :: !Data.Text.Text,
                                    _CreateReq'Options'groups :: !(Data.Vector.Vector Data.Text.Text),
                                    _CreateReq'Options'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show CreateReq'Options where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField CreateReq'Options "loginName" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CreateReq'Options'loginName
           (\ x__ y__ -> x__ {_CreateReq'Options'loginName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField CreateReq'Options "password" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CreateReq'Options'password
           (\ x__ y__ -> x__ {_CreateReq'Options'password = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField CreateReq'Options "fullName" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CreateReq'Options'fullName
           (\ x__ y__ -> x__ {_CreateReq'Options'fullName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField CreateReq'Options "groups" [Data.Text.Text] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CreateReq'Options'groups
           (\ x__ y__ -> x__ {_CreateReq'Options'groups = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField CreateReq'Options "vec'groups" (Data.Vector.Vector Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CreateReq'Options'groups
           (\ x__ y__ -> x__ {_CreateReq'Options'groups = y__}))
        Prelude.id
instance Data.ProtoLens.Message CreateReq'Options where
  messageName _
    = Data.Text.pack "event_store.client.users.CreateReq.Options"
  packedMessageDescriptor _
    = "\n\
      \\aOptions\DC2\GS\n\
      \\n\
      \login_name\CAN\SOH \SOH(\tR\tloginName\DC2\SUB\n\
      \\bpassword\CAN\STX \SOH(\tR\bpassword\DC2\ESC\n\
      \\tfull_name\CAN\ETX \SOH(\tR\bfullName\DC2\SYN\n\
      \\ACKgroups\CAN\EOT \ETX(\tR\ACKgroups"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        loginName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "login_name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"loginName")) ::
              Data.ProtoLens.FieldDescriptor CreateReq'Options
        password__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "password"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"password")) ::
              Data.ProtoLens.FieldDescriptor CreateReq'Options
        fullName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "full_name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"fullName")) ::
              Data.ProtoLens.FieldDescriptor CreateReq'Options
        groups__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "groups"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"groups")) ::
              Data.ProtoLens.FieldDescriptor CreateReq'Options
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, loginName__field_descriptor),
           (Data.ProtoLens.Tag 2, password__field_descriptor),
           (Data.ProtoLens.Tag 3, fullName__field_descriptor),
           (Data.ProtoLens.Tag 4, groups__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _CreateReq'Options'_unknownFields
        (\ x__ y__ -> x__ {_CreateReq'Options'_unknownFields = y__})
  defMessage
    = CreateReq'Options'_constructor
        {_CreateReq'Options'loginName = Data.ProtoLens.fieldDefault,
         _CreateReq'Options'password = Data.ProtoLens.fieldDefault,
         _CreateReq'Options'fullName = Data.ProtoLens.fieldDefault,
         _CreateReq'Options'groups = Data.Vector.Generic.empty,
         _CreateReq'Options'_unknownFields = []}
  parseMessage
    = let
        loop ::
          CreateReq'Options
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Text.Text
             -> Data.ProtoLens.Encoding.Bytes.Parser CreateReq'Options
        loop x mutable'groups
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'groups <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                         (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                            mutable'groups)
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
                              (Data.ProtoLens.Field.field @"vec'groups") frozen'groups x))
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
                                       "login_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"loginName") y x)
                                  mutable'groups
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
                                       "password"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"password") y x)
                                  mutable'groups
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "full_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"fullName") y x)
                                  mutable'groups
                        34
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                        Data.ProtoLens.Encoding.Bytes.getBytes
                                                          (Prelude.fromIntegral len)
                                            Data.ProtoLens.Encoding.Bytes.runEither
                                              (case Data.Text.Encoding.decodeUtf8' value of
                                                 (Prelude.Left err)
                                                   -> Prelude.Left (Prelude.show err)
                                                 (Prelude.Right r) -> Prelude.Right r))
                                        "groups"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'groups y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'groups
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'groups <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                  Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'groups)
          "Options"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"loginName") _x
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
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"password") _x
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
                   (let
                      _v = Lens.Family2.view (Data.ProtoLens.Field.field @"fullName") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                            ((Prelude..)
                               (\ bs
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                          (Prelude.fromIntegral (Data.ByteString.length bs)))
                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                               Data.Text.Encoding.encodeUtf8
                               _v))
                   ((Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                         (\ _v
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                 ((Prelude..)
                                    (\ bs
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                               (Prelude.fromIntegral (Data.ByteString.length bs)))
                                            (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                    Data.Text.Encoding.encodeUtf8
                                    _v))
                         (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'groups") _x))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData CreateReq'Options where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_CreateReq'Options'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_CreateReq'Options'loginName x__)
                (Control.DeepSeq.deepseq
                   (_CreateReq'Options'password x__)
                   (Control.DeepSeq.deepseq
                      (_CreateReq'Options'fullName x__)
                      (Control.DeepSeq.deepseq (_CreateReq'Options'groups x__) ()))))
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
    = Data.Text.pack "event_store.client.users.CreateResp"
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
     
         * 'Proto.Users_Fields.options' @:: Lens' DeleteReq DeleteReq'Options@
         * 'Proto.Users_Fields.maybe'options' @:: Lens' DeleteReq (Prelude.Maybe DeleteReq'Options)@ -}
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
  messageName _ = Data.Text.pack "event_store.client.users.DeleteReq"
  packedMessageDescriptor _
    = "\n\
      \\tDeleteReq\DC2E\n\
      \\aoptions\CAN\SOH \SOH(\v2+.event_store.client.users.DeleteReq.OptionsR\aoptions\SUB(\n\
      \\aOptions\DC2\GS\n\
      \\n\
      \login_name\CAN\SOH \SOH(\tR\tloginName"
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
     
         * 'Proto.Users_Fields.loginName' @:: Lens' DeleteReq'Options Data.Text.Text@ -}
data DeleteReq'Options
  = DeleteReq'Options'_constructor {_DeleteReq'Options'loginName :: !Data.Text.Text,
                                    _DeleteReq'Options'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DeleteReq'Options where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField DeleteReq'Options "loginName" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DeleteReq'Options'loginName
           (\ x__ y__ -> x__ {_DeleteReq'Options'loginName = y__}))
        Prelude.id
instance Data.ProtoLens.Message DeleteReq'Options where
  messageName _
    = Data.Text.pack "event_store.client.users.DeleteReq.Options"
  packedMessageDescriptor _
    = "\n\
      \\aOptions\DC2\GS\n\
      \\n\
      \login_name\CAN\SOH \SOH(\tR\tloginName"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        loginName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "login_name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"loginName")) ::
              Data.ProtoLens.FieldDescriptor DeleteReq'Options
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, loginName__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DeleteReq'Options'_unknownFields
        (\ x__ y__ -> x__ {_DeleteReq'Options'_unknownFields = y__})
  defMessage
    = DeleteReq'Options'_constructor
        {_DeleteReq'Options'loginName = Data.ProtoLens.fieldDefault,
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
                                       "login_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"loginName") y x)
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
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"loginName") _x
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
instance Control.DeepSeq.NFData DeleteReq'Options where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DeleteReq'Options'_unknownFields x__)
             (Control.DeepSeq.deepseq (_DeleteReq'Options'loginName x__) ())
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
    = Data.Text.pack "event_store.client.users.DeleteResp"
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
     
         * 'Proto.Users_Fields.options' @:: Lens' DetailsReq DetailsReq'Options@
         * 'Proto.Users_Fields.maybe'options' @:: Lens' DetailsReq (Prelude.Maybe DetailsReq'Options)@ -}
data DetailsReq
  = DetailsReq'_constructor {_DetailsReq'options :: !(Prelude.Maybe DetailsReq'Options),
                             _DetailsReq'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DetailsReq where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField DetailsReq "options" DetailsReq'Options where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DetailsReq'options (\ x__ y__ -> x__ {_DetailsReq'options = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField DetailsReq "maybe'options" (Prelude.Maybe DetailsReq'Options) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DetailsReq'options (\ x__ y__ -> x__ {_DetailsReq'options = y__}))
        Prelude.id
instance Data.ProtoLens.Message DetailsReq where
  messageName _
    = Data.Text.pack "event_store.client.users.DetailsReq"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \DetailsReq\DC2F\n\
      \\aoptions\CAN\SOH \SOH(\v2,.event_store.client.users.DetailsReq.OptionsR\aoptions\SUB(\n\
      \\aOptions\DC2\GS\n\
      \\n\
      \login_name\CAN\SOH \SOH(\tR\tloginName"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        options__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "options"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor DetailsReq'Options)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'options")) ::
              Data.ProtoLens.FieldDescriptor DetailsReq
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, options__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DetailsReq'_unknownFields
        (\ x__ y__ -> x__ {_DetailsReq'_unknownFields = y__})
  defMessage
    = DetailsReq'_constructor
        {_DetailsReq'options = Prelude.Nothing,
         _DetailsReq'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DetailsReq -> Data.ProtoLens.Encoding.Bytes.Parser DetailsReq
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
          (do loop Data.ProtoLens.defMessage) "DetailsReq"
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
instance Control.DeepSeq.NFData DetailsReq where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DetailsReq'_unknownFields x__)
             (Control.DeepSeq.deepseq (_DetailsReq'options x__) ())
{- | Fields :
     
         * 'Proto.Users_Fields.loginName' @:: Lens' DetailsReq'Options Data.Text.Text@ -}
data DetailsReq'Options
  = DetailsReq'Options'_constructor {_DetailsReq'Options'loginName :: !Data.Text.Text,
                                     _DetailsReq'Options'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DetailsReq'Options where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField DetailsReq'Options "loginName" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DetailsReq'Options'loginName
           (\ x__ y__ -> x__ {_DetailsReq'Options'loginName = y__}))
        Prelude.id
instance Data.ProtoLens.Message DetailsReq'Options where
  messageName _
    = Data.Text.pack "event_store.client.users.DetailsReq.Options"
  packedMessageDescriptor _
    = "\n\
      \\aOptions\DC2\GS\n\
      \\n\
      \login_name\CAN\SOH \SOH(\tR\tloginName"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        loginName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "login_name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"loginName")) ::
              Data.ProtoLens.FieldDescriptor DetailsReq'Options
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, loginName__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DetailsReq'Options'_unknownFields
        (\ x__ y__ -> x__ {_DetailsReq'Options'_unknownFields = y__})
  defMessage
    = DetailsReq'Options'_constructor
        {_DetailsReq'Options'loginName = Data.ProtoLens.fieldDefault,
         _DetailsReq'Options'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DetailsReq'Options
          -> Data.ProtoLens.Encoding.Bytes.Parser DetailsReq'Options
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
                                       "login_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"loginName") y x)
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
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"loginName") _x
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
instance Control.DeepSeq.NFData DetailsReq'Options where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DetailsReq'Options'_unknownFields x__)
             (Control.DeepSeq.deepseq (_DetailsReq'Options'loginName x__) ())
{- | Fields :
     
         * 'Proto.Users_Fields.userDetails' @:: Lens' DetailsResp DetailsResp'UserDetails@
         * 'Proto.Users_Fields.maybe'userDetails' @:: Lens' DetailsResp (Prelude.Maybe DetailsResp'UserDetails)@ -}
data DetailsResp
  = DetailsResp'_constructor {_DetailsResp'userDetails :: !(Prelude.Maybe DetailsResp'UserDetails),
                              _DetailsResp'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DetailsResp where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField DetailsResp "userDetails" DetailsResp'UserDetails where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DetailsResp'userDetails
           (\ x__ y__ -> x__ {_DetailsResp'userDetails = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField DetailsResp "maybe'userDetails" (Prelude.Maybe DetailsResp'UserDetails) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DetailsResp'userDetails
           (\ x__ y__ -> x__ {_DetailsResp'userDetails = y__}))
        Prelude.id
instance Data.ProtoLens.Message DetailsResp where
  messageName _
    = Data.Text.pack "event_store.client.users.DetailsResp"
  packedMessageDescriptor _
    = "\n\
      \\vDetailsResp\DC2T\n\
      \\fuser_details\CAN\SOH \SOH(\v21.event_store.client.users.DetailsResp.UserDetailsR\vuserDetails\SUB\148\STX\n\
      \\vUserDetails\DC2\GS\n\
      \\n\
      \login_name\CAN\SOH \SOH(\tR\tloginName\DC2\ESC\n\
      \\tfull_name\CAN\STX \SOH(\tR\bfullName\DC2\SYN\n\
      \\ACKgroups\CAN\ETX \ETX(\tR\ACKgroups\DC2]\n\
      \\flast_updated\CAN\EOT \SOH(\v2:.event_store.client.users.DetailsResp.UserDetails.DateTimeR\vlastUpdated\DC2\SUB\n\
      \\bdisabled\CAN\ENQ \SOH(\bR\bdisabled\SUB6\n\
      \\bDateTime\DC2*\n\
      \\DC1ticks_since_epoch\CAN\SOH \SOH(\ETXR\SIticksSinceEpoch"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        userDetails__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "user_details"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor DetailsResp'UserDetails)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'userDetails")) ::
              Data.ProtoLens.FieldDescriptor DetailsResp
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, userDetails__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DetailsResp'_unknownFields
        (\ x__ y__ -> x__ {_DetailsResp'_unknownFields = y__})
  defMessage
    = DetailsResp'_constructor
        {_DetailsResp'userDetails = Prelude.Nothing,
         _DetailsResp'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DetailsResp -> Data.ProtoLens.Encoding.Bytes.Parser DetailsResp
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
                                       "user_details"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"userDetails") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "DetailsResp"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'userDetails") _x
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
instance Control.DeepSeq.NFData DetailsResp where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DetailsResp'_unknownFields x__)
             (Control.DeepSeq.deepseq (_DetailsResp'userDetails x__) ())
{- | Fields :
     
         * 'Proto.Users_Fields.loginName' @:: Lens' DetailsResp'UserDetails Data.Text.Text@
         * 'Proto.Users_Fields.fullName' @:: Lens' DetailsResp'UserDetails Data.Text.Text@
         * 'Proto.Users_Fields.groups' @:: Lens' DetailsResp'UserDetails [Data.Text.Text]@
         * 'Proto.Users_Fields.vec'groups' @:: Lens' DetailsResp'UserDetails (Data.Vector.Vector Data.Text.Text)@
         * 'Proto.Users_Fields.lastUpdated' @:: Lens' DetailsResp'UserDetails DetailsResp'UserDetails'DateTime@
         * 'Proto.Users_Fields.maybe'lastUpdated' @:: Lens' DetailsResp'UserDetails (Prelude.Maybe DetailsResp'UserDetails'DateTime)@
         * 'Proto.Users_Fields.disabled' @:: Lens' DetailsResp'UserDetails Prelude.Bool@ -}
data DetailsResp'UserDetails
  = DetailsResp'UserDetails'_constructor {_DetailsResp'UserDetails'loginName :: !Data.Text.Text,
                                          _DetailsResp'UserDetails'fullName :: !Data.Text.Text,
                                          _DetailsResp'UserDetails'groups :: !(Data.Vector.Vector Data.Text.Text),
                                          _DetailsResp'UserDetails'lastUpdated :: !(Prelude.Maybe DetailsResp'UserDetails'DateTime),
                                          _DetailsResp'UserDetails'disabled :: !Prelude.Bool,
                                          _DetailsResp'UserDetails'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DetailsResp'UserDetails where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField DetailsResp'UserDetails "loginName" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DetailsResp'UserDetails'loginName
           (\ x__ y__ -> x__ {_DetailsResp'UserDetails'loginName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField DetailsResp'UserDetails "fullName" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DetailsResp'UserDetails'fullName
           (\ x__ y__ -> x__ {_DetailsResp'UserDetails'fullName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField DetailsResp'UserDetails "groups" [Data.Text.Text] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DetailsResp'UserDetails'groups
           (\ x__ y__ -> x__ {_DetailsResp'UserDetails'groups = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField DetailsResp'UserDetails "vec'groups" (Data.Vector.Vector Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DetailsResp'UserDetails'groups
           (\ x__ y__ -> x__ {_DetailsResp'UserDetails'groups = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField DetailsResp'UserDetails "lastUpdated" DetailsResp'UserDetails'DateTime where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DetailsResp'UserDetails'lastUpdated
           (\ x__ y__ -> x__ {_DetailsResp'UserDetails'lastUpdated = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField DetailsResp'UserDetails "maybe'lastUpdated" (Prelude.Maybe DetailsResp'UserDetails'DateTime) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DetailsResp'UserDetails'lastUpdated
           (\ x__ y__ -> x__ {_DetailsResp'UserDetails'lastUpdated = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField DetailsResp'UserDetails "disabled" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DetailsResp'UserDetails'disabled
           (\ x__ y__ -> x__ {_DetailsResp'UserDetails'disabled = y__}))
        Prelude.id
instance Data.ProtoLens.Message DetailsResp'UserDetails where
  messageName _
    = Data.Text.pack "event_store.client.users.DetailsResp.UserDetails"
  packedMessageDescriptor _
    = "\n\
      \\vUserDetails\DC2\GS\n\
      \\n\
      \login_name\CAN\SOH \SOH(\tR\tloginName\DC2\ESC\n\
      \\tfull_name\CAN\STX \SOH(\tR\bfullName\DC2\SYN\n\
      \\ACKgroups\CAN\ETX \ETX(\tR\ACKgroups\DC2]\n\
      \\flast_updated\CAN\EOT \SOH(\v2:.event_store.client.users.DetailsResp.UserDetails.DateTimeR\vlastUpdated\DC2\SUB\n\
      \\bdisabled\CAN\ENQ \SOH(\bR\bdisabled\SUB6\n\
      \\bDateTime\DC2*\n\
      \\DC1ticks_since_epoch\CAN\SOH \SOH(\ETXR\SIticksSinceEpoch"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        loginName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "login_name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"loginName")) ::
              Data.ProtoLens.FieldDescriptor DetailsResp'UserDetails
        fullName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "full_name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"fullName")) ::
              Data.ProtoLens.FieldDescriptor DetailsResp'UserDetails
        groups__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "groups"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"groups")) ::
              Data.ProtoLens.FieldDescriptor DetailsResp'UserDetails
        lastUpdated__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "last_updated"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor DetailsResp'UserDetails'DateTime)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'lastUpdated")) ::
              Data.ProtoLens.FieldDescriptor DetailsResp'UserDetails
        disabled__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "disabled"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"disabled")) ::
              Data.ProtoLens.FieldDescriptor DetailsResp'UserDetails
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, loginName__field_descriptor),
           (Data.ProtoLens.Tag 2, fullName__field_descriptor),
           (Data.ProtoLens.Tag 3, groups__field_descriptor),
           (Data.ProtoLens.Tag 4, lastUpdated__field_descriptor),
           (Data.ProtoLens.Tag 5, disabled__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DetailsResp'UserDetails'_unknownFields
        (\ x__ y__ -> x__ {_DetailsResp'UserDetails'_unknownFields = y__})
  defMessage
    = DetailsResp'UserDetails'_constructor
        {_DetailsResp'UserDetails'loginName = Data.ProtoLens.fieldDefault,
         _DetailsResp'UserDetails'fullName = Data.ProtoLens.fieldDefault,
         _DetailsResp'UserDetails'groups = Data.Vector.Generic.empty,
         _DetailsResp'UserDetails'lastUpdated = Prelude.Nothing,
         _DetailsResp'UserDetails'disabled = Data.ProtoLens.fieldDefault,
         _DetailsResp'UserDetails'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DetailsResp'UserDetails
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Text.Text
             -> Data.ProtoLens.Encoding.Bytes.Parser DetailsResp'UserDetails
        loop x mutable'groups
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'groups <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                         (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                            mutable'groups)
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
                              (Data.ProtoLens.Field.field @"vec'groups") frozen'groups x))
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
                                       "login_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"loginName") y x)
                                  mutable'groups
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
                                       "full_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"fullName") y x)
                                  mutable'groups
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                        Data.ProtoLens.Encoding.Bytes.getBytes
                                                          (Prelude.fromIntegral len)
                                            Data.ProtoLens.Encoding.Bytes.runEither
                                              (case Data.Text.Encoding.decodeUtf8' value of
                                                 (Prelude.Left err)
                                                   -> Prelude.Left (Prelude.show err)
                                                 (Prelude.Right r) -> Prelude.Right r))
                                        "groups"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'groups y)
                                loop x v
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "last_updated"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"lastUpdated") y x)
                                  mutable'groups
                        40
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "disabled"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"disabled") y x)
                                  mutable'groups
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'groups
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'groups <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                  Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'groups)
          "UserDetails"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"loginName") _x
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
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"fullName") _x
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
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v
                         -> (Data.Monoid.<>)
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                              ((Prelude..)
                                 (\ bs
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                         (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Data.Text.Encoding.encodeUtf8
                                 _v))
                      (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'groups") _x))
                   ((Data.Monoid.<>)
                      (case
                           Lens.Family2.view
                             (Data.ProtoLens.Field.field @"maybe'lastUpdated") _x
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
                         (let
                            _v = Lens.Family2.view (Data.ProtoLens.Field.field @"disabled") _x
                          in
                            if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                Data.Monoid.mempty
                            else
                                (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 40)
                                  ((Prelude..)
                                     Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (\ b -> if b then 1 else 0)
                                     _v))
                         (Data.ProtoLens.Encoding.Wire.buildFieldSet
                            (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))
instance Control.DeepSeq.NFData DetailsResp'UserDetails where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DetailsResp'UserDetails'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_DetailsResp'UserDetails'loginName x__)
                (Control.DeepSeq.deepseq
                   (_DetailsResp'UserDetails'fullName x__)
                   (Control.DeepSeq.deepseq
                      (_DetailsResp'UserDetails'groups x__)
                      (Control.DeepSeq.deepseq
                         (_DetailsResp'UserDetails'lastUpdated x__)
                         (Control.DeepSeq.deepseq
                            (_DetailsResp'UserDetails'disabled x__) ())))))
{- | Fields :
     
         * 'Proto.Users_Fields.ticksSinceEpoch' @:: Lens' DetailsResp'UserDetails'DateTime Data.Int.Int64@ -}
data DetailsResp'UserDetails'DateTime
  = DetailsResp'UserDetails'DateTime'_constructor {_DetailsResp'UserDetails'DateTime'ticksSinceEpoch :: !Data.Int.Int64,
                                                   _DetailsResp'UserDetails'DateTime'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DetailsResp'UserDetails'DateTime where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField DetailsResp'UserDetails'DateTime "ticksSinceEpoch" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DetailsResp'UserDetails'DateTime'ticksSinceEpoch
           (\ x__ y__
              -> x__ {_DetailsResp'UserDetails'DateTime'ticksSinceEpoch = y__}))
        Prelude.id
instance Data.ProtoLens.Message DetailsResp'UserDetails'DateTime where
  messageName _
    = Data.Text.pack
        "event_store.client.users.DetailsResp.UserDetails.DateTime"
  packedMessageDescriptor _
    = "\n\
      \\bDateTime\DC2*\n\
      \\DC1ticks_since_epoch\CAN\SOH \SOH(\ETXR\SIticksSinceEpoch"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        ticksSinceEpoch__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "ticks_since_epoch"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"ticksSinceEpoch")) ::
              Data.ProtoLens.FieldDescriptor DetailsResp'UserDetails'DateTime
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, ticksSinceEpoch__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DetailsResp'UserDetails'DateTime'_unknownFields
        (\ x__ y__
           -> x__ {_DetailsResp'UserDetails'DateTime'_unknownFields = y__})
  defMessage
    = DetailsResp'UserDetails'DateTime'_constructor
        {_DetailsResp'UserDetails'DateTime'ticksSinceEpoch = Data.ProtoLens.fieldDefault,
         _DetailsResp'UserDetails'DateTime'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DetailsResp'UserDetails'DateTime
          -> Data.ProtoLens.Encoding.Bytes.Parser DetailsResp'UserDetails'DateTime
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
                                       "ticks_since_epoch"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"ticksSinceEpoch") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "DateTime"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view
                      (Data.ProtoLens.Field.field @"ticksSinceEpoch") _x
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
instance Control.DeepSeq.NFData DetailsResp'UserDetails'DateTime where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DetailsResp'UserDetails'DateTime'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_DetailsResp'UserDetails'DateTime'ticksSinceEpoch x__) ())
{- | Fields :
     
         * 'Proto.Users_Fields.options' @:: Lens' DisableReq DisableReq'Options@
         * 'Proto.Users_Fields.maybe'options' @:: Lens' DisableReq (Prelude.Maybe DisableReq'Options)@ -}
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
    = Data.Text.pack "event_store.client.users.DisableReq"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \DisableReq\DC2F\n\
      \\aoptions\CAN\SOH \SOH(\v2,.event_store.client.users.DisableReq.OptionsR\aoptions\SUB(\n\
      \\aOptions\DC2\GS\n\
      \\n\
      \login_name\CAN\SOH \SOH(\tR\tloginName"
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
     
         * 'Proto.Users_Fields.loginName' @:: Lens' DisableReq'Options Data.Text.Text@ -}
data DisableReq'Options
  = DisableReq'Options'_constructor {_DisableReq'Options'loginName :: !Data.Text.Text,
                                     _DisableReq'Options'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DisableReq'Options where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField DisableReq'Options "loginName" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DisableReq'Options'loginName
           (\ x__ y__ -> x__ {_DisableReq'Options'loginName = y__}))
        Prelude.id
instance Data.ProtoLens.Message DisableReq'Options where
  messageName _
    = Data.Text.pack "event_store.client.users.DisableReq.Options"
  packedMessageDescriptor _
    = "\n\
      \\aOptions\DC2\GS\n\
      \\n\
      \login_name\CAN\SOH \SOH(\tR\tloginName"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        loginName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "login_name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"loginName")) ::
              Data.ProtoLens.FieldDescriptor DisableReq'Options
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, loginName__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DisableReq'Options'_unknownFields
        (\ x__ y__ -> x__ {_DisableReq'Options'_unknownFields = y__})
  defMessage
    = DisableReq'Options'_constructor
        {_DisableReq'Options'loginName = Data.ProtoLens.fieldDefault,
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
                                       "login_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"loginName") y x)
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
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"loginName") _x
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
instance Control.DeepSeq.NFData DisableReq'Options where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DisableReq'Options'_unknownFields x__)
             (Control.DeepSeq.deepseq (_DisableReq'Options'loginName x__) ())
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
    = Data.Text.pack "event_store.client.users.DisableResp"
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
     
         * 'Proto.Users_Fields.options' @:: Lens' EnableReq EnableReq'Options@
         * 'Proto.Users_Fields.maybe'options' @:: Lens' EnableReq (Prelude.Maybe EnableReq'Options)@ -}
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
  messageName _ = Data.Text.pack "event_store.client.users.EnableReq"
  packedMessageDescriptor _
    = "\n\
      \\tEnableReq\DC2E\n\
      \\aoptions\CAN\SOH \SOH(\v2+.event_store.client.users.EnableReq.OptionsR\aoptions\SUB(\n\
      \\aOptions\DC2\GS\n\
      \\n\
      \login_name\CAN\SOH \SOH(\tR\tloginName"
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
     
         * 'Proto.Users_Fields.loginName' @:: Lens' EnableReq'Options Data.Text.Text@ -}
data EnableReq'Options
  = EnableReq'Options'_constructor {_EnableReq'Options'loginName :: !Data.Text.Text,
                                    _EnableReq'Options'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show EnableReq'Options where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField EnableReq'Options "loginName" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _EnableReq'Options'loginName
           (\ x__ y__ -> x__ {_EnableReq'Options'loginName = y__}))
        Prelude.id
instance Data.ProtoLens.Message EnableReq'Options where
  messageName _
    = Data.Text.pack "event_store.client.users.EnableReq.Options"
  packedMessageDescriptor _
    = "\n\
      \\aOptions\DC2\GS\n\
      \\n\
      \login_name\CAN\SOH \SOH(\tR\tloginName"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        loginName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "login_name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"loginName")) ::
              Data.ProtoLens.FieldDescriptor EnableReq'Options
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, loginName__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _EnableReq'Options'_unknownFields
        (\ x__ y__ -> x__ {_EnableReq'Options'_unknownFields = y__})
  defMessage
    = EnableReq'Options'_constructor
        {_EnableReq'Options'loginName = Data.ProtoLens.fieldDefault,
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
                                       "login_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"loginName") y x)
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
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"loginName") _x
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
             (Control.DeepSeq.deepseq (_EnableReq'Options'loginName x__) ())
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
    = Data.Text.pack "event_store.client.users.EnableResp"
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
     
         * 'Proto.Users_Fields.options' @:: Lens' ResetPasswordReq ResetPasswordReq'Options@
         * 'Proto.Users_Fields.maybe'options' @:: Lens' ResetPasswordReq (Prelude.Maybe ResetPasswordReq'Options)@ -}
data ResetPasswordReq
  = ResetPasswordReq'_constructor {_ResetPasswordReq'options :: !(Prelude.Maybe ResetPasswordReq'Options),
                                   _ResetPasswordReq'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ResetPasswordReq where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ResetPasswordReq "options" ResetPasswordReq'Options where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResetPasswordReq'options
           (\ x__ y__ -> x__ {_ResetPasswordReq'options = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ResetPasswordReq "maybe'options" (Prelude.Maybe ResetPasswordReq'Options) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResetPasswordReq'options
           (\ x__ y__ -> x__ {_ResetPasswordReq'options = y__}))
        Prelude.id
instance Data.ProtoLens.Message ResetPasswordReq where
  messageName _
    = Data.Text.pack "event_store.client.users.ResetPasswordReq"
  packedMessageDescriptor _
    = "\n\
      \\DLEResetPasswordReq\DC2L\n\
      \\aoptions\CAN\SOH \SOH(\v22.event_store.client.users.ResetPasswordReq.OptionsR\aoptions\SUBK\n\
      \\aOptions\DC2\GS\n\
      \\n\
      \login_name\CAN\SOH \SOH(\tR\tloginName\DC2!\n\
      \\fnew_password\CAN\STX \SOH(\tR\vnewPassword"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        options__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "options"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ResetPasswordReq'Options)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'options")) ::
              Data.ProtoLens.FieldDescriptor ResetPasswordReq
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, options__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ResetPasswordReq'_unknownFields
        (\ x__ y__ -> x__ {_ResetPasswordReq'_unknownFields = y__})
  defMessage
    = ResetPasswordReq'_constructor
        {_ResetPasswordReq'options = Prelude.Nothing,
         _ResetPasswordReq'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ResetPasswordReq
          -> Data.ProtoLens.Encoding.Bytes.Parser ResetPasswordReq
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
          (do loop Data.ProtoLens.defMessage) "ResetPasswordReq"
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
instance Control.DeepSeq.NFData ResetPasswordReq where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ResetPasswordReq'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ResetPasswordReq'options x__) ())
{- | Fields :
     
         * 'Proto.Users_Fields.loginName' @:: Lens' ResetPasswordReq'Options Data.Text.Text@
         * 'Proto.Users_Fields.newPassword' @:: Lens' ResetPasswordReq'Options Data.Text.Text@ -}
data ResetPasswordReq'Options
  = ResetPasswordReq'Options'_constructor {_ResetPasswordReq'Options'loginName :: !Data.Text.Text,
                                           _ResetPasswordReq'Options'newPassword :: !Data.Text.Text,
                                           _ResetPasswordReq'Options'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ResetPasswordReq'Options where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ResetPasswordReq'Options "loginName" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResetPasswordReq'Options'loginName
           (\ x__ y__ -> x__ {_ResetPasswordReq'Options'loginName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ResetPasswordReq'Options "newPassword" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResetPasswordReq'Options'newPassword
           (\ x__ y__ -> x__ {_ResetPasswordReq'Options'newPassword = y__}))
        Prelude.id
instance Data.ProtoLens.Message ResetPasswordReq'Options where
  messageName _
    = Data.Text.pack
        "event_store.client.users.ResetPasswordReq.Options"
  packedMessageDescriptor _
    = "\n\
      \\aOptions\DC2\GS\n\
      \\n\
      \login_name\CAN\SOH \SOH(\tR\tloginName\DC2!\n\
      \\fnew_password\CAN\STX \SOH(\tR\vnewPassword"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        loginName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "login_name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"loginName")) ::
              Data.ProtoLens.FieldDescriptor ResetPasswordReq'Options
        newPassword__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "new_password"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"newPassword")) ::
              Data.ProtoLens.FieldDescriptor ResetPasswordReq'Options
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, loginName__field_descriptor),
           (Data.ProtoLens.Tag 2, newPassword__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ResetPasswordReq'Options'_unknownFields
        (\ x__ y__ -> x__ {_ResetPasswordReq'Options'_unknownFields = y__})
  defMessage
    = ResetPasswordReq'Options'_constructor
        {_ResetPasswordReq'Options'loginName = Data.ProtoLens.fieldDefault,
         _ResetPasswordReq'Options'newPassword = Data.ProtoLens.fieldDefault,
         _ResetPasswordReq'Options'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ResetPasswordReq'Options
          -> Data.ProtoLens.Encoding.Bytes.Parser ResetPasswordReq'Options
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
                                       "login_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"loginName") y x)
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
                                       "new_password"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"newPassword") y x)
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
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"loginName") _x
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
                     = Lens.Family2.view (Data.ProtoLens.Field.field @"newPassword") _x
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
instance Control.DeepSeq.NFData ResetPasswordReq'Options where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ResetPasswordReq'Options'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ResetPasswordReq'Options'loginName x__)
                (Control.DeepSeq.deepseq
                   (_ResetPasswordReq'Options'newPassword x__) ()))
{- | Fields :
      -}
data ResetPasswordResp
  = ResetPasswordResp'_constructor {_ResetPasswordResp'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ResetPasswordResp where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message ResetPasswordResp where
  messageName _
    = Data.Text.pack "event_store.client.users.ResetPasswordResp"
  packedMessageDescriptor _
    = "\n\
      \\DC1ResetPasswordResp"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ResetPasswordResp'_unknownFields
        (\ x__ y__ -> x__ {_ResetPasswordResp'_unknownFields = y__})
  defMessage
    = ResetPasswordResp'_constructor
        {_ResetPasswordResp'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ResetPasswordResp
          -> Data.ProtoLens.Encoding.Bytes.Parser ResetPasswordResp
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
          (do loop Data.ProtoLens.defMessage) "ResetPasswordResp"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData ResetPasswordResp where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ResetPasswordResp'_unknownFields x__) ()
{- | Fields :
     
         * 'Proto.Users_Fields.options' @:: Lens' UpdateReq UpdateReq'Options@
         * 'Proto.Users_Fields.maybe'options' @:: Lens' UpdateReq (Prelude.Maybe UpdateReq'Options)@ -}
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
  messageName _ = Data.Text.pack "event_store.client.users.UpdateReq"
  packedMessageDescriptor _
    = "\n\
      \\tUpdateReq\DC2E\n\
      \\aoptions\CAN\SOH \SOH(\v2+.event_store.client.users.UpdateReq.OptionsR\aoptions\SUBy\n\
      \\aOptions\DC2\GS\n\
      \\n\
      \login_name\CAN\SOH \SOH(\tR\tloginName\DC2\SUB\n\
      \\bpassword\CAN\STX \SOH(\tR\bpassword\DC2\ESC\n\
      \\tfull_name\CAN\ETX \SOH(\tR\bfullName\DC2\SYN\n\
      \\ACKgroups\CAN\EOT \ETX(\tR\ACKgroups"
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
     
         * 'Proto.Users_Fields.loginName' @:: Lens' UpdateReq'Options Data.Text.Text@
         * 'Proto.Users_Fields.password' @:: Lens' UpdateReq'Options Data.Text.Text@
         * 'Proto.Users_Fields.fullName' @:: Lens' UpdateReq'Options Data.Text.Text@
         * 'Proto.Users_Fields.groups' @:: Lens' UpdateReq'Options [Data.Text.Text]@
         * 'Proto.Users_Fields.vec'groups' @:: Lens' UpdateReq'Options (Data.Vector.Vector Data.Text.Text)@ -}
data UpdateReq'Options
  = UpdateReq'Options'_constructor {_UpdateReq'Options'loginName :: !Data.Text.Text,
                                    _UpdateReq'Options'password :: !Data.Text.Text,
                                    _UpdateReq'Options'fullName :: !Data.Text.Text,
                                    _UpdateReq'Options'groups :: !(Data.Vector.Vector Data.Text.Text),
                                    _UpdateReq'Options'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show UpdateReq'Options where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField UpdateReq'Options "loginName" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UpdateReq'Options'loginName
           (\ x__ y__ -> x__ {_UpdateReq'Options'loginName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField UpdateReq'Options "password" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UpdateReq'Options'password
           (\ x__ y__ -> x__ {_UpdateReq'Options'password = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField UpdateReq'Options "fullName" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UpdateReq'Options'fullName
           (\ x__ y__ -> x__ {_UpdateReq'Options'fullName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField UpdateReq'Options "groups" [Data.Text.Text] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UpdateReq'Options'groups
           (\ x__ y__ -> x__ {_UpdateReq'Options'groups = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField UpdateReq'Options "vec'groups" (Data.Vector.Vector Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UpdateReq'Options'groups
           (\ x__ y__ -> x__ {_UpdateReq'Options'groups = y__}))
        Prelude.id
instance Data.ProtoLens.Message UpdateReq'Options where
  messageName _
    = Data.Text.pack "event_store.client.users.UpdateReq.Options"
  packedMessageDescriptor _
    = "\n\
      \\aOptions\DC2\GS\n\
      \\n\
      \login_name\CAN\SOH \SOH(\tR\tloginName\DC2\SUB\n\
      \\bpassword\CAN\STX \SOH(\tR\bpassword\DC2\ESC\n\
      \\tfull_name\CAN\ETX \SOH(\tR\bfullName\DC2\SYN\n\
      \\ACKgroups\CAN\EOT \ETX(\tR\ACKgroups"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        loginName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "login_name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"loginName")) ::
              Data.ProtoLens.FieldDescriptor UpdateReq'Options
        password__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "password"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"password")) ::
              Data.ProtoLens.FieldDescriptor UpdateReq'Options
        fullName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "full_name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"fullName")) ::
              Data.ProtoLens.FieldDescriptor UpdateReq'Options
        groups__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "groups"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"groups")) ::
              Data.ProtoLens.FieldDescriptor UpdateReq'Options
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, loginName__field_descriptor),
           (Data.ProtoLens.Tag 2, password__field_descriptor),
           (Data.ProtoLens.Tag 3, fullName__field_descriptor),
           (Data.ProtoLens.Tag 4, groups__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _UpdateReq'Options'_unknownFields
        (\ x__ y__ -> x__ {_UpdateReq'Options'_unknownFields = y__})
  defMessage
    = UpdateReq'Options'_constructor
        {_UpdateReq'Options'loginName = Data.ProtoLens.fieldDefault,
         _UpdateReq'Options'password = Data.ProtoLens.fieldDefault,
         _UpdateReq'Options'fullName = Data.ProtoLens.fieldDefault,
         _UpdateReq'Options'groups = Data.Vector.Generic.empty,
         _UpdateReq'Options'_unknownFields = []}
  parseMessage
    = let
        loop ::
          UpdateReq'Options
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Text.Text
             -> Data.ProtoLens.Encoding.Bytes.Parser UpdateReq'Options
        loop x mutable'groups
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'groups <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                         (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                            mutable'groups)
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
                              (Data.ProtoLens.Field.field @"vec'groups") frozen'groups x))
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
                                       "login_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"loginName") y x)
                                  mutable'groups
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
                                       "password"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"password") y x)
                                  mutable'groups
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "full_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"fullName") y x)
                                  mutable'groups
                        34
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                        Data.ProtoLens.Encoding.Bytes.getBytes
                                                          (Prelude.fromIntegral len)
                                            Data.ProtoLens.Encoding.Bytes.runEither
                                              (case Data.Text.Encoding.decodeUtf8' value of
                                                 (Prelude.Left err)
                                                   -> Prelude.Left (Prelude.show err)
                                                 (Prelude.Right r) -> Prelude.Right r))
                                        "groups"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'groups y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'groups
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'groups <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                  Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'groups)
          "Options"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"loginName") _x
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
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"password") _x
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
                   (let
                      _v = Lens.Family2.view (Data.ProtoLens.Field.field @"fullName") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                            ((Prelude..)
                               (\ bs
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                          (Prelude.fromIntegral (Data.ByteString.length bs)))
                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                               Data.Text.Encoding.encodeUtf8
                               _v))
                   ((Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                         (\ _v
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                 ((Prelude..)
                                    (\ bs
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                               (Prelude.fromIntegral (Data.ByteString.length bs)))
                                            (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                    Data.Text.Encoding.encodeUtf8
                                    _v))
                         (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'groups") _x))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData UpdateReq'Options where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_UpdateReq'Options'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_UpdateReq'Options'loginName x__)
                (Control.DeepSeq.deepseq
                   (_UpdateReq'Options'password x__)
                   (Control.DeepSeq.deepseq
                      (_UpdateReq'Options'fullName x__)
                      (Control.DeepSeq.deepseq (_UpdateReq'Options'groups x__) ()))))
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
    = Data.Text.pack "event_store.client.users.UpdateResp"
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
data Users = Users {}
instance Data.ProtoLens.Service.Types.Service Users where
  type ServiceName Users = "Users"
  type ServicePackage Users = "event_store.client.users"
  type ServiceMethods Users = '["changePassword",
                                "create",
                                "delete",
                                "details",
                                "disable",
                                "enable",
                                "resetPassword",
                                "update"]
instance Data.ProtoLens.Service.Types.HasMethodImpl Users "create" where
  type MethodName Users "create" = "Create"
  type MethodInput Users "create" = CreateReq
  type MethodOutput Users "create" = CreateResp
  type MethodStreamingType Users "create" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Users "update" where
  type MethodName Users "update" = "Update"
  type MethodInput Users "update" = UpdateReq
  type MethodOutput Users "update" = UpdateResp
  type MethodStreamingType Users "update" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Users "delete" where
  type MethodName Users "delete" = "Delete"
  type MethodInput Users "delete" = DeleteReq
  type MethodOutput Users "delete" = DeleteResp
  type MethodStreamingType Users "delete" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Users "disable" where
  type MethodName Users "disable" = "Disable"
  type MethodInput Users "disable" = DisableReq
  type MethodOutput Users "disable" = DisableResp
  type MethodStreamingType Users "disable" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Users "enable" where
  type MethodName Users "enable" = "Enable"
  type MethodInput Users "enable" = EnableReq
  type MethodOutput Users "enable" = EnableResp
  type MethodStreamingType Users "enable" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Users "details" where
  type MethodName Users "details" = "Details"
  type MethodInput Users "details" = DetailsReq
  type MethodOutput Users "details" = DetailsResp
  type MethodStreamingType Users "details" = 'Data.ProtoLens.Service.Types.ServerStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Users "changePassword" where
  type MethodName Users "changePassword" = "ChangePassword"
  type MethodInput Users "changePassword" = ChangePasswordReq
  type MethodOutput Users "changePassword" = ChangePasswordResp
  type MethodStreamingType Users "changePassword" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Users "resetPassword" where
  type MethodName Users "resetPassword" = "ResetPassword"
  type MethodInput Users "resetPassword" = ResetPasswordReq
  type MethodOutput Users "resetPassword" = ResetPasswordResp
  type MethodStreamingType Users "resetPassword" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\vusers.proto\DC2\CANevent_store.client.users\"\205\SOH\n\
    \\tCreateReq\DC2E\n\
    \\aoptions\CAN\SOH \SOH(\v2+.event_store.client.users.CreateReq.OptionsR\aoptions\SUBy\n\
    \\aOptions\DC2\GS\n\
    \\n\
    \login_name\CAN\SOH \SOH(\tR\tloginName\DC2\SUB\n\
    \\bpassword\CAN\STX \SOH(\tR\bpassword\DC2\ESC\n\
    \\tfull_name\CAN\ETX \SOH(\tR\bfullName\DC2\SYN\n\
    \\ACKgroups\CAN\EOT \ETX(\tR\ACKgroups\"\f\n\
    \\n\
    \CreateResp\"\205\SOH\n\
    \\tUpdateReq\DC2E\n\
    \\aoptions\CAN\SOH \SOH(\v2+.event_store.client.users.UpdateReq.OptionsR\aoptions\SUBy\n\
    \\aOptions\DC2\GS\n\
    \\n\
    \login_name\CAN\SOH \SOH(\tR\tloginName\DC2\SUB\n\
    \\bpassword\CAN\STX \SOH(\tR\bpassword\DC2\ESC\n\
    \\tfull_name\CAN\ETX \SOH(\tR\bfullName\DC2\SYN\n\
    \\ACKgroups\CAN\EOT \ETX(\tR\ACKgroups\"\f\n\
    \\n\
    \UpdateResp\"|\n\
    \\tDeleteReq\DC2E\n\
    \\aoptions\CAN\SOH \SOH(\v2+.event_store.client.users.DeleteReq.OptionsR\aoptions\SUB(\n\
    \\aOptions\DC2\GS\n\
    \\n\
    \login_name\CAN\SOH \SOH(\tR\tloginName\"\f\n\
    \\n\
    \DeleteResp\"|\n\
    \\tEnableReq\DC2E\n\
    \\aoptions\CAN\SOH \SOH(\v2+.event_store.client.users.EnableReq.OptionsR\aoptions\SUB(\n\
    \\aOptions\DC2\GS\n\
    \\n\
    \login_name\CAN\SOH \SOH(\tR\tloginName\"\f\n\
    \\n\
    \EnableResp\"~\n\
    \\n\
    \DisableReq\DC2F\n\
    \\aoptions\CAN\SOH \SOH(\v2,.event_store.client.users.DisableReq.OptionsR\aoptions\SUB(\n\
    \\aOptions\DC2\GS\n\
    \\n\
    \login_name\CAN\SOH \SOH(\tR\tloginName\"\r\n\
    \\vDisableResp\"~\n\
    \\n\
    \DetailsReq\DC2F\n\
    \\aoptions\CAN\SOH \SOH(\v2,.event_store.client.users.DetailsReq.OptionsR\aoptions\SUB(\n\
    \\aOptions\DC2\GS\n\
    \\n\
    \login_name\CAN\SOH \SOH(\tR\tloginName\"\250\STX\n\
    \\vDetailsResp\DC2T\n\
    \\fuser_details\CAN\SOH \SOH(\v21.event_store.client.users.DetailsResp.UserDetailsR\vuserDetails\SUB\148\STX\n\
    \\vUserDetails\DC2\GS\n\
    \\n\
    \login_name\CAN\SOH \SOH(\tR\tloginName\DC2\ESC\n\
    \\tfull_name\CAN\STX \SOH(\tR\bfullName\DC2\SYN\n\
    \\ACKgroups\CAN\ETX \ETX(\tR\ACKgroups\DC2]\n\
    \\flast_updated\CAN\EOT \SOH(\v2:.event_store.client.users.DetailsResp.UserDetails.DateTimeR\vlastUpdated\DC2\SUB\n\
    \\bdisabled\CAN\ENQ \SOH(\bR\bdisabled\SUB6\n\
    \\bDateTime\DC2*\n\
    \\DC1ticks_since_epoch\CAN\SOH \SOH(\ETXR\SIticksSinceEpoch\"\218\SOH\n\
    \\DC1ChangePasswordReq\DC2M\n\
    \\aoptions\CAN\SOH \SOH(\v23.event_store.client.users.ChangePasswordReq.OptionsR\aoptions\SUBv\n\
    \\aOptions\DC2\GS\n\
    \\n\
    \login_name\CAN\SOH \SOH(\tR\tloginName\DC2)\n\
    \\DLEcurrent_password\CAN\STX \SOH(\tR\SIcurrentPassword\DC2!\n\
    \\fnew_password\CAN\ETX \SOH(\tR\vnewPassword\"\DC4\n\
    \\DC2ChangePasswordResp\"\173\SOH\n\
    \\DLEResetPasswordReq\DC2L\n\
    \\aoptions\CAN\SOH \SOH(\v22.event_store.client.users.ResetPasswordReq.OptionsR\aoptions\SUBK\n\
    \\aOptions\DC2\GS\n\
    \\n\
    \login_name\CAN\SOH \SOH(\tR\tloginName\DC2!\n\
    \\fnew_password\CAN\STX \SOH(\tR\vnewPassword\"\DC3\n\
    \\DC1ResetPasswordResp2\228\ENQ\n\
    \\ENQUsers\DC2S\n\
    \\ACKCreate\DC2#.event_store.client.users.CreateReq\SUB$.event_store.client.users.CreateResp\DC2S\n\
    \\ACKUpdate\DC2#.event_store.client.users.UpdateReq\SUB$.event_store.client.users.UpdateResp\DC2S\n\
    \\ACKDelete\DC2#.event_store.client.users.DeleteReq\SUB$.event_store.client.users.DeleteResp\DC2V\n\
    \\aDisable\DC2$.event_store.client.users.DisableReq\SUB%.event_store.client.users.DisableResp\DC2S\n\
    \\ACKEnable\DC2#.event_store.client.users.EnableReq\SUB$.event_store.client.users.EnableResp\DC2X\n\
    \\aDetails\DC2$.event_store.client.users.DetailsReq\SUB%.event_store.client.users.DetailsResp0\SOH\DC2k\n\
    \\SOChangePassword\DC2+.event_store.client.users.ChangePasswordReq\SUB,.event_store.client.users.ChangePasswordResp\DC2h\n\
    \\rResetPassword\DC2*.event_store.client.users.ResetPasswordReq\SUB+.event_store.client.users.ResetPasswordRespB%\n\
    \#com.eventstore.dbclient.proto.usersJ\251\CAN\n\
    \\ACK\DC2\EOT\NUL\NULv\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\SOH\NUL!\n\
    \\b\n\
    \\SOH\b\DC2\ETX\STX\NUL<\n\
    \\t\n\
    \\STX\b\SOH\DC2\ETX\STX\NUL<\n\
    \\n\
    \\n\
    \\STX\ACK\NUL\DC2\EOT\EOT\NUL\r\SOH\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX\EOT\b\r\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETX\ENQ\b4\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX\ENQ\f\DC2\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX\ENQ\DC4\GS\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX\ENQ(2\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\SOH\DC2\ETX\ACK\b4\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\SOH\DC2\ETX\ACK\f\DC2\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\STX\DC2\ETX\ACK\DC4\GS\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\ETX\DC2\ETX\ACK(2\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\STX\DC2\ETX\a\b4\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\SOH\DC2\ETX\a\f\DC2\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\STX\DC2\ETX\a\DC4\GS\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\ETX\DC2\ETX\a(2\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\ETX\DC2\ETX\b\b7\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\SOH\DC2\ETX\b\f\DC3\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\STX\DC2\ETX\b\NAK\US\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\ETX\DC2\ETX\b*5\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\EOT\DC2\ETX\t\b4\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\EOT\SOH\DC2\ETX\t\f\DC2\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\EOT\STX\DC2\ETX\t\DC4\GS\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\EOT\ETX\DC2\ETX\t(2\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\ENQ\DC2\ETX\n\
    \\b>\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ENQ\SOH\DC2\ETX\n\
    \\f\DC3\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ENQ\STX\DC2\ETX\n\
    \\NAK\US\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ENQ\ACK\DC2\ETX\n\
    \*0\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ENQ\ETX\DC2\ETX\n\
    \1<\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\ACK\DC2\ETX\v\bL\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ACK\SOH\DC2\ETX\v\f\SUB\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ACK\STX\DC2\ETX\v\FS-\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ACK\ETX\DC2\ETX\v8J\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\a\DC2\ETX\f\bI\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\a\SOH\DC2\ETX\f\f\EM\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\a\STX\DC2\ETX\f\ESC+\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\a\ETX\DC2\ETX\f6G\n\
    \\n\
    \\n\
    \\STX\EOT\NUL\DC2\EOT\SI\NUL\ETB\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\SI\b\DC1\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\DLE\b\FS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ACK\DC2\ETX\DLE\b\SI\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\DLE\DLE\ETB\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\DLE\SUB\ESC\n\
    \\f\n\
    \\EOT\EOT\NUL\ETX\NUL\DC2\EOT\DC1\b\SYN\t\n\
    \\f\n\
    \\ENQ\EOT\NUL\ETX\NUL\SOH\DC2\ETX\DC1\DLE\ETB\n\
    \\r\n\
    \\ACK\EOT\NUL\ETX\NUL\STX\NUL\DC2\ETX\DC2\DLE&\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\NUL\ENQ\DC2\ETX\DC2\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\NUL\SOH\DC2\ETX\DC2\ETB!\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\NUL\ETX\DC2\ETX\DC2$%\n\
    \\r\n\
    \\ACK\EOT\NUL\ETX\NUL\STX\SOH\DC2\ETX\DC3\DLE$\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\SOH\ENQ\DC2\ETX\DC3\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\SOH\SOH\DC2\ETX\DC3\ETB\US\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\SOH\ETX\DC2\ETX\DC3\"#\n\
    \\r\n\
    \\ACK\EOT\NUL\ETX\NUL\STX\STX\DC2\ETX\DC4\DLE%\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\STX\ENQ\DC2\ETX\DC4\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\STX\SOH\DC2\ETX\DC4\ETB \n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\STX\ETX\DC2\ETX\DC4#$\n\
    \\r\n\
    \\ACK\EOT\NUL\ETX\NUL\STX\ETX\DC2\ETX\NAK\DLE+\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\ETX\EOT\DC2\ETX\NAK\DLE\CAN\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\ETX\ENQ\DC2\ETX\NAK\EM\US\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\ETX\SOH\DC2\ETX\NAK &\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\ETX\ETX\DC2\ETX\NAK)*\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT\EM\NUL\ESC\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\EM\b\DC2\n\
    \\n\
    \\n\
    \\STX\EOT\STX\DC2\EOT\GS\NUL%\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX\GS\b\DC1\n\
    \\v\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX\RS\b\FS\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ACK\DC2\ETX\RS\b\SI\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX\RS\DLE\ETB\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX\RS\SUB\ESC\n\
    \\f\n\
    \\EOT\EOT\STX\ETX\NUL\DC2\EOT\US\b$\t\n\
    \\f\n\
    \\ENQ\EOT\STX\ETX\NUL\SOH\DC2\ETX\US\DLE\ETB\n\
    \\r\n\
    \\ACK\EOT\STX\ETX\NUL\STX\NUL\DC2\ETX \DLE&\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\NUL\ENQ\DC2\ETX \DLE\SYN\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\NUL\SOH\DC2\ETX \ETB!\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\NUL\ETX\DC2\ETX $%\n\
    \\r\n\
    \\ACK\EOT\STX\ETX\NUL\STX\SOH\DC2\ETX!\DLE$\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\SOH\ENQ\DC2\ETX!\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\SOH\SOH\DC2\ETX!\ETB\US\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\SOH\ETX\DC2\ETX!\"#\n\
    \\r\n\
    \\ACK\EOT\STX\ETX\NUL\STX\STX\DC2\ETX\"\DLE%\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\STX\ENQ\DC2\ETX\"\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\STX\SOH\DC2\ETX\"\ETB \n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\STX\ETX\DC2\ETX\"#$\n\
    \\r\n\
    \\ACK\EOT\STX\ETX\NUL\STX\ETX\DC2\ETX#\DLE+\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\ETX\EOT\DC2\ETX#\DLE\CAN\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\ETX\ENQ\DC2\ETX#\EM\US\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\ETX\SOH\DC2\ETX# &\n\
    \\SO\n\
    \\a\EOT\STX\ETX\NUL\STX\ETX\ETX\DC2\ETX#)*\n\
    \\n\
    \\n\
    \\STX\EOT\ETX\DC2\EOT'\NUL)\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETX'\b\DC2\n\
    \\n\
    \\n\
    \\STX\EOT\EOT\DC2\EOT+\NUL0\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\EOT\SOH\DC2\ETX+\b\DC1\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\NUL\DC2\ETX,\b\FS\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ACK\DC2\ETX,\b\SI\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\SOH\DC2\ETX,\DLE\ETB\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ETX\DC2\ETX,\SUB\ESC\n\
    \\f\n\
    \\EOT\EOT\EOT\ETX\NUL\DC2\EOT-\b/\t\n\
    \\f\n\
    \\ENQ\EOT\EOT\ETX\NUL\SOH\DC2\ETX-\DLE\ETB\n\
    \\r\n\
    \\ACK\EOT\EOT\ETX\NUL\STX\NUL\DC2\ETX.\DLE&\n\
    \\SO\n\
    \\a\EOT\EOT\ETX\NUL\STX\NUL\ENQ\DC2\ETX.\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\EOT\ETX\NUL\STX\NUL\SOH\DC2\ETX.\ETB!\n\
    \\SO\n\
    \\a\EOT\EOT\ETX\NUL\STX\NUL\ETX\DC2\ETX.$%\n\
    \\n\
    \\n\
    \\STX\EOT\ENQ\DC2\EOT2\NUL4\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ENQ\SOH\DC2\ETX2\b\DC2\n\
    \\n\
    \\n\
    \\STX\EOT\ACK\DC2\EOT6\NUL;\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ACK\SOH\DC2\ETX6\b\DC1\n\
    \\v\n\
    \\EOT\EOT\ACK\STX\NUL\DC2\ETX7\b\FS\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ACK\DC2\ETX7\b\SI\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\SOH\DC2\ETX7\DLE\ETB\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ETX\DC2\ETX7\SUB\ESC\n\
    \\f\n\
    \\EOT\EOT\ACK\ETX\NUL\DC2\EOT8\b:\t\n\
    \\f\n\
    \\ENQ\EOT\ACK\ETX\NUL\SOH\DC2\ETX8\DLE\ETB\n\
    \\r\n\
    \\ACK\EOT\ACK\ETX\NUL\STX\NUL\DC2\ETX9\DLE&\n\
    \\SO\n\
    \\a\EOT\ACK\ETX\NUL\STX\NUL\ENQ\DC2\ETX9\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\ACK\ETX\NUL\STX\NUL\SOH\DC2\ETX9\ETB!\n\
    \\SO\n\
    \\a\EOT\ACK\ETX\NUL\STX\NUL\ETX\DC2\ETX9$%\n\
    \\n\
    \\n\
    \\STX\EOT\a\DC2\EOT=\NUL?\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\a\SOH\DC2\ETX=\b\DC2\n\
    \\n\
    \\n\
    \\STX\EOT\b\DC2\EOTA\NULF\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\b\SOH\DC2\ETXA\b\DC2\n\
    \\v\n\
    \\EOT\EOT\b\STX\NUL\DC2\ETXB\b\FS\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\ACK\DC2\ETXB\b\SI\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\SOH\DC2\ETXB\DLE\ETB\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\ETX\DC2\ETXB\SUB\ESC\n\
    \\f\n\
    \\EOT\EOT\b\ETX\NUL\DC2\EOTC\bE\t\n\
    \\f\n\
    \\ENQ\EOT\b\ETX\NUL\SOH\DC2\ETXC\DLE\ETB\n\
    \\r\n\
    \\ACK\EOT\b\ETX\NUL\STX\NUL\DC2\ETXD\DLE&\n\
    \\SO\n\
    \\a\EOT\b\ETX\NUL\STX\NUL\ENQ\DC2\ETXD\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\b\ETX\NUL\STX\NUL\SOH\DC2\ETXD\ETB!\n\
    \\SO\n\
    \\a\EOT\b\ETX\NUL\STX\NUL\ETX\DC2\ETXD$%\n\
    \\n\
    \\n\
    \\STX\EOT\t\DC2\EOTH\NULI\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\t\SOH\DC2\ETXH\b\DC3\n\
    \\n\
    \\n\
    \\STX\EOT\n\
    \\DC2\EOTK\NULP\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\n\
    \\SOH\DC2\ETXK\b\DC2\n\
    \\v\n\
    \\EOT\EOT\n\
    \\STX\NUL\DC2\ETXL\b\FS\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ACK\DC2\ETXL\b\SI\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\SOH\DC2\ETXL\DLE\ETB\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ETX\DC2\ETXL\SUB\ESC\n\
    \\f\n\
    \\EOT\EOT\n\
    \\ETX\NUL\DC2\EOTM\bO\t\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\ETX\NUL\SOH\DC2\ETXM\DLE\ETB\n\
    \\r\n\
    \\ACK\EOT\n\
    \\ETX\NUL\STX\NUL\DC2\ETXN\DLE&\n\
    \\SO\n\
    \\a\EOT\n\
    \\ETX\NUL\STX\NUL\ENQ\DC2\ETXN\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\n\
    \\ETX\NUL\STX\NUL\SOH\DC2\ETXN\ETB!\n\
    \\SO\n\
    \\a\EOT\n\
    \\ETX\NUL\STX\NUL\ETX\DC2\ETXN$%\n\
    \\n\
    \\n\
    \\STX\EOT\v\DC2\EOTR\NUL_\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\v\SOH\DC2\ETXR\b\DC3\n\
    \\v\n\
    \\EOT\EOT\v\STX\NUL\DC2\ETXS\b%\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\ACK\DC2\ETXS\b\DC3\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\SOH\DC2\ETXS\DC4 \n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\ETX\DC2\ETXS#$\n\
    \\f\n\
    \\EOT\EOT\v\ETX\NUL\DC2\EOTT\b^\t\n\
    \\f\n\
    \\ENQ\EOT\v\ETX\NUL\SOH\DC2\ETXT\DLE\ESC\n\
    \\r\n\
    \\ACK\EOT\v\ETX\NUL\STX\NUL\DC2\ETXU\DLE&\n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\STX\NUL\ENQ\DC2\ETXU\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\STX\NUL\SOH\DC2\ETXU\ETB!\n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\STX\NUL\ETX\DC2\ETXU$%\n\
    \\r\n\
    \\ACK\EOT\v\ETX\NUL\STX\SOH\DC2\ETXV\DLE%\n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\STX\SOH\ENQ\DC2\ETXV\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\STX\SOH\SOH\DC2\ETXV\ETB \n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\STX\SOH\ETX\DC2\ETXV#$\n\
    \\r\n\
    \\ACK\EOT\v\ETX\NUL\STX\STX\DC2\ETXW\DLE+\n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\STX\STX\EOT\DC2\ETXW\DLE\CAN\n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\STX\STX\ENQ\DC2\ETXW\EM\US\n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\STX\STX\SOH\DC2\ETXW &\n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\STX\STX\ETX\DC2\ETXW)*\n\
    \\r\n\
    \\ACK\EOT\v\ETX\NUL\STX\ETX\DC2\ETXX\DLE*\n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\STX\ETX\ACK\DC2\ETXX\DLE\CAN\n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\STX\ETX\SOH\DC2\ETXX\EM%\n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\STX\ETX\ETX\DC2\ETXX()\n\
    \\r\n\
    \\ACK\EOT\v\ETX\NUL\STX\EOT\DC2\ETXY\DLE\"\n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\STX\EOT\ENQ\DC2\ETXY\DLE\DC4\n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\STX\EOT\SOH\DC2\ETXY\NAK\GS\n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\STX\EOT\ETX\DC2\ETXY !\n\
    \\SO\n\
    \\ACK\EOT\v\ETX\NUL\ETX\NUL\DC2\EOT[\DLE]\DC1\n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\ETX\NUL\SOH\DC2\ETX[\CAN \n\
    \\SI\n\
    \\b\EOT\v\ETX\NUL\ETX\NUL\STX\NUL\DC2\ETX\\\CAN4\n\
    \\DLE\n\
    \\t\EOT\v\ETX\NUL\ETX\NUL\STX\NUL\ENQ\DC2\ETX\\\CAN\GS\n\
    \\DLE\n\
    \\t\EOT\v\ETX\NUL\ETX\NUL\STX\NUL\SOH\DC2\ETX\\\RS/\n\
    \\DLE\n\
    \\t\EOT\v\ETX\NUL\ETX\NUL\STX\NUL\ETX\DC2\ETX\\23\n\
    \\n\
    \\n\
    \\STX\EOT\f\DC2\EOTa\NULh\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\f\SOH\DC2\ETXa\b\EM\n\
    \\v\n\
    \\EOT\EOT\f\STX\NUL\DC2\ETXb\b\FS\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\ACK\DC2\ETXb\b\SI\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\SOH\DC2\ETXb\DLE\ETB\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\ETX\DC2\ETXb\SUB\ESC\n\
    \\f\n\
    \\EOT\EOT\f\ETX\NUL\DC2\EOTc\bg\t\n\
    \\f\n\
    \\ENQ\EOT\f\ETX\NUL\SOH\DC2\ETXc\DLE\ETB\n\
    \\r\n\
    \\ACK\EOT\f\ETX\NUL\STX\NUL\DC2\ETXd\DLE&\n\
    \\SO\n\
    \\a\EOT\f\ETX\NUL\STX\NUL\ENQ\DC2\ETXd\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\f\ETX\NUL\STX\NUL\SOH\DC2\ETXd\ETB!\n\
    \\SO\n\
    \\a\EOT\f\ETX\NUL\STX\NUL\ETX\DC2\ETXd$%\n\
    \\r\n\
    \\ACK\EOT\f\ETX\NUL\STX\SOH\DC2\ETXe\DLE,\n\
    \\SO\n\
    \\a\EOT\f\ETX\NUL\STX\SOH\ENQ\DC2\ETXe\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\f\ETX\NUL\STX\SOH\SOH\DC2\ETXe\ETB'\n\
    \\SO\n\
    \\a\EOT\f\ETX\NUL\STX\SOH\ETX\DC2\ETXe*+\n\
    \\r\n\
    \\ACK\EOT\f\ETX\NUL\STX\STX\DC2\ETXf\DLE(\n\
    \\SO\n\
    \\a\EOT\f\ETX\NUL\STX\STX\ENQ\DC2\ETXf\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\f\ETX\NUL\STX\STX\SOH\DC2\ETXf\ETB#\n\
    \\SO\n\
    \\a\EOT\f\ETX\NUL\STX\STX\ETX\DC2\ETXf&'\n\
    \\n\
    \\n\
    \\STX\EOT\r\DC2\EOTj\NULk\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\r\SOH\DC2\ETXj\b\SUB\n\
    \\n\
    \\n\
    \\STX\EOT\SO\DC2\EOTm\NULs\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SO\SOH\DC2\ETXm\b\CAN\n\
    \\v\n\
    \\EOT\EOT\SO\STX\NUL\DC2\ETXn\b\FS\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\NUL\ACK\DC2\ETXn\b\SI\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\NUL\SOH\DC2\ETXn\DLE\ETB\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\NUL\ETX\DC2\ETXn\SUB\ESC\n\
    \\f\n\
    \\EOT\EOT\SO\ETX\NUL\DC2\EOTo\br\t\n\
    \\f\n\
    \\ENQ\EOT\SO\ETX\NUL\SOH\DC2\ETXo\DLE\ETB\n\
    \\r\n\
    \\ACK\EOT\SO\ETX\NUL\STX\NUL\DC2\ETXp\DLE&\n\
    \\SO\n\
    \\a\EOT\SO\ETX\NUL\STX\NUL\ENQ\DC2\ETXp\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\SO\ETX\NUL\STX\NUL\SOH\DC2\ETXp\ETB!\n\
    \\SO\n\
    \\a\EOT\SO\ETX\NUL\STX\NUL\ETX\DC2\ETXp$%\n\
    \\r\n\
    \\ACK\EOT\SO\ETX\NUL\STX\SOH\DC2\ETXq\DLE(\n\
    \\SO\n\
    \\a\EOT\SO\ETX\NUL\STX\SOH\ENQ\DC2\ETXq\DLE\SYN\n\
    \\SO\n\
    \\a\EOT\SO\ETX\NUL\STX\SOH\SOH\DC2\ETXq\ETB#\n\
    \\SO\n\
    \\a\EOT\SO\ETX\NUL\STX\SOH\ETX\DC2\ETXq&'\n\
    \\n\
    \\n\
    \\STX\EOT\SI\DC2\EOTu\NULv\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SI\SOH\DC2\ETXu\b\EMb\ACKproto3"